{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EvaluationEngine where

import ContractsDSL
import ModelUtils

import Data.Time (addDays)
import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map

------------------------- Type-checker -------------------------

-- Now we only return Either Error (), so we never rebuild the contract.
typeCheck :: Contract -> Date -> Either Error ()
typeCheck None d
  | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
  | otherwise = Right ()
typeCheck (One cur) d
  | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
  | otherwise = Right ()
typeCheck (Give c) d = do
  typeCheck c d
typeCheck (And c1 c2) d = do
  typeCheck c1 d
  typeCheck c2 d
typeCheck (Or c1 c2) d = do
  typeCheck c1 d
  typeCheck c2 d
typeCheck (Scale obs c) d =
  typeCheck c d
typeCheck (AcquireOn d2 c) d1 = Right ()
typeCheck (AcquireWhen obs c) d1 = Right ()
typeCheck (AcquireOnBefore d2 c) d1 = Right ()

------------------------- Optimisation layer -------------------------

optimiseContract :: Contract -> Contract
-- Double negation: give (give c) = c
optimiseContract (Give (Give c)) = optimiseContract c
optimiseContract (Give c)
    | c == None = optimiseContract c
    | otherwise = Give (optimiseContract c)

-- Avoiding double scale computation
optimiseContract (Or (Scale obs1 c1) (Scale obs2 c2))  
    | Konst k <- obs1, Konst j <- obs2, k == j, k >= 0 = Scale obs1 (optimiseContract (Or c1 c2))
    | otherwise = Or (Scale obs1 (optimiseContract c1)) (Scale obs2 (optimiseContract c2))
optimiseContract (Or c1 c2) 
    | c1 == c2 = c1
    | otherwise = Or (optimiseContract c1) (optimiseContract c2)

optimiseContract (And c1 c2)  
    | c1 == None = optimiseContract c2  -- Remove unnecessary None
    | c2 == None = optimiseContract c1
    | otherwise  = And (optimiseContract c1) (optimiseContract c2)

optimiseContract (Scale obs c) = Scale obs (optimiseContract c)

-- Nested acquire on:
optimiseContract (AcquireOn d1 (AcquireOn d2 c))  
    | d1 <= d2   = AcquireOn d1 (optimiseContract c)
    | otherwise = AcquireOn d1 (optimiseContract (AcquireOn d2 c))
optimiseContract (AcquireOn d c) = AcquireOn d (optimiseContract c)

optimiseContract (AcquireOnBefore d c) = AcquireOnBefore d (optimiseContract c)

optimiseContract (AcquireWhen obs c) = AcquireWhen obs (optimiseContract c)

-- Base cases: None, One, etc., are already in simplest form
optimiseContract contract = contract

----------------------------- Evaluation -----------------------------

---------------------------------------
-- Cache
---------------------------------------
data Key
  = KContract Contract
  | KObsDouble (Obs Double)
  | KObsBool   (Obs Bool)
  deriving (Eq, Ord, Show)

data Val
  = VDouble (PR Double)
  | VBool   (PR Bool)
  deriving (Show)

type Cache = Map.Map Key Val

-- We'll keep an EvalState with that single cache
data EvalState = EvalState
  { cache :: Cache
  }

-- The monad: we carry our cache (EvalState) in StateT,
-- and can fail with String errors using Either String
type EvalM a = StateT EvalState (Either String) a

emptyEvalState :: EvalState
emptyEvalState = EvalState { cache = Map.empty }

---------------------------------------
-- Top-level evaluation
---------------------------------------
eval :: Model -> Contract -> Either String (PR Double)
eval model@(Model startDate _ _ _ _ _ _ _ _) c = do
  -- If typeCheck fails, we short-circuit with a Left.
  _ <- typeCheck c infiniteHorizon
  let optC = optimiseContract c
  -- Run our StateT-based evaluator with an empty cache:
  (result, _finalState) <- runStateT (evalC model optC startDate) emptyEvalState
  return result

----------------------------------------------------------------------------
-- Memoized evaluation of a contract
----------------------------------------------------------------------------

evalC :: Model -> Contract -> Date -> EvalM (PR Double)
evalC model@(Model startDate stepSize constPr discDate discObs snell exchange _ _) contract earliestAcDate = do
  st <- get
  case Map.lookup (KContract contract) (cache st) of
    Just (VDouble cachedPR) -> return cachedPR
    Just (VBool _)          -> throwError "Contract was stored with VBool, logic bug!"
    Nothing -> do
      -- Not in cache
      result <- eval' contract earliestAcDate
      -- Insert the result
      let newMap = Map.insert (KContract contract) (VDouble result) (cache st)
      put st { cache = newMap }
      return result
  where
    ----------------------------------------------------------------------------
    -- The local `eval` function does the real recursive work
    ----------------------------------------------------------------------------
    eval' :: Contract -> Date -> EvalM (PR Double)
    eval' None _ =
      return (constPr 0)

    eval' (One cur) _ = do
      pr <- liftEither (exchange cur)
      return pr

    eval' (Give c) d = do
      cVal <- evalC model c d
      return (negate cVal)

    eval' (And c1 c2) d = do
      pr1 <- evalC model c1 d
      pr2 <- evalC model c2 d
      return (pr1 + pr2)

    eval' (Or c1 c2) d = do
        (PR pr1) <- evalC model c1 d
        (PR pr2) <- evalC model c2 d

        let startTimeStep = ((daysBetween startDate d) `div` stepSize) 
        let pr1Len = length $ take startTimeStep pr1 
        let pr2Len = length $ take startTimeStep pr2 

        if pr1Len == startTimeStep && pr2Len == startTimeStep then return (maximumValToday (PR pr1) (PR pr2))
        else if pr1Len == startTimeStep then return (PR pr1)
        else if pr2Len == startTimeStep then return (PR pr2)
        else return (constPr 0)

    eval' (AcquireOn d2 c) d1 = do
      cVal <- evalC model c d2
      pr <- liftEither (discDate d2 cVal)
      return pr

    eval' (AcquireOnBefore d2 c) _ = do
      cVal <- evalC model c startDate
      pr <- liftEither (snell d2 cVal)
      return pr

    eval' (AcquireWhen obs c) d = do
      obsPR <- evalBO model obs
      cVal <- evalC model c d
      pr <- liftEither (discObs obsPR cVal)
      return pr

    eval' (Scale obs c) d = do
      obsPR <- evalDO model obs
      cVal   <- evalC model c d
      return (obsPR * cVal)

----------------------------------------------------------------------------
-- Memoized evaluation of an observable
----------------------------------------------------------------------------

evalDO :: Model -> Obs Double -> EvalM (PR Double)
evalDO model@(Model _ _ constPr _ _ _ _ stockModel _) obsD = do
  st <- get
  case Map.lookup (KObsDouble obsD) (cache st) of
    Just (VDouble prD) -> return prD
    Just (VBool _)     -> throwError "Obs Double is stored as VBool, logic error!"
    Nothing -> do
      pr <- eval' obsD
      let newMap = Map.insert (KObsDouble obsD) (VDouble pr) (cache st)
      put st { cache = newMap }
      return pr
  where
    eval' :: Obs Double -> EvalM (PR Double)
    eval' (Konst k) = return (constPr (realToFrac k))
    eval' (StockPrice stk) = do 
      pr <- liftEither (stockModel stk)
      return pr
    eval' (LiftD op o) = do
      po <- evalDO model o
      return (ModelUtils.lift (unaryOpMap op) po)
    eval' (Lift2D op o1 o2) = do
      po1 <- evalDO model o1
      po2 <- evalDO model o2
      return (ModelUtils.lift2 (binaryOpMap op) po1 po2)  


evalBO :: Model -> Obs Bool -> EvalM (PR Bool)
evalBO model@(Model _ _ _ _ _ _ _ _ datePr) obsB = do
  st <- get
  case Map.lookup (KObsBool obsB) (cache st) of
    Just (VBool prB) -> return prB
    Just (VDouble _) -> throwError "Obs Bool stored as VDouble, logic error!"
    Nothing -> do
      pr <- eval' obsB
      let newMap = Map.insert (KObsBool obsB) (VBool pr) (cache st)
      put st { cache = newMap }
      return pr
  where
    eval' :: Obs Bool -> EvalM (PR Bool)
    eval' (DateO d) = return (datePr d)