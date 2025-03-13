{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EvaluationEngine where

import ContractsDSL
import ModelUtils

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map

---------------------------- Type-checker ----------------------------

typeCheck :: Contract -> Date -> Either Error ()
typeCheck None _ = Left "Error: Contract has no acquisition date set."
typeCheck (One _) _ = Left "Error: Contract has no acquisition date set."
typeCheck (Give c) d = do
  typeCheck c d
typeCheck (And c1 c2) d = do
  typeCheck c1 d
  typeCheck c2 d
typeCheck (Or c1 c2) d = do
  -- check for error in either here
  -- if both have an error throw an error
  -- if only one has an error return just the contract without the error
  typeCheck c1 d
  typeCheck c2 d
typeCheck (Scale _ c) d =
  typeCheck c d
typeCheck (AcquireOn d2 _) d1 
  | d2 < d1 = Left "Error: Top level contract with an expiry earlier than the model start date is prohibited."
  | otherwise = Right ()
typeCheck (AcquireWhen _ _) _ = Right ()
typeCheck (AcquireOnBefore d2 _) d1 
  | d2 < d1 = Left "Error: Top level contract with an expiry earlier than the model start date is prohibited."
  | otherwise = Right ()

------------------------- Optimisation layer -------------------------

optimiseContract :: Contract -> Contract
optimiseContract c = case c of
    AcquireOn d sub ->
      let sub' = optimiseContract sub 
       in 
          case sub' of
            AcquireOn d2 c2
              | d == d2  -> AcquireOn d (optimiseContract c2)
              | d > d2 -> AcquireOn d (None) 
            _ -> AcquireOn d sub'
    AcquireOnBefore d sub ->
      let sub' = optimiseContract sub
       in AcquireOnBefore d sub'
    AcquireWhen obs sub ->
      let sub' = optimiseContract sub
       in AcquireWhen obs sub'
    Give sub ->
      let sub' = optimiseContract sub
       in case sub' of
            None -> None
            (Give c') -> optimiseContract c'
            _    -> Give sub'
    Or c1 c2 ->
      let c1' = optimiseContract c1
          c2' = optimiseContract c2
       in if c1' == c2'
             then c1'
             else Or c1' c2'
    And c1 c2 ->
      let c1' = optimiseContract c1
          c2' = optimiseContract c2
       in case (c1', c2') of
            (None, x) -> x
            (x, None) -> x
            _         -> And c1' c2'
    Scale obs sub ->
      let sub' = optimiseContract sub
       in 
          Scale obs sub'
    None      -> None
    One cur   -> One cur


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
eval model c = do
  -- If typeCheck fails, we short-circuit with a Left.
  _ <- typeCheck c (startDate model)
  let optC = optimiseContract c
  -- Run our StateT-based evaluator with an empty cache:
  (result, _finalState) <- runStateT (evalC model optC (startDate model)) emptyEvalState
  return result

----------------------------------------------------------------------------
-- Memoized evaluation of a contract
----------------------------------------------------------------------------

evalC :: Model -> Contract -> Date -> EvalM (PR Double)
evalC model contract earliestAcDate = do
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
      return (constPr model 0)

    eval' (One cur) _ = do
      pr <- liftEither (exchange model cur)
      return pr

    eval' (Give c) d = do
      cVal <- evalC model c d
      return (negate cVal)

    eval' (And c1 c2) d = do
      pr1 <- evalC model c1 d
      pr2 <- evalC model c2 d
      return (pr1 + pr2)

    eval' (Or c1 c2) d = do
        pr1 <- evalC model c1 d
        pr2 <- evalC model c2 d
        return (maximumValToday pr1 pr2)

    eval' (AcquireOn d2 c) d1 = do
      if d2 < d1
        then return (constPr model 0)
        else do
          cVal  <- evalC model c d2
          pr    <- liftEither (discDate model d2 cVal)
          return pr

    eval' (AcquireOnBefore d2 c) d1 = do
      if d2 < d1
        then return (constPr model 0)
        else do
          cVal <- evalC model c (startDate model)
          pr   <- liftEither (snell model d2 cVal)
          return pr

    eval' (AcquireWhen obs c) d = do
      (PR obsPR) <- evalBO model obs
      d2 <- liftEither (findHorizon model obsPR 0)
      let d1 = (daysBetween (startDate model) d) `div` stepSize model
      if (d2 < d1) then return (constPr model 0) else do
        cVal  <- evalC model c d
        pr    <- liftEither (discObs model d2 cVal)
        return pr

    eval' (Scale obs c) d = do
      obsPR <- evalDO model obs
      cVal  <- evalC model c d
      return (obsPR * cVal)

----------------------------------------------------------------------------
-- Memoized evaluation of an observable
----------------------------------------------------------------------------

evalDO :: Model -> Obs Double -> EvalM (PR Double)
evalDO model obsD = do
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
    eval' (Konst k) = return (constPr model (realToFrac k))
    eval' (StockPrice stk) = do 
      pr <- liftEither (stockModel model stk)
      return pr
    eval' (LiftD op o) = do
      po <- evalDO model o
      return (ModelUtils.lift (unaryOpMap op) po)
    eval' (Lift2D op o1 o2) = do
      po1 <- evalDO model o1
      po2 <- evalDO model o2
      return (ModelUtils.lift2 (binaryOpMap op) po1 po2)  

evalBO :: Model -> Obs Bool -> EvalM (PR Bool)
evalBO model obsB = do
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
    eval' (Lift2B op o1 o2) = do
      po1 <- evalDO model o1
      po2 <- evalDO model o2
      return (ModelUtils.lift2 (compareOpMap op) po1 po2)