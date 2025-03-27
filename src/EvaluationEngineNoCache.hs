{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module EvaluationEngineNoCache where

import ContractsDSL
import ModelUtils

import Control.Monad.Except

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
       in case sub' of
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
       in case (c1', c2') of
            (None, x) -> x
            (x, None) -> x
            (Scale obs1 c1'', Scale obs2 c2'') -> 
                case (obs1, obs2) of
                    (Konst k, Konst j) | k == j && k >= 0 -> 
                        Scale obs1 (Or c1'' c2'')
                    _ -> Or c1' c2'
            _ -> if c1' == c2'
                    then c1'
                    else Or c1' c2'
    And c1 c2 ->
      let c1' = optimiseContract c1
          c2' = optimiseContract c2
       in case (c1', c2') of
            (None, x) -> x
            (x, None) -> x
            (Scale obs1 c1'', Scale obs2 c2'') -> 
                if obs1 == obs2 
                  then Scale obs1 (And c1'' c2'')
                  else And c1' c2'
            _         -> And c1' c2'
    Scale obs sub ->
      let sub' = optimiseContract sub
       in case obs of
            Konst 1 -> sub'  
            Konst k1 -> 
              case sub' of
                Scale (Konst k2) c' -> Scale (Konst (k1 * k2)) c'
                _ -> Scale (Konst k1) sub'
            _ -> Scale obs sub'
    None      -> None
    One cur   -> One cur


----------------------------- Evaluation -----------------------------

-- ---------------------------------------
-- -- Cache
-- ---------------------------------------
-- data Key
--   = KContract Contract
--   | KObsDouble (Obs Double)
--   | KObsBool   (Obs Bool)
--   deriving (Eq, Ord, Show)

-- data Val
--   = VDouble (PR Double)
--   | VBool   (PR Bool)
--   deriving (Show)

-- type Cache = Map.Map Key Val

-- -- We'll keep an EvalState with that single cache
-- data EvalState = EvalState
--   { cache :: Cache
--   }

-- -- The monad: we carry our cache (EvalState) in StateT,
-- -- and can fail with String errors using Either String
-- type EvalM a = StateT EvalState (Either String) a

-- emptyEvalState :: EvalState
-- emptyEvalState = EvalState { cache = Map.empty }

type EvalM a = Either String a


---------------------------------------
-- Top-level evaluation
---------------------------------------
eval :: Model -> Contract -> EvalM (PR Double)
eval model c = do
  _ <- typeCheck c (startDate model)
  let optC = optimiseContract c
  evalC model optC (startDate model)

----------------------------------------------------------------------------
-- Memoized evaluation of a contract
----------------------------------------------------------------------------

evalC :: Model -> Contract -> Date -> EvalM (PR Double)
evalC model None _ = 
  return (constPr model 0)

evalC model (One cur) _ = do
  pr <- liftEither (exchange model cur)
  return pr

evalC model (Give c) d = do
  cVal <- evalC model c d
  return (negate cVal)

evalC model (And c1 c2) d = do
  pr1 <- evalC model c1 d
  pr2 <- evalC model c2 d
  return (pr1 + pr2)

evalC model (Or c1 c2) d = do
    pr1 <- evalC model c1 d
    pr2 <- evalC model c2 d
    return (maximumValToday pr1 pr2)

evalC model (AcquireOn d2 c) d1 = do
  if d2 < d1
    then return (constPr model 0)
    else do
      cVal  <- evalC model c d2
      pr    <- liftEither (discDate model d2 cVal)
      return pr

evalC model (AcquireOnBefore d2 c) d1 = do
  if d2 < d1
    then return (constPr model 0)
    else do
      cVal <- evalC model c (startDate model)
      pr   <- liftEither (snell model d2 cVal)
      return pr

evalC model (AcquireWhen obs c) d = do
  (PR obsPR) <- evalBO model obs
  d2 <- liftEither (findHorizon model obsPR 0)
  let d1 = (daysBetween (startDate model) d) `div` stepSize model
  if (d2 < d1) then return (constPr model 0) else do
    cVal  <- evalC model c d
    pr    <- liftEither (discObs model d2 cVal)
    return pr

evalC model (Scale obs c) d = do
  obsPR <- evalDO model obs
  cVal  <- evalC model c d
  return (obsPR * cVal)

----------------------------------------------------------------------------
-- Memoized evaluation of an observable
----------------------------------------------------------------------------

evalDO :: Model -> Obs Double -> EvalM (PR Double)
evalDO model (Konst k) = return (constPr model (realToFrac k))
evalDO model (StockPrice stk) = do 
  pr <- liftEither (stockModel model stk)
  return pr
evalDO model (LiftD op o) = do
  po <- evalDO model o
  return (ModelUtils.lift (unaryOpMap op) po)
evalDO model (Lift2D op o1 o2) = do
  po1 <- evalDO model o1
  po2 <- evalDO model o2
  return (ModelUtils.lift2 (binaryOpMap op) po1 po2)  
evalDO model (MaxObs o1 o2) = do
  po1 <- evalDO model o1
  po2 <- evalDO model o2
  return (maxPR po1 po2)

evalBO :: Model -> Obs Bool -> EvalM (PR Bool)
evalBO model (Lift2B op o1 o2) = do
  po1 <- evalDO model o1
  po2 <- evalDO model o2
  return (ModelUtils.lift2 (compareOpMap op) po1 po2)