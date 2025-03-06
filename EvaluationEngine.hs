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

------------------------- Type-checker -------------------------

type Error = String

typeCheck :: Contract -> Date -> Either Error Contract 
typeCheck None d 
  | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
  | otherwise            = Right None
typeCheck (One cur) d 
  | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
  | otherwise            = Right (One cur)
typeCheck (Give c) d = do
  c' <- typeCheck c d
  return (Give c')
typeCheck (And c1 c2) d = do
  c1' <- typeCheck c1 d
  c2' <- typeCheck c2 d
  return (And c1' c2')
typeCheck (Or c1 c2) d = do
  c1' <- typeCheck c1 d
  c2' <- typeCheck c2 d
  return (Or c1' c2')
typeCheck (AcquireOn d2 c) d1
  | d1 /= infiniteHorizon && d1 > d2 = Left "Error: Attempting to acquire contract with an earlier expiry date at a later date."
  | otherwise = do
      c' <- typeCheck c d2
      return (AcquireOn d2 c')
typeCheck (AcquireOnBefore d2 c) d1
  | d1 /= infiniteHorizon && d1 > d2 = Left "Error: Attempting to acquire contract with an earlier expiry date at a later date."
  | otherwise = do
      c' <- typeCheck c d2
      return (AcquireOnBefore d2 c')
typeCheck (Scale obs c) d = do
  c' <- typeCheck c d
  return (Scale obs c')
typeCheck x d = return x


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

-- Nested acquire on:
optimiseContract (AcquireOn d1 (AcquireOn d2 c))  
    | d1 <= d2   = AcquireOn d1 (optimiseContract c)
    | otherwise = AcquireOn d1 (optimiseContract (AcquireOn d2 c))
optimiseContract (AcquireOn d c) = AcquireOn d (optimiseContract c)

optimiseContract (AcquireOnBefore d c) = AcquireOnBefore d (optimiseContract c)

optimiseContract (Scale obs c) = Scale obs (optimiseContract c)

-- Base cases: None, One, etc., are already in simplest form
optimiseContract contract = contract

----------------------------- Evaluation -----------------------------

type EvalM a = (State Cache) a
type Cache = Map.Map Contract (PR Double)

----------------------------------------------------------------------------
-- 1. Top-level eval_contract
----------------------------------------------------------------------------

eval_contract :: Model -> Contract -> Either String (PR Double)
eval_contract model c = case typeCheck c infiniteHorizon of
    Left err -> Left err
    Right c' -> let optimisedC = optimiseContract c' in
                Right (evalState (evalC model optimisedC) Map.empty)

----------------------------------------------------------------------------
-- 2. Memoized evaluation of a contract
----------------------------------------------------------------------------

evalC :: Model -> Contract -> EvalM (PR Double)
evalC model@(Model _ _ constPr discount discObs snell exchange _ _) contract = do
  cache <- get
  case Map.lookup contract cache of
    -- If this contract is already in the cache, just return it:
    Just result -> return result
    -- Otherwise evaluate and store it:
    Nothing -> do
      result <- eval contract
      modify (Map.insert contract result)
      return result
  where
    ----------------------------------------------------------------------------
    -- 2a. The local `eval` function does the real recursive work
    ----------------------------------------------------------------------------
    eval :: Contract -> EvalM (PR Double)
    eval None =
      return (constPr 0)

    eval (One cur) =
      return (exchange cur)

    eval (Give c) = do
      cVal <- evalC model c
      return (negate cVal)

    eval (And c1 c2) = do
      pr1 <- evalC model c1
      pr2 <- evalC model c2
      return (pr1 + pr2)

    eval (Or c1 c2) = do
      pr1 <- evalC model c1
      pr2 <- evalC model c2
      return (maximumValToday pr1 pr2)

    eval (AcquireOn d c) = do
      cVal <- evalC model c
      return (discount d cVal)

    eval (AcquireOnBefore d c) = do
      cVal <- evalC model c
      return (snell d cVal)

    eval (When obs c) = do
      obsPR <- evalO model obs
      cVal <- evalC model c
      return (discObs obsPR cVal)

    eval (Scale obs c) = do
      obsPR <- evalO model obs
      cVal   <- evalC model c
      return (obsPR * cVal)

----------------------------------------------------------------------------
-- 3. Evaluating an observation with memoization
----------------------------------------------------------------------------

evalO :: Model -> Obs a -> EvalM (PR a)
evalO model@(Model _ _ constPr _ _ _ _ stockModel datePr) obs = eval obs
  where
  -- do
  -- -- We reuse the same contract->PR cache, so we need a "fake" contract
  -- -- that represents “Scale obs (one GBP)” as a key.  This is a bit hacky but works
  -- -- because it would give correct result even if there is a colision.
  -- let key = Scale obs (one GBP)

  -- cache <- get
  -- case Map.lookup key cache of
  --   Just result -> return result
  --   Nothing -> do
  --     result <- eval obs
  --     modify (Map.insert key result)
  --     return result
  --     where 
        eval :: Obs a -> EvalM (PR a)
        eval (Konst k) = return (constPr (realToFrac k))
        eval (StockPrice stk) = return (stockModel stk)
        eval (DateO d) = return (datePr d)