module EvaluationEngine where

import ContractsDSL
import ModelUtils

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map

------------------------- Optimisation layer -------------------------

type Error = String

optimiseContract :: Contract -> Date -> Either Error Contract 
-- Double negation: Give (Give c) = c
optimiseContract (Give (Give c)) d = optimiseContract c d
optimiseContract (Give c) d = do
  c' <- optimiseContract c d
  return (Give c')

-- Avoiding double scale computation
optimiseContract (Or (Scale obs1 c1) (Scale obs2 c2)) d 
  | Konst k <- obs1, Konst j <- obs2, k == j, k >= 0 = do
      c1' <- optimiseContract c1 d
      c2' <- optimiseContract c2 d
      return (Scale obs1 (Or c1' c2'))
  | otherwise = do
      c1' <- optimiseContract c1 d
      c2' <- optimiseContract c2 d
      return (Or (Scale obs1 c1') (Scale obs2 c2'))

optimiseContract (Or c1 c2) d = do
  c1' <- optimiseContract c1 d
  c2' <- optimiseContract c2 d
  return (Or c1' c2')

optimiseContract (And c1 c2) d
  | c1 == None = optimiseContract c2 d
  | c2 == None = optimiseContract c1 d
  | otherwise  = do
      c1' <- optimiseContract c1 d
      c2' <- optimiseContract c2 d
      return (And c1' c2')

-- Nested AcquireOn
optimiseContract (AcquireOn d2 (AcquireOn d3 c)) d1
  | (d1 /= infiniteHorizon && d1 > d2) || d2 > d3 = Left "Error: Attempting to acquire contract with an earlier expiry date at a later date."
  | otherwise = do
      c' <- optimiseContract c d2
      return (AcquireOn d2 c')

optimiseContract (AcquireOn d2 c) d1
  | d1 /= infiniteHorizon && d1 > d2 = Left "Error: Attempting to acquire contract with an earlier expiry date at a later date."
  | otherwise = do
      c' <- optimiseContract c d2
      return (AcquireOn d2 c')

optimiseContract (AcquireOnBefore d2 c) d1
  | d1 /= infiniteHorizon && d1 > d2 = Left "Error: Attempting to acquire contract with an earlier expiry date at a later date."
  | otherwise = do
      c' <- optimiseContract c d2
      return (AcquireOnBefore d2 c')

optimiseContract (Scale obs c) d = do
  c' <- optimiseContract c d
  return (Scale obs c')

-- Base cases: None, One, etc., are already in simplest form
optimiseContract None d 
      | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
      | otherwise            = return None

optimiseContract (One cur) d 
      | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
      | otherwise            = return (One cur)


----------------------------- Evaluation -----------------------------

----------------------------------------------------------------------------
-- 1. Top-level eval_contract
----------------------------------------------------------------------------

type EvalM a = (State Cache) a
type Cache = Map.Map Contract (PR Double)

eval_contract :: Model -> Contract -> Date -> Either String (PR Double)
eval_contract model c d = case optimiseContract c d of
    Left err -> Left err
    Right c' -> Right (evalState (evalC model c') Map.empty)

----------------------------------------------------------------------------
-- 2. Memoized evaluation of a contract
----------------------------------------------------------------------------

evalC :: Model -> Contract -> EvalM (PR Double)
evalC model@(Model _ _ constPr discount snell exchange _) contract = do
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
      v1 <- evalC model c1
      v2 <- evalC model c2
      return (v1 + v2)

    eval (Or c1 c2) = do
      v1 <- evalC model c1
      v2 <- evalC model c2
      return (max v1 v2)

    eval (AcquireOn d c) = do
      cVal <- evalC model c
      return (discount d cVal)

    eval (AcquireOnBefore d c) = do
      cVal <- evalC model c
      return (snell d cVal)

    eval (Scale obs c) = do
      obsVal <- evalO model obs
      cVal   <- evalC model c
      return (obsVal * cVal)

----------------------------------------------------------------------------
-- 3. Evaluating an observation with memoization
----------------------------------------------------------------------------

evalO :: Model -> Obs Double -> EvalM (PR Double)
evalO model@(Model _ _ constPr _ _ _ stockModel) obs = do
  -- We reuse the same contract->PR cache, so we need a "fake" contract
  -- that represents “Scale obs (one GBP)” as a key.  This is a bit hacky but works
  -- because it would give correct result even if there is a colision.
  let key = Scale obs (one GBP)

  cache <- get
  case Map.lookup key cache of
    Just result -> return result
    Nothing -> do
      result <- eval obs
      modify (Map.insert key result)
      return result
      where 
        eval :: Obs Double -> EvalM (PR Double)
        eval (Konst k) = return (constPr k)
        eval (StockPrice stk) = return (stockModel stk)