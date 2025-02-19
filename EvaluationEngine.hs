module EvaluationEngine where

import ContractsDSL
import ModelUtils

import Control.Monad.Except
import Control.Monad.State
import qualified Data.Map.Strict as Map

------------------------- Optimisation layer -------------------------

optimiseContract :: Contract -> Contract
-- Double negation: give (give c) = c
optimiseContract (Give (Give c)) = optimiseContract c
optimiseContract (Give c) = Give (optimiseContract c)

-- Avoiding double scale computation
optimiseContract (Or (Scale obs1 c1) (Scale obs2 c2))  
    | Konst k <- obs1, Konst j <- obs2, k == j, k >= 0 = Scale obs1 (optimiseContract (Or c1 c2))
    | otherwise = Or (Scale obs1 (optimiseContract c1)) (Scale obs2 (optimiseContract c2))
optimiseContract (Or c1 c2) = Or (optimiseContract c1) (optimiseContract c2)

optimiseContract (And c1 c2)  
    | c1 == c2  = Scale (Konst 2) (optimiseContract c1)  -- Duplicate contract simplification
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

type EvalM a = ExceptT String (State Cache) a
type Cache = Map.Map Contract (PR Double)

eval_contract :: Model -> Contract -> Date -> Either String (PR Double)
eval_contract model contract date =
  evalState (runExceptT (eval_contract_cache model (optimiseContract contract) date)) Map.empty

eval_contract_cache :: Model -> Contract -> Date -> EvalM (PR Double)
eval_contract_cache model@(Model startDate stepSize constPr discount snell exchange _) contract date = do
  cache <- get
  case Map.lookup contract cache of
    Just result -> return result
    Nothing -> do
      result <- eval contract date
      modify (Map.insert contract result)
      return result
  where
    eval :: Contract -> Date -> EvalM (PR Double)
    eval None d 
      | d == infiniteHorizon = throwError "Error: Contract has no acquisition date set."
      | otherwise            = return (constPr 0)
    
    eval (One cur) d 
      | d == infiniteHorizon = throwError "Error: Contract has no acquisition date set."
      | otherwise            = return (exchange cur)
    
    eval (Give c) d = do
      pr <- eval_contract_cache model c d
      return (negate pr)
    
    eval (And c1 c2) d = do
      pr1 <- eval_contract_cache model c1 d
      pr2 <- eval_contract_cache model c2 d
      return (pr1 + pr2)
    
    eval (Or c1 c2) d = do
      pr1 <- eval_contract_cache model c1 d
      pr2 <- eval_contract_cache model c2 d
      return (max pr1 pr2)
    
    eval (AcquireOn d2 c) d1
      | d2 > d1   = throwError "Error: Attempting to acquire contract with an earlier expiry date at a later date."
      | otherwise = do
          val <- eval_contract_cache model c d2
          return (discount d2 val)
    
    eval (AcquireOnBefore d2 c) d1
      | d2 > d1   = throwError "Error: Attempting to acquire contract with an earlier expiry date at a later date."
      | otherwise = do
          val <- eval_contract_cache model c d2
          return (snell d2 val)
    
    eval (Scale obs c) d = do
      val    <- eval_contract_cache model c d
      obsVal <- eval_obs model obs
      return (obsVal * val)

eval_obs :: Model -> Obs Double -> EvalM (PR Double)
eval_obs model@(Model _ _ constPr _ _ _ stockModel) obs = do
  cache <- get
  let key = Scale obs None
  case Map.lookup key cache of
    Just result -> return result
    Nothing -> do
      result <- case obs of
                  Konst k       -> return (constPr k)
                  StockPrice stk -> return (stockModel stk)
      modify (Map.insert key result)
      return result