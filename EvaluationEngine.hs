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

type EvalM a = ExceptT String (State Cache) a
type Cache = Map.Map Contract (PR Double)

eval_contract :: Model -> Contract -> Date -> Either String (PR Double)
eval_contract model contract date = case optimiseContract contract date of
    Left err -> Left err
    Right optimisedContract -> evalState (runExceptT (eval_contract_cache model optimisedContract date)) Map.empty

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
      | d1 /= infiniteHorizon, d2 < d1 = throwError "Error: Attempting to acquire contract with an earlier expiry date at a later date."
      | otherwise = do
          val <- eval_contract_cache model c d2
          return (discount d2 val)
    
    eval (AcquireOnBefore d2 c) d1
      | d1 /= infiniteHorizon, d2 < d1 = throwError "Error: Attempting to acquire contract with an earlier expiry date at a later date."
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