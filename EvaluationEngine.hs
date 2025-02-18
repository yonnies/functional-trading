module EvaluationEngine where

import ContractsDSL
import ModelUtils

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

-- Nested acquire on: AcquireOn d1 (AcquireOn d2 c) → AcquireOn (min d1 d2) c
optimiseContract (AcquireOn d1 (AcquireOn d2 c))  
    | d1 <= d2   = AcquireOn d1 (optimiseContract c)
    | otherwise = AcquireOn d1 (optimiseContract (AcquireOn d2 c))
optimiseContract (AcquireOn d c) = AcquireOn d (optimiseContract c)

optimiseContract (AcquireOnBefore d c) = AcquireOnBefore d (optimiseContract c)

optimiseContract (Scale obs c) = Scale obs (optimiseContract c)

-- Base cases: None, One, etc., are already in simplest form
optimiseContract contract = contract


--------------------------- Evaluation ---------------------------

eval_contract :: Model -> Contract -> Date -> Either String (PR Double)
eval_contract model@(Model startDate stepSize constPr discount snell exchange stockModel) contract date = 
    eval (optimiseContract contract) date
    where 
        eval None d 
            | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
            | otherwise = Right (constPr 0)
        
        eval (One cur) d 
            | d == infiniteHorizon = Left "Error: Contract has no acquisition date set."
            | otherwise = Right (exchange cur)

        eval (Give c) d = fmap negate (eval c d)

        eval (And c1 c2) d = do
            pr1 <- eval c1 d
            pr2 <- eval c2 d
            return (pr1 + pr2)

        eval (Or c1 c2) d = do
            pr1 <- eval c1 d
            pr2 <- eval c2 d
            return (max pr1 pr2)

        eval (AcquireOn d2 c) d1
            | d2 > d1  = Left $ "Error: Attemting to acquire contract with an earlier expiry date at a later date."
            | otherwise = do
                val <- eval c d2
                return (discount d2 val)

        eval (AcquireOnBefore d2 c) d1
            | d2 > d1  = Left $ "Error: Attemting to acquire contract with an earlier expiry date at a later date."
            | otherwise = do
                val <- eval c d2
                return (snell d2 val)

        eval (Scale obs c) d = do
            val <- eval c d
            return ((eval_obs model obs) * val)

eval_obs :: Model -> Obs Double -> PR Double
eval_obs (Model startd stepSize constPr discount snell exchange stockModel) = eval
    where 
        eval (Konst k) = constPr k
        eval (StockPrice stk) = stockModel stk

