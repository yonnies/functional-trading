module EvaluationEngine where

import ContractsDSL
import ModelUtils

--------------------------- Evaluation ---------------------------

eval_contract :: Model -> Contract -> Date -> Either String (PR Double)
eval_contract model@(Model startDate stepSize constPr discount snell exchange stockModel) = eval
    where 
        eval None date 
            | date == infiniteHorizon = Left "Error: Contract has no acquisition date set."
            | otherwise = Right (constPr 0)
        
        eval (One cur) date 
            | date == infiniteHorizon = Left "Error: Contract has no acquisition date set."
            | otherwise = Right (exchange cur)

        eval (Give c) date = fmap negate (eval c date)

        eval (And c1 c2) date = do
            pr1 <- eval c1 date
            pr2 <- eval c2 date
            return (pr1 + pr2)

        eval (Or c1 c2) date = do
            pr1 <- eval c1 date
            pr2 <- eval c2 date
            return (max pr1 pr2)

        eval (AcquireOn d c) date
            | d > date  = Left $ "Error: Attemting to acquire contract with an earlier expiry date at a later date."
            | otherwise = do
                val <- eval c d
                return (discount d val)

        eval (AcquireOnBefore d c) date
            | d > date  = Left $ "Error: Attemting to acquire contract with an earlier expiry date at a later date."
            | otherwise = do
                val <- eval c d
                return (snell d val)

        eval (Scale obs c) date = do
            val <- eval c date
            return ((eval_obs model obs) * val)

eval_obs :: Model -> Obs Double -> PR Double
eval_obs (Model startDate stepSize constPr discount snell exchange stockModel) = eval
    where 
        eval (Konst k) = constPr k
        eval (StockPrice stk) = stockModel stk

