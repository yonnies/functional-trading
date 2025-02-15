module EvaluationEngine where

import ContractsDSL
import ModelUtils

--------------------------- Evaluation ---------------------------

eval_contract :: Model -> Contract -> PR Double
eval_contract model@(Model startDate stepSize constPr discount snell exchange stockModel) = eval
    where 
        eval None = constPr 0
        eval (One cur) = exchange cur
        eval (Give c) = negate (eval c)
        eval (And c1 c2) = (eval c1) + (eval c2)
        eval (Or c1 c2) = max (eval c1) (eval c2)
        eval (AcquireOn d c) = discount d (eval c) 
        eval (AcquireOnBefore d c) = snell d (eval c)
        eval (Scale obs c) = (eval_obs model obs) * (eval c)

eval_obs :: Model -> Obs Double -> PR Double
eval_obs (Model startDate stepSize constPr discount snell exchange stockModel) = eval
    where 
        eval (Konst k) = constPr k
        eval (StockPrice stk) = stockModel stk

