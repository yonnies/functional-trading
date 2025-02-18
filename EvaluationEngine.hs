module EvaluationEngine where

import ContractsDSL
import ModelUtils

------------------------- Optimisation layer -------------------------

-- | Optimise a contract by rewriting it using known equivalences.
optimiseContract :: Contract -> Contract
optimiseContract contract = case contract of
    -- Avoiding double scale computation
    Or (Scale obs1 c1) (Scale obs2 c2) -> 
        case (obs1, obs2) of
            (Konst k, Konst j) | k == j && k >= 0 -> 
                Scale obs1 (optimiseContract (Or c1 c2))
            _ -> 
                Or (Scale obs1 (optimiseContract c1)) (Scale obs2 (optimiseContract c2))

    -- Double negation: give (give c) = c
    Give (Give c) ->
        optimiseContract c

    -- Removing unecessary None computation
    And None c ->
        optimiseContract c
    And c None ->
        optimiseContract c
    And c1 c2 -> 
        if c1 == c2 then
            Scale (Konst 2) (optimiseContract c1)

    -- Nested aquire on
    AcquireOn d1 (AcquireOn d2 c) ->
        if d1 < d2 then AcquireOn d1 (optimiseContract c)
        else AcquireOn d1 (AcquireOn d2 c)

    -- Recursively optimising subcontracts
    And c1 c2 ->
        And (optimiseContract c1) (optimiseContract c2)
    Or c1 c2 ->
        Or (optimiseContract c1) (optimiseContract c2)
    Give c ->
        Give (optimiseContract c)
    AcquireOn d c ->
        AcquireOn d (optimiseContract c)    
    AcquireOnBefore d c ->
        AcquireOnBefore d (optimiseContract c)
    Scale obs c ->
        Scale obs (optimiseContract c)

    -- Base cases: None, One, etc., are already in simplest form
    _ -> contract


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

