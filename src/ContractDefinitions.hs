module ContractDefinitions where

import ContractsDSL

europeanStockCall :: Date -> Double -> Stock -> Contract
europeanStockCall t strikePrice stk = 
    AcquireOn t 
        (Scale (StockPrice stk) (One GBP) `And` 
        Give (Scale (Konst strikePrice) (One GBP))) 
    `Or` AcquireOn t None

europeanStockPut :: Date -> Double -> Stock -> Contract
europeanStockPut t strikePrice stk = 
    AcquireOn t 
        (Scale (Konst strikePrice) (One GBP) `And` 
        Give (Scale (StockPrice stk) (One GBP)))
    `Or` AcquireOn t None

americanStockCall :: Date -> Double -> Stock -> Contract
americanStockCall t strikePrice stk = 
    AcquireOnBefore t 
        (Scale (StockPrice stk) (One GBP) `And` 
        Give (Scale (Konst strikePrice) (One GBP))) 
    `Or` AcquireOn t None


americanStockPut :: Date -> Double -> Stock -> Contract
americanStockPut t strikePrice stk = 
    AcquireOnBefore t 
        (Scale (Konst strikePrice) (One GBP) `And` 
        Give (Scale (StockPrice stk) (One GBP)))
    `Or` AcquireOn t None

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

upAndInOption :: Double -> Stock -> Double -> Contract
upAndInOption barrierPrice stk payoff =
            AcquireWhen (StockPrice stk %>= Konst barrierPrice) 
                        (Scale (Konst payoff) (One GBP)) -- Activate the contract if the barrier is breached

downAndInOption :: Double -> Stock -> Double -> Contract
downAndInOption barrierPrice stk payoff =
            AcquireWhen (StockPrice stk %<= Konst barrierPrice) 
                        (Scale (Konst payoff) (One GBP)) -- Activate the contract if the barrier is breached

shortfall :: Double -> Double -> Obs Double
shortfall goalYield actualYield = MaxObs (Konst 0) (Konst goalYield - GrainYield actualYield)

shortfallGrainYieldC :: Date -> Double -> Double -> Contract
shortfallGrainYieldC t goalYield actualYield =
    AcquireOn t (Scale ((shortfall goalYield actualYield) * Konst 3) (One USD))