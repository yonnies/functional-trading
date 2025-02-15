module TestEvaluation where

import ContractsDSL
import ModelUtils
import EvaluationEngine

-------------------- Testing ------------------------

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

-- Should subtract 5 after discounting
zcdb1 :: Contract
zcdb1 = And (zcdb (date "06-11-2028") 10 GBP) (Give $ (Scale (Konst 5) (One GBP)))
-- Example result:
--      VP [[3.6415112274834627],[3.900756564307965,4.246417013407305],[4.345794392523365,4.523809523809524,4.708737864077669],[5.0,5.0,5.0,5.0]] 

-- Should subtract 5 before discounting
zcdb2 :: Contract
zcdb2 = AcquireOn (date "06-11-2028") (And (Scale (Konst 10) (One GBP)) (Give $ (Scale (Konst 5) (One GBP))))
-- Example result:
--      VP [[4.320755613741731],[4.450378282153983,4.623208506703652],[4.672897196261682,4.761904761904762,4.854368932038835],[5.0,5.0,5.0,5.0]]

-- CALL OPTIONS
european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = AcquireOn t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` AcquireOn t none 

-- profitable
european1 :: Contract
european1 = european (date "05-04-2025") (Scale (StockPrice DIS) (One GBP)) 100 
-- Should return the underlying 

european2 :: Contract
european2 = european (date "05-03-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

american :: Date -> Contract -> Double -> Contract
american t underlying strikePrice = AcquireOnBefore t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` AcquireOnBefore t none 

american1 :: Contract
american1 = american (date "05-04-2025") (Scale (StockPrice DIS) (One GBP)) 100 

american2 :: Contract
american2 = american (date "05-03-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

-- use with take 4 unwrapPR
testContract =  ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 100) (one GBP)))


invalidContract = AcquireOn (date "06-11-2028") ((Scale (Konst 10) (One GBP)) `And` (AcquireOn (date "06-12-2024") $ Give $ (Scale (Konst 5) (One GBP))))

-- Helpers
unwrapPR :: PR a -> [ValSlice a]
unwrapPR (PR x) = x

exampleModelInstance = exampleModel today 30

zcdb12 = acquireOn (date "01-11-2028") ( scale (konst 1300) ( one GBP) )