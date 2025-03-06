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
zcdb2 = AcquireOn (date "01-11-2027") (And (Scale (Konst 10) (One GBP)) (Give $ (Scale (Konst 5) (One GBP))))
-- Example result:
--      VP [[4.320755613741731],[4.450378282153983,4.623208506703652],[4.672897196261682,4.761904761904762,4.854368932038835],[5.0,5.0,5.0,5.0]]

-- CALL OPTIONS
european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = AcquireOn t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` AcquireOn t none 

-- AcquireOn (date "01-04-2025") (underlying `And` give (scale (konst strikePrice) (one GBP)))

-- profitable
european1 :: Contract
european1 = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 
-- Should return the underlying 

european2 :: Contract
european2 = european (date "01-02-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

american :: Date -> Contract -> Double -> Contract
american t underlying strikePrice = AcquireOnBefore t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` AcquireOnBefore t none 

american1 :: Contract
american1 = american (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 

american2 :: Contract
american2 = american (date "05-02-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

-- use with take 4 unwrapPR
testContract =  ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 100) (one GBP)))

invalidContract = AcquireOn (date "06-11-2028") ((Scale (Konst 10) (One GBP)) `And` (AcquireOn (date "06-12-2024") $ Give $ (Scale (Konst 5) (One GBP))))

invalidAcquire :: Contract
invalidAcquire = acquireOn (date "01-01-2026") (acquireOn (date "01-01-2025") ((one GBP) `Or` none))

-- Helpers
unwrapPR :: PR a -> [ValSlice a]
unwrapPR (PR x) = x

takePR :: Int -> PR a -> [ValSlice a]
takePR n (PR x) = take n x

exampleModelInstance = exampleModel today 30

zcdb12 = acquireOn (date "01-11-2028") (scale (konst 1300) (one GBP))


-- takePR 5 $ stockModel exampleModelInstance DIS
-- Step 0: 109.12
-- Step 1: 136.69445697731393 87.10795348472789
-- Step 2: 171.236937026418 109.11999999999999 69.53624963615759
-- Step 3: 214.50824891206662 136.69445697731393 87.10795348472787 55.50916787764701
-- Step 4: 268.7141550787155 171.23693702641796 109.11999999999999 69.53624963615758 44.31167534331036
-- Step 5: 336.61781076432135 214.5082489120666 136.6944569773139 87.10795348472787 55.509167877646995 35.3729779567031

-- ghci> eval_contract exampleModelInstance european1 infiniteHorizon
-- Right Step 0: 16.652770825682584
-- Step 1: 40.398043697787344 -5.427224963853919
-- Step 2: 71.7487192692069 13.318807679406989 -24.684861362738847
-- Step 3: 113.33408559819057 38.16101360716087 -10.191517480406192 -41.363614189375795
-- Step 4: 168.71415507871552 71.23693702641796 9.11999999999999 -30.463750363842422 -55.68832465668964

testDoubleNegation :: Contract
testDoubleNegation = give (give (One GBP))

testScalingOr :: Contract
testScalingOr = (Or (Scale (Konst 2) (One GBP)) (Scale (Konst 2) (One USD)))

testScalingOr2 :: Contract
testScalingOr2 = AcquireOn (date "01-02-2025") (Or (Scale (Konst 2) (One GBP)) (Scale (Konst 2) (One USD)))

-- c5 = european (date "24 Apr 2003") (
--     zcb (date "12 May 2003") 0.4 GBP `and`
--     zcb (date "12 May 2004") 9.3 GBP `and`
--     zcb (date "12 May 2005") 109.3 GBP `and`
--     give (zcb (date "26 Apr 2003") 100 GBP)
-- )


c1 = AcquireOn (date "01-02-2026") (Give (Or (Give (Give (One EUR))) None))


c2 = AcquireOn (date "01-02-2030") (Give (And (Or (Or (One USD) (Or (One USD) None)) (AcquireOnBefore (date "01-07-2030") (Give None))) None))



-- Failed:   
-- NonNegative {getNonNegative = 0.16132386943986787}
-- AcquireOn 2025-11-30 (Give (Or (Give (Give (One EUR))) None))
-- AcquireOnBefore 2030-06-23 (Give (AcquireOnBefore 2030-07-05 (And (Or None None) None)))

-- Failed:                                                  
-- NonNegative {getNonNegative = 0.0}
-- AcquireOn 2025-11-30 (Give (Or (Give (Give (One EUR))) None))
-- AcquireOnBefore 2030-06-23 (Give (AcquireOnBefore 2030-07-05 (And (Or None None) None)))


-- Failed:   
-- NonNegative {getNonNegative = 92.38636363636364}
-- AcquireOnBefore 2026-04-28 (Give (Or (Give (Scale (Konst 69.55) (Give (Scale (Konst (-55.56666666666667)) (One USD))))) (AcquireOn 2026-06-12 (AcquireOn 2030-11-25 (One USD)))))
-- Or (Or (AcquireOnBefore 2026-03-29 (Scale (StockPrice TSLA) (Give None))) (Give (AcquireOnBefore 2030-08-12 (AcquireOnBefore 2030-12-03 (AcquireOn 2030-12-07 (One EUR)))))) (Give (AcquireOnBefore 2027-06-19 (Or (Scale (Konst (-36.53333333333333)) None) (Give None))))

-- Failed:                                                  
-- NonNegative {getNonNegative = 92.0}
-- AcquireOnBefore 2026-04-28 (Give (Or (Give (Scale (Konst 69.55) (Give (Scale (Konst (-55.56666666666667)) (One USD))))) (AcquireOn 2026-06-12 (AcquireOn 2030-11-25 (One USD)))))
-- Or (Or (AcquireOnBefore 2026-03-29 (Scale (StockPrice TSLA) (Give None))) (Give (AcquireOnBefore 2030-08-12 (AcquireOnBefore 2030-12-03 (AcquireOn 2030-12-07 (One EUR)))))) (Give (AcquireOnBefore 2027-06-19 (Or (Scale (Konst (-36.53333333333333)) None) (Give None))))

-- Failed:                                                               
-- NonNegative {getNonNegative = 0.0}
-- AcquireOnBefore 2026-04-28 (Give (Or (Give (Scale (Konst 69.55) (Give (Scale (Konst (-55.56666666666667)) (One USD))))) (AcquireOn 2026-06-12 (AcquireOn 2030-11-25 (One USD)))))
-- Or (Or (AcquireOnBefore 2026-03-29 (Scale (StockPrice TSLA) (Give None))) (Give (AcquireOnBefore 2030-08-12 (AcquireOnBefore 2030-12-03 (AcquireOn 2030-12-07 (One EUR)))))) (Give (AcquireOnBefore 2027-06-19 (Or (Scale (Konst (-36.53333333333333)) None) (Give None))))


-- Failed:   
-- NonNegative {getNonNegative = 7.0}
-- AcquireOn 2030-08-09 (Give (And (Or (Or (One USD) (Or (One USD) None)) (AcquireOnBefore 2030-11-07 (Give None))) None))
-- Scale (Konst (-49.0)) (AcquireOn 2029-01-03 (AcquireOn 2029-09-14 (Give (Or (Or (One GBP) (One EUR)) (Or (One GBP) (One EUR))))))

-- Failed:                                                  
-- NonNegative {getNonNegative = 0.0}
-- AcquireOn 2030-08-09 (Give (And (Or (Or (One USD) (Or (One USD) None)) (AcquireOnBefore 2030-11-07 (Give None))) None))
-- Scale (Konst (-49.0)) (AcquireOn 2029-01-03 (AcquireOn 2029-09-14 (Give (Or (Or (One GBP) (One EUR)) (Or (One GBP) (One EUR))))))

-- *** Failed! Timeout of 1000000 microseconds exceeded. (after 68 tests and 1 shrink):
-- NonNegative {getNonNegative = 0.0}
-- AcquireOn 2030-08-09 (Give (And (Or (Or (One USD) (Or (One USD) None)) (AcquireOnBefore 2030-11-07 (Give None))) None))
-- Scale (Konst (-49.0)) (AcquireOn 2029-01-03 (AcquireOn 2029-09-14 (Give (Or (Or (One GBP) (One EUR)) (Or (One GBP) (One EUR))))))
-- ghci> 


-- test = datePr exampleModelInstance (date "01-02-2025")


c3 = AcquireWhen (dateO (date "01-03-2025")) ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 100) (one GBP))) `Or` AcquireWhen (dateO (date "01-03-2025")) none 


-- european1 :: Contract
-- european1 = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 
-- -- Should return the underlying 


c4 = acquireOn (date "01-11-2028") (scale (konst 1300 + konst 400) (one GBP))


c5 = AcquireOn (date "01-03-2025") ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 80 + konst 20) (one GBP)))

