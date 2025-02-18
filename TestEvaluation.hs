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
american1 = american (date "01-02-2025") (Scale (StockPrice DIS) (One GBP)) 100 

american2 :: Contract
american2 = american (date "05-02-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

-- use with take 4 unwrapPR
testContract =  ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 100) (one GBP)))

invalidContract = AcquireOn (date "06-11-2028") ((Scale (Konst 10) (One GBP)) `And` (AcquireOn (date "06-12-2024") $ Give $ (Scale (Konst 5) (One GBP))))

-- Helpers
unwrapPR :: PR a -> [ValSlice a]
unwrapPR (PR x) = x

exampleModelInstance = exampleModel today 30

zcdb12 = acquireOn (date "01-11-2028") ( scale (konst 1300) ( one GBP) )

-- take for Processes
takePR :: Int -> PR a -> PR a
takePR n (PR x) = PR (take n x)

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

-- VP [[16.652770825682584],
-- [40.398043697787344,-5.427224963853919],
-- [71.7487192692069,13.318807679406989,-24.684861362738847],
-- [113.33408559819057,38.16101360716087,-10.191517480406192,-41.363614189375795],
-- [168.71415507871552,71.23693702641796,9.11999999999999,-30.463750363842422,-55.68832465668964]]


testDoubleNegation :: Contract
testDoubleNegation = give (give (One GBP))

testScalingOr :: Contract
testScalingOr = (Or (Scale (Konst 2) (One GBP)) (Scale (Konst 2) (One USD)))

testScalingAnd :: Contract
testScalingAnd = (And (Scale (Konst 3) (One GBP)) (Scale (Konst 2) (One USD)))