module TestEvaluation where

import ContractsDSL
import ModelUtils
import EvaluationEngine

-------------------------------------------------------
-- Utility functions
-------------------------------------------------------

takePR :: Int -> PR a -> PR a
takePR n (PR x) = PR (take n x)

takeFailablePR :: Int -> Either Error (PR a) -> Either Error (PR a)
takeFailablePR n pr = case pr of 
    Left err -> Left err
    Right (PR x) -> Right $ PR (take n x)

eModel = exampleModel today 30

eModel2 = exampleModel today 365

-------------------------------------------------------
-- Discount bonds
-------------------------------------------------------

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

-------------------------------------------------------
-- European options
-------------------------------------------------------

-- CALL OPTIONS
european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = AcquireOn t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` AcquireOn t none 

-- profitable
european1 :: Contract
european1 = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 
-- Should return the underlying 

european2 :: Contract
european2 = european (date "01-02-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

european3 :: Contract
european3 = european (date "01-03-2027") (Scale (StockPrice DIS) (One GBP)) 100 

-- european3 = european (date "24 Apr 2003") (
--     zcb (date "12 May 2003") 0.4 GBP `and`
--     zcb (date "12 May 2004") 9.3 GBP `and`
--     zcb (date "12 May 2005") 109.3 GBP `and`
--     give (zcb (date "26 Apr 2003") 100 GBP)
-- )

-------------------------------------------------------
-- American options
-------------------------------------------------------

american :: Date -> Contract -> Double -> Contract
american t underlying strikePrice = AcquireOnBefore t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` AcquireOnBefore t none 

american1 :: Contract
american1 = american (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 
-- Should return the underlying

american2 :: Contract
american2 = american (date "05-02-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 

american3 :: Contract
american3 = american (date "01-03-2027") (Scale (StockPrice DIS) (One GBP)) 100 

-------------------------------------------------------
-- Invalid contracts
-------------------------------------------------------

inv1 = AcquireOn (date "06-11-2028") ((Scale (Konst 10) (One GBP)) `And` (AcquireOn (date "06-12-2024") $ Give $ (Scale (Konst 5) (One GBP))))

inv2 = acquireOn (date "01-01-2026") (acquireOn (date "01-01-2025") ((one GBP) `Or` none))

inv3 = european (date "01-03-2024") (Scale (StockPrice DIS) (One GBP)) 100 -- date is in the past

inv4 = european (date "01-03-2025") (Scale (StockPrice DIS) (One BGN)) 100 -- inv currency

-- inv5 = AcquireWhen (dateO (date "01-03-2023")) ((Scale (StockPrice DIS) (One GBP)))


-------------------------------------------------------
-- Optimisation Layer tests
-------------------------------------------------------

testDoubleNegation :: Contract
testDoubleNegation = give (give (One GBP))

testScalingOr :: Contract
testScalingOr = (Or (Scale (Konst 2) (One GBP)) (Scale (Konst 2) (One USD)))

testScalingOr2 :: Contract
testScalingOr2 = AcquireOn (date "01-02-2025") (Or (Scale (Konst 2) (One GBP)) (Scale (Konst 2) (One USD)))

-------------------------------------------------------
-- Random contracts
-------------------------------------------------------

c1 = AcquireOn (date "01-02-2026") (Give (Or (Give (Give (One EUR))) None))

c2 = AcquireOn (date "01-02-2030") (Give (And (Or (Or (One USD) (Or (One USD) None)) (AcquireOnBefore (date "01-07-2030") (Give None))) None))

-- c3 = AcquireWhen (dateO (date "01-03-2025")) ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 100) (one GBP))) `Or` AcquireWhen (dateO (date "01-03-2025")) none 

c4 = acquireOn (date "01-11-2028") (scale (konst 1300 + konst 400) (one GBP))

c5 = AcquireOn (date "01-03-2025") ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 80 + konst 20) (one GBP)))


-- One is too early
c6 = AcquireOn (date "01-03-2026") ((AcquireOn (date "01-03-2025") (scale (konst 1000) (one GBP))) `Or` (AcquireOn (date "01-03-2027") (scale (konst 1) (one GBP))))

-- Neither is too early 
c7 = AcquireOn (date "01-03-2026") ((AcquireOn (date "01-03-2027") (scale (konst 1000) (one GBP))) `Or` (AcquireOn (date "01-03-2027") (scale (konst 1) (one GBP))))

-- Both are too early
c8 = AcquireOn (date "01-03-2026") ((AcquireOn (date "01-03-2025") (scale (konst 1000) (one GBP))) `Or` (AcquireOn (date "01-03-2025") (scale (konst 1) (one GBP))))

-- American with an earlier date
c9 = AcquireOnBefore (date "01-03-2027") c9_underlying

c9_underlying = (AcquireOn (date "01-09-2026") (scale (konst 1000) (one GBP)))

-- 
c10 = AcquireOnBefore (date "01-09-2026") (AcquireOn (date "01-09-2029") (Or (One EUR) (One GBP)))
c11 = AcquireOnBefore (date "01-09-2025") (Scale (Konst 0.6) (And (One GBP) (One USD)))

c12 = Scale (konst 2) (c10 `or_` c11)
c13 = Scale (konst 2) c10 `or_`  Scale (konst 2) c11

c14 = Scale (Konst (-2.6666666666666665)) (AcquireOn (date "25-12-2030") (Or (And (AcquireOnBefore (date "30-12-2030") (Give (One EUR))) (One GBP)) (One GBP)))

c15 = acquireWhen (stockPrice NVDA %> 300) (scale (stockPrice TSLA) (one USD))

-------------------------------------------------------
-- Model specific functions
-------------------------------------------------------

-- datePrTest = datePr eModel (date "01-02-2025")

stockModelTest = takeFailablePR 5 $ stockModel eModel DIS

-- Define a function to evaluate a contract and print its result
runTest :: String -> Contract -> IO ()
runTest name contract = do
    let result = eval eModel contract
    putStrLn $ "Running test: " ++ name
    case result of
        Left err -> putStrLn $ "Error: " ++ err
        Right val -> putStrLn $ "Result:\n" ++ show val
    putStrLn "----------------------------"

-- List of test cases with their names
testCases :: [(String, Contract)]
testCases =
    [ ("zcdb1", zcdb1)
    , ("zcdb2", zcdb2)
    , ("european1", european1)
    , ("european2", european2)
    , ("european3", european3)
    , ("american1", american1)
    , ("american2", american2)
    , ("american3", american3)
    , ("inv1", inv1)
    , ("inv2", inv2)
    , ("inv3", inv3)
    , ("inv4", inv4)
    , ("testDoubleNegation", testDoubleNegation)
    , ("testScalingOr", testScalingOr)
    , ("testScalingOr2", testScalingOr2)
    , ("c1", c1)
    , ("c2", c2)
    , ("c4", c4)
    , ("c5", c5)
    , ("c6", c6)
    , ("c7", c7)
    , ("c8", c8)
    , ("c9", c9)
    , ("c10", c10)
    , ("c11", c11)
    , ("c12", c12)
    , ("c13", c13)
    , ("c14", c14)
    , ("c15", c15)
    ]

-- Run all tests
runAllTests :: IO ()
runAllTests = mapM_ (uncurry runTest) testCases

-- Main function to execute tests when the script is run
main :: IO ()
main = runAllTests