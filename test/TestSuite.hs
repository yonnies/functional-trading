{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative (liftA2)
import Text.Printf (printf)
import Control.Monad.State

import ContractsDSL 
import ModelUtils
import EvaluationEngine
import ContractParser

----------------------------------------------------------------
-- Running all tests
----------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testGroup "Structural (no eval needed)"
      [ testProperty "doubleNegation"             prop_doubleNegation
      , testProperty "optimiseIdempotent"         prop_optimise_idempotent
      , testProperty "contractParserConsistency"  prop_contractParserConsistency
      ]
  , testGroup "Valuation properties"
      [ testProperty "optLayerPreservesMeanign"   prop_optLayerPreservesMeanign
      , testProperty "orScaleDistributive"        prop_orScaleDistributive
      , testProperty "giveNegates"                prop_give_negates
      , testProperty "andAdds"                    prop_and_adds
      , testProperty "orMax"                      prop_or_max
      ]
  , testGroup "HUnit tests"
      [ testCase "PR_structure"                   unit_test_PR_structure
      , testCase "acquireOnStartDate"             unit_test_acquireOnStartDate
      , testCase "profitable_european"            unit_test_profitable_european
      , testCase "unprofitable_european"          unit_test_unprofitable_european
      , testCase "profitable_american"            unit_test_profitable_american
      , testCase "unprofitable_american"          unit_test_unprofitable_american
      , testCase "american_vs_eu"                 unit_test_american_vs_eu
      , testCase "expired_c_acquisition"          unit_test_expired_c_acquisition
      , testCase "expired_c_acquisition2"         unit_test_expired_c_acquisition2
      , testCase "unsupported_currency"           unit_test_unsupported_currency
      , testCase "unsupported_stock"              unit_test_unsupported_stock
      , testCase "expired_nested_part_evals_to_zero" unit_test_expired_nested_part_evals_to_zero
      , testCase "american_allows_expired_underlying" unit_test_american_allows_expired_underlying
      , testCase "observable_summation"           unit_test_observable_summation
      , testCase "and_takes_later_expiry"         unit_test_and_takes_later_expiry
      ]
  ]


----------------------------------------------------------------
-- Generic helper to compare two evaluated contracts in HUnit
----------------------------------------------------------------

-- Compares two Either Error (PR Double) results within an Assertion.
-- If both are Right, it does a numeric approximate check.
-- If one or both are Left, it shows an error message.
assertPRApproxEqual 
  :: String                      -- Label or test name
  -> Either Error (PR Double)    -- Left-hand evaluation
  -> Either Error (PR Double)    -- Right-hand evaluation
  -> Assertion
assertPRApproxEqual testName leftE rightE =
  case (leftE, rightE) of
    (Left errL, _) ->
      assertFailure (testName ++ ": Left side failed: " ++ errL)

    (_, Left errR) ->
      assertFailure (testName ++ ": Right side failed: " ++ errR)

    (Right prL, Right prR) ->
      let eps  = 1e-4
          same = prApproxEq eps prL prR
      in assertBool
           (  testName 
           ++ " mismatch:\nLeft:\n"     ++ show prL
           ++ "\nRight:\n"             ++ show prR 
           )
           same


----------------------------------------------------------------
-- Approximate equality checks for contract evaluation
----------------------------------------------------------------

-- prApproxEq checks if two PR (value process) results are approximately equal.
-- Since PR values are lists of lists (representing binomial trees), we compare
-- them element-wise with a given epsilon tolerance.
prApproxEq :: Double -> PR Double -> PR Double -> Bool
prApproxEq epsilon (PR xs) (PR ys) = and $ zipWith eqLayers xs ys
  where
    eqLayers as bs = and $ zipWith (\a b -> abs (a - b) < epsilon) as bs

-- Custom equality operator for comparing evaluated contracts.
(≈) :: Either Error (PR Double) -> Either Error (PR Double) -> Property
a ≈ b = case (a, b) of
    (Right pr1, Right pr2) -> 
        counterexample (printf "Expected:\n%s\nActual:\n%s" 
                        (show pr2) (show pr1)) $
        property (prApproxEq 1e-4 pr1 pr2)
    (Left err1, Left err2) -> 
        counterexample (printf "Both failed with errors:\n%s\n%s" err1 err2) True
    (Left err1, Right _) -> 
        counterexample ("Mismatch: First failed with error: " ++ err1) False
    (Right _, Left err2) -> 
        counterexample ("Mismatch: Second failed with error: " ++ err2) False


----------------------------------------------------------------
-- Utilities for testing and constructing financial contracts
----------------------------------------------------------------

sampleStartDate :: Date
sampleStartDate = date "01-11-2024"

evalTest :: Model -> Contract -> Bool -> Either String (PR Double)
evalTest model c optContract= do
  _ <- validityCheck c (startDate model)
  let optC = if optContract then optimiseContract c else c
  (result, _finalState) <- runStateT (evalC model optC (startDate model)) emptyEvalState
  return result

european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = 
  AcquireOn t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOn t None 

american :: Date -> Contract -> Double -> Contract
american t underlying strikePrice = 
  AcquireOnBefore t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOnBefore t None

modelWithMonthStep :: Model
modelWithMonthStep = exampleModel sampleStartDate 30

modelWithDayStep :: Model
modelWithDayStep = exampleModel sampleStartDate 1

----------------------------------------------------------------
-- QuickCheck properties
----------------------------------------------------------------

prop_contractParserConsistency :: Contract -> Property
prop_contractParserConsistency contract =
  let contractStr = show contract
      parsed = parseContract contractStr
  in case parsed of
       Right parsedContract ->
         counterexample
           (  "Original: " ++ contractStr
           ++ "\nParsed: " ++ show parsedContract
           )
           (parsedContract == contract)
       Left err ->
         counterexample
           (  "Failed to parse contract: " ++ contractStr
           ++ "\nError: " ++ err
           )
           False

-- Double negation should be a no-op: Give (Give c) = c
prop_doubleNegation :: Contract -> Bool
prop_doubleNegation c =
  optimiseContract (Give (Give c)) == optimiseContract c

-- Optimizing twice should yield the same result as optimizing once
prop_optimise_idempotent :: Contract -> Property
prop_optimise_idempotent c =
  let first  = optimiseContract c
      second = optimiseContract first
  in counterexample
       (  "optimiseContract once:\n"  ++ show first
       ++ "\noptimiseContract twice:\n" ++ show second
       )
       (first == second)


-- Distributive property: Scale x (c1 `Or` c2) = (Scale x c1) `Or` (Scale x c2)
prop_optLayerPreservesMeanign :: Contract -> Property
prop_optLayerPreservesMeanign c =
  within 1000000 $ evalTest modelWithMonthStep c False ≈ evalTest modelWithMonthStep c True

-- Distributive property: Scale x (c1 `Or` c2) = (Scale x c1) `Or` (Scale x c2)
prop_orScaleDistributive :: NonNegative Double -> Contract -> Contract -> Property
prop_orScaleDistributive (NonNegative x) c1 c2 =
  within 1000000 $ 
    let left  = Scale (Konst x) (c1 `Or` c2)
        right = (Scale (Konst x) c1) `Or` (Scale (Konst x) c2)
    in evalTest modelWithMonthStep left False ≈ evalTest modelWithMonthStep right False

-- Give c should negate the contract value
prop_give_negates :: Contract -> Property
prop_give_negates c =
  within 1000000 $
    let left  = eval modelWithMonthStep (Give c)
        right = negate <$> eval modelWithMonthStep c
    in left ≈ right

-- And c1 c2 should add their values
prop_and_adds :: Contract -> Contract -> Property
prop_and_adds c1 c2 =
  within 1000000 $
    let left  = eval modelWithMonthStep (And c1 c2)
        right = liftA2 (+) (eval modelWithMonthStep c1) (eval modelWithMonthStep c2)
    in left ≈ right

-- Or c1 c2 should return the max value at each step
prop_or_max :: Contract -> Contract -> Property
prop_or_max c1 c2 =
  within 1000000 $
    let left  = eval modelWithMonthStep (Or c1 c2)
        right = liftA2 maximumValToday (eval modelWithMonthStep c1) (eval modelWithMonthStep c2)
    in left ≈ right

----------------------------------------------------------------
-- Basic HUnit tests
----------------------------------------------------------------

unit_test_PR_structure :: Assertion
unit_test_PR_structure = 
  let result = eval modelWithMonthStep (AcquireOn (date "20-04-2025") (One GBP))
  in case result of
       Left err ->
         assertFailure ("eval returned Left: " ++ err)
       Right (PR layers) ->
         length (layers !! 3) @?= 4

unit_test_acquireOnStartDate :: Assertion
unit_test_acquireOnStartDate = 
  let leftE        = eval modelWithMonthStep (AcquireOn (startDate modelWithMonthStep) (One USD))
      rightE       = exchange modelWithMonthStep USD
  in assertPRApproxEqual "AcquireOnStartDate" leftE rightE

----------------------------------------------------------------
-- HUnit tests for options
----------------------------------------------------------------

unit_test_profitable_european :: Assertion
unit_test_profitable_european = 
  let contract   = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100
      underlying = AcquireOn (date "01-03-2025") ((Scale (StockPrice DIS) (One GBP)) `And` Give (Scale (Konst 100) (One GBP)))
      leftE  = eval modelWithMonthStep contract
      rightE = eval modelWithMonthStep underlying
  in assertPRApproxEqual "ProfitableEuropean" leftE rightE

unit_test_unprofitable_european :: Assertion
unit_test_unprofitable_european = 
  let contract = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 130
      underlying = AcquireOn (date "01-03-2025") None
      leftE  = eval modelWithMonthStep contract
      rightE = eval modelWithMonthStep underlying
  in assertPRApproxEqual "UnprofitableEuropean" leftE rightE

unit_test_profitable_american :: Assertion
unit_test_profitable_american = 
  let contract   = american (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 
      underlying = AcquireOnBefore (date "01-03-2025") ((Scale (StockPrice DIS) (One GBP)) `And` Give (Scale (Konst 100) (One GBP)))
      leftE  = eval modelWithMonthStep contract
      rightE = eval modelWithMonthStep underlying
  in assertPRApproxEqual "ProfitableAmerican" leftE rightE

unit_test_unprofitable_american :: Assertion
unit_test_unprofitable_american = 
  let contract = american (date "05-02-2025") (Scale (StockPrice DIS) (One GBP)) 120
      underlying = AcquireOn (date "05-02-2025") None
      leftE  = eval modelWithMonthStep contract
      rightE = eval modelWithMonthStep underlying
  in assertPRApproxEqual "UnprofitableAmerican" leftE rightE


unit_test_american_vs_eu :: Assertion
unit_test_american_vs_eu = 
  let europeanC      = AcquireOn (date "05-06-2025") (Scale (StockPrice RACE) (One GBP))
      americanC      = AcquireOnBefore (date "05-06-2025") (Scale (StockPrice RACE) (One GBP))
      europeanResult = eval modelWithMonthStep europeanC
      americanResult = eval modelWithMonthStep americanC
  in case (europeanResult, americanResult) of
       (Right (PR europeanLayers), Right (PR americanLayers)) ->
         let europeanValue = sum (map sum europeanLayers)
             americanValue = sum (map sum americanLayers)
         in assertBool
              ("Expected American value to be greater than European value, but got: "
               ++ "European = " ++ show europeanValue ++ ", American = " ++ show americanValue)
              (americanValue > europeanValue)
       (Left err, _) -> assertFailure ("European evaluation failed: " ++ err)
       (_, Left err) -> assertFailure ("American evaluation failed: " ++ err)

----------------------------------------------------------------
-- HUnit tests for invalid contracts
----------------------------------------------------------------

-- Trying to acquire an expired contract should fail
unit_test_expired_c_acquisition :: Assertion
unit_test_expired_c_acquisition = 
  let contract = AcquireOn (date "05-02-2020") (One GBP)
      result   = eval modelWithMonthStep contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

-- Trying to acquire a contract with expired branch should fail
unit_test_expired_c_acquisition2 :: Assertion
unit_test_expired_c_acquisition2 = 
  let contract = AcquireOn (date "05-02-2030") (One GBP) `And` AcquireOn (date "05-02-2020") (One GBP)
      result   = eval modelWithMonthStep contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

-- Using an unsupported currency should fail
unit_test_unsupported_currency :: Assertion
unit_test_unsupported_currency = 
  let contract = AcquireOn (date "05-02-2025") (One BGN)
      result   = eval modelWithMonthStep contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

unit_test_unsupported_stock :: Assertion
unit_test_unsupported_stock = 
  let contract = AcquireOn (date "05-02-2025") ((Scale (StockPrice MSFT)) (One GBP))
      result   = eval modelWithMonthStep contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

----------------------------------------------------------------
-- Edge Cases and Expected Behaviour
----------------------------------------------------------------

-- Expired nested part should evaluate to 0
unit_test_expired_nested_part_evals_to_zero :: Assertion
unit_test_expired_nested_part_evals_to_zero = 
  let not_expired_nested  = Scale (Konst 1000) (One GBP) 
      expired_nested      = AcquireOn (date "05-02-2025") $ Scale (Konst 2) (One GBP) 
      contract            = AcquireOn (date "05-02-2030") (not_expired_nested `And` expired_nested)
      expected_result     = eval modelWithMonthStep (AcquireOn (date "05-02-2030") not_expired_nested)
      result              = eval modelWithMonthStep contract
  in assertPRApproxEqual "ExpiredNestedPartEvalsToZero" expected_result result

unit_test_american_allows_expired_underlying :: Assertion
unit_test_american_allows_expired_underlying = case (evalResult1, evalResult2) of
  (Right (PR pr1), Right (PR pr2)) -> 
    assertBool "Combined contract should have higher value" (pr1 > pr2)
  (Left err, _) -> assertFailure $ "Eval error: " ++ err
  (_, Left err) -> assertFailure $ "Eval error: " ++ err
  where
      not_expired_nested  = (One GBP) 
      expired_nested      = AcquireOn (date "05-02-2025") $ Scale (Konst 2000) (One GBP) 
      contract            = AcquireOnBefore (date "05-02-2030") (not_expired_nested `And` expired_nested)
      evalResult1         = eval modelWithMonthStep contract
      evalResult2         = eval modelWithMonthStep (AcquireOnBefore (date "05-02-2030") not_expired_nested)

-- Summation works as expexted
unit_test_observable_summation :: Assertion
unit_test_observable_summation = 
  let c_no_summation      = AcquireOn (date "05-02-2030") $ Scale (Konst 1200) (One GBP) 
      c_summation         = AcquireOn (date "05-02-2030") $ Scale (Konst 1000 + Konst 200) (One GBP) 
      expected_result     = eval modelWithMonthStep c_no_summation
      result              = eval modelWithMonthStep c_summation
  in assertPRApproxEqual "ExpiredNestedPartEvalsToZero" expected_result result

-- Compound contract of 2 contracts takes the expiry date of the bigger one
unit_test_and_takes_later_expiry :: Assertion
unit_test_and_takes_later_expiry = case (evalResult1, evalResult2) of
  (Right (PR pr1), Right (PR pr2)) -> 
    assertEqual "Step count mismatch" (length pr2) (length pr1)
  (Left err, _) -> assertFailure $ "Eval error: " ++ err
  (_, Left err) -> assertFailure $ "Eval error: " ++ err
  where
    c1 = AcquireOn (date "05-02-2025") (One GBP)
    c2 = AcquireOn (date "05-02-2030") (One GBP)
    combined = And c1 c2
    evalResult1 = eval modelWithMonthStep combined
    evalResult2 = eval modelWithMonthStep c2
