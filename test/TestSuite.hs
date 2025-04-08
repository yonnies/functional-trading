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

----------------------------------------------------------------
-- QuickCheck properties
----------------------------------------------------------------

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

evalTest :: Model -> Contract -> Bool -> Either String (PR Double)
evalTest model c optContract= do
  _ <- validityCheck c (startDate model)
  let optC = if optContract then optimiseContract c else c
  (result, _finalState) <- runStateT (evalC model optC (startDate model)) emptyEvalState
  return result

-- Distributive property: Scale x (c1 `Or` c2) = (Scale x c1) `Or` (Scale x c2)
prop_optLayerPreservesMeanign :: Contract -> Property
prop_optLayerPreservesMeanign c =
  within 1000000 $ evalTest model c False ≈ evalTest model c True
  where
    model = exampleModel today 30

-- Distributive property: Scale x (c1 `Or` c2) = (Scale x c1) `Or` (Scale x c2)
prop_orScaleDistributive :: NonNegative Double -> Contract -> Contract -> Property
prop_orScaleDistributive (NonNegative x) c1 c2 =
  within 1000000 $ 
    let left  = Scale (Konst x) (c1 `Or` c2)
        right = (Scale (Konst x) c1) `Or` (Scale (Konst x) c2)
    in evalTest model left False ≈ evalTest model right False
  where
    model = exampleModel today 30

-- Give c should negate the contract value
prop_give_negates :: Contract -> Property
prop_give_negates c =
  within 1000000 $
    let left  = eval model (Give c)
        right = negate <$> eval model c
    in left ≈ right
  where 
    model = exampleModel today 30

-- And c1 c2 should add their values
prop_and_adds :: Contract -> Contract -> Property
prop_and_adds c1 c2 =
  within 1000000 $
    let left  = eval model (And c1 c2)
        right = liftA2 (+) (eval model c1) (eval model c2)
    in left ≈ right
  where
    model = exampleModel today 30

-- Or c1 c2 should return the max value at each step
prop_or_max :: Contract -> Contract -> Property
prop_or_max c1 c2 =
  within 1000000 $
    let left  = eval model (Or c1 c2)
        right = liftA2 maximumValToday (eval model c1) (eval model c2)
    in left ≈ right
  where
    model = exampleModel today 30

----------------------------------------------------------------
-- Basic HUnit tests
----------------------------------------------------------------

unit_test_PR_structure :: Assertion
unit_test_PR_structure = 
  let model  = exampleModel today 30
      result = eval model (AcquireOn (date "20-04-2025") (One GBP))
  in case result of
       Left err ->
         assertFailure ("eval returned Left: " ++ err)
       Right (PR layers) ->
         length (layers !! 3) @?= 4

unit_test_acquireOnStartDate :: Assertion
unit_test_acquireOnStartDate = 
  let model        = exampleModel today 30
      leftE        = eval model (AcquireOn (startDate model) (One USD))
      rightE       = exchange model USD
  in assertPRApproxEqual "AcquireOnStartDate" leftE rightE

----------------------------------------------------------------
-- HUnit tests for options
----------------------------------------------------------------

european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = 
  AcquireOn t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOn t None 

unit_test_profitable_european :: Assertion
unit_test_profitable_european = 
  let model      = exampleModel today 30
      contract   = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100
      underlying = AcquireOn (date "01-03-2025") ((Scale (StockPrice DIS) (One GBP)) `And` Give (Scale (Konst 100) (One GBP)))
      leftE  = eval model contract
      rightE = eval model underlying
  in assertPRApproxEqual "ProfitableEuropean" leftE rightE

unit_test_unprofitable_european :: Assertion
unit_test_unprofitable_european = 
  let model      = exampleModel today 30
      contract   = european (date "01-03-2025") 
                            (Scale (StockPrice DIS) (One GBP)) 
                            130
      underlying = AcquireOn (date "01-03-2025") None
      leftE  = eval model contract
      rightE = eval model underlying
  in assertPRApproxEqual "UnprofitableEuropean" leftE rightE

american :: Date -> Contract -> Double -> Contract
american t underlying strikePrice = 
  AcquireOnBefore t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOnBefore t None

unit_test_profitable_american :: Assertion
unit_test_profitable_american = 
  let model      = exampleModel today 30
      contract   = american (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100 
      underlying = AcquireOnBefore (date "01-03-2025") ((Scale (StockPrice DIS) (One GBP)) `And` Give (Scale (Konst 100) (One GBP)))
      leftE  = eval model contract
      rightE = eval model underlying
  in assertPRApproxEqual "ProfitableAmerican" leftE rightE

unit_test_unprofitable_american :: Assertion
unit_test_unprofitable_american = 
  let model      = exampleModel today 30
      contract   = american (date "05-02-2025") 
                            (Scale (StockPrice DIS) (One GBP)) 
                            120
      underlying = AcquireOn (date "05-02-2025") None
      leftE  = eval model contract
      rightE = eval model underlying
  in assertPRApproxEqual "UnprofitableAmerican" leftE rightE


unit_test_american_vs_eu :: Assertion
unit_test_american_vs_eu = 
  let model          = exampleModel today 30
      europeanC      = AcquireOn (date "05-06-2025") (Scale (stockPrice RACE) (One GBP))
      americanC      = AcquireOnBefore (date "05-06-2025") (Scale (stockPrice RACE) (One GBP))
      europeanResult = eval model europeanC
      americanResult = eval model americanC
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
  let model    = exampleModel today 30
      contract = AcquireOn (date "05-02-2020") (One GBP)
      result   = eval model contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

-- Trying to acquire a contract with expired branch should fail
unit_test_expired_c_acquisition2 :: Assertion
unit_test_expired_c_acquisition2 = 
  let model    = exampleModel today 30
      contract = AcquireOn (date "05-02-2030") (One GBP) `And` AcquireOn (date "05-02-2020") (One GBP)
      result   = eval model contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

-- Using an unsupported currency should fail
unit_test_unsupported_currency :: Assertion
unit_test_unsupported_currency = 
  let model    = exampleModel today 30
      contract = AcquireOn (date "05-02-2025") (One BGN)
      result   = eval model contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

unit_test_unsupported_stock :: Assertion
unit_test_unsupported_stock = 
  let model    = exampleModel today 30
      contract = AcquireOn (date "05-02-2025") ((Scale (stockPrice MSFT)) (One GBP))
      result   = eval model contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)


----------------------------------------------------------------
-- Edge Cases and Expected Behaviour
----------------------------------------------------------------

-- Expired nested part should evaluate to 0
unit_test_expired_nested_part_evals_to_zero :: Assertion
unit_test_expired_nested_part_evals_to_zero = 
  let model               = exampleModel today 30
      not_expired_nested  = Scale (Konst 1000) (One GBP) 
      expired_nested      = AcquireOn (date "05-02-2025") $ Scale (Konst 2) (One GBP) 
      contract            = AcquireOn (date "05-02-2030") (not_expired_nested `And` expired_nested)
      expected_result     = eval model (AcquireOn (date "05-02-2030") not_expired_nested)
      result              = eval model contract
  in assertPRApproxEqual "ExpiredNestedPartEvalsToZero" expected_result result

unit_test_american_allows_expired_underlying :: Assertion
unit_test_american_allows_expired_underlying = case (evalResult1, evalResult2) of
  (Right (PR pr1), Right (PR pr2)) -> 
    assertBool "Combined contract should have higher value" (pr1 > pr2)
  (Left err, _) -> assertFailure $ "Eval error: " ++ err
  (_, Left err) -> assertFailure $ "Eval error: " ++ err
  where
      model               = exampleModel today 30
      not_expired_nested  = (One GBP) 
      expired_nested      = AcquireOn (date "05-02-2025") $ Scale (Konst 2000) (One GBP) 
      contract            = AcquireOnBefore (date "05-02-2030") (not_expired_nested `And` expired_nested)
      evalResult1         = eval model contract
      evalResult2         = eval model (AcquireOnBefore (date "05-02-2030") not_expired_nested)

-- Summation works as expexted
unit_test_observable_summation :: Assertion
unit_test_observable_summation = 
  let model               = exampleModel today 30
      c_no_summation      = AcquireOn (date "05-02-2030") $ Scale (Konst 1200) (One GBP) 
      c_summation         = AcquireOn (date "05-02-2030") $ Scale (Konst 1000 + Konst 200) (One GBP) 
      expected_result     = eval model c_no_summation
      result              = eval model c_summation
  in assertPRApproxEqual "ExpiredNestedPartEvalsToZero" expected_result result

-- Compound contract of 2 contracts takes the expiry date of the bigger one
unit_test_and_takes_later_expiry :: Assertion
unit_test_and_takes_later_expiry = case (evalResult1, evalResult2) of
  (Right (PR pr1), Right (PR pr2)) -> 
    assertEqual "Step count mismatch" (length pr2) (length pr1)
  (Left err, _) -> assertFailure $ "Eval error: " ++ err
  (_, Left err) -> assertFailure $ "Eval error: " ++ err
  where
    model = exampleModel today 30
    c1 = AcquireOn (date "05-02-2025") (One GBP)
    c2 = AcquireOn (date "05-02-2030") (One GBP)
    combined = And c1 c2
    evalResult1 = eval model combined
    evalResult2 = eval model c2

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
-- Running all tests
----------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testGroup "Structural (no eval needed)"
      [ testProperty "doubleNegation"           prop_doubleNegation
      , testProperty "optimiseIdempotent"       prop_optimise_idempotent
      ]
  , testGroup "Valuation properties"
      [ testProperty "prop_optLayerPreservesMeanign" prop_optLayerPreservesMeanign
      , testProperty "orScaleDistributive"      prop_orScaleDistributive
      , testProperty "giveNegates"              prop_give_negates
      , testProperty "andAdds"                  prop_and_adds
      , testProperty "orMax"                    prop_or_max
      ]
  , testGroup "HUnit tests"
      [ testCase "test_PR_structure"            unit_test_PR_structure
      , testCase "test_acquireOnStartDate"      unit_test_acquireOnStartDate
      , testCase "test_profitable_european"     unit_test_profitable_european
      , testCase "test_unprofitable_european"   unit_test_unprofitable_european
      , testCase "test_profitable_american"     unit_test_profitable_american
      , testCase "test_unprofitable_american"   unit_test_unprofitable_american
      , testCase "test_american_vs_eu"          unit_test_american_vs_eu
      , testCase "test_expired_c_acquisition"   unit_test_expired_c_acquisition
      , testCase "test_expired_c_acquisition2"  unit_test_expired_c_acquisition2
      , testCase "test_unsupported_currency"    unit_test_unsupported_currency
      , testCase "test_unsupported_stock"       unit_test_unsupported_stock
      , testCase "test_expired_nested_part_evals_to_zero" unit_test_expired_nested_part_evals_to_zero
      , testCase "test_american_allows_expired_underlying" unit_test_american_allows_expired_underlying
      , testCase "test_observable_summation"    unit_test_observable_summation
      , testCase "test_and_takes_later_expiry"  unit_test_and_takes_later_expiry
      ]
  ]