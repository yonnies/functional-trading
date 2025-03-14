{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.QuickCheck
import Data.Time (addDays)
import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit
import Control.Applicative (liftA2)
import Text.Printf (printf)
import Control.Monad.State

import ContractsDSL 
import ModelUtils
import EvaluationEngine

-------------------------------------------------
-- Contract generation rules 
-------------------------------------------------

-- Arbitrary instance for generating random contracts
instance Arbitrary Contract where
  arbitrary = sized genContract 

-- Generate random contracts with a size limit
genContract :: Int -> Gen Contract
genContract 0 = frequency
  [ (1, pure None)
  , (8, One <$> genRandomCurrency)
  ]
genContract n = oneof
  [ Give <$> genContract (n `div` 2)
  , do
      leftSize <- choose (0, n `div` 2)
      c1 <- genContract leftSize
      c2 <- genContract (n `div` 2 - leftSize)
      oneof [ pure (And c1 c2), pure (Or c1 c2) ]
  , do
      c <- genContract (n `div` 2)
      someDate <- genRandomDate
      oneof [ pure (AcquireOn someDate c), pure (AcquireOnBefore someDate c) ]
  , do
      c <- genContract (n `div` 2)
      obs <- genObsDouble
      pure (Scale obs c)
  , do
      c <- genContract (n `div` 2)
      obs <- genObsBool
      pure (AcquireWhen obs c)
  ]

-- Generate random numeric observables
genObsDouble :: Gen (Obs Double)
genObsDouble = sized $ \n -> 
  if n <= 0 then baseCase else frequency
    [ (4, baseCase)
    , (2, LiftD <$> genUnaryOp <*> resize (n-1) genObsDouble)
    , (2, Lift2D <$> genBinaryOp <*> resize (n `div` 2) genObsDouble <*> resize (n `div` 2) genObsDouble)
    ]
  where
    baseCase = frequency
      [ (3, Konst <$> arbitrary)
      , (2, StockPrice <$> genRandomStock)
      ]

-- Generate random boolean observables
genObsBool :: Gen (Obs Bool)
genObsBool = Lift2B <$> genCompareOp <*> genObsDouble <*> genObsDouble

-- Generate random operators for expressions
genUnaryOp :: Gen UnaryOp
genUnaryOp = elements [UNegate]

genBinaryOp :: Gen BinaryOp
genBinaryOp = elements [BAdd, BSub, BMul]

genCompareOp :: Gen CompareOp
genCompareOp = elements [CLT, CLE, CEQ, CGE, CGT]

-- Generate random currencies and stocks
genRandomCurrency :: Gen Currency
genRandomCurrency = elements [GBP, USD, EUR]

genRandomStock :: Gen Stock
genRandomStock = elements [DIS, TSLA, NVDA]

-- Generate a random date in a fixed range
genRandomDate :: Gen Date
genRandomDate = do
  let startDay = today
      endDay   = date "31-12-2030"
  offset <- choose (0, daysBetween startDay endDay)
  pure (addDays (toInteger offset) startDay)

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
        property (prApproxEq 1e-6 pr1 pr2)
    (Left err1, Left err2) -> 
        counterexample (printf "Both failed with errors:\n%s\n%s" err1 err2) True
    (Left err1, Right _) -> 
        counterexample ("Mismatch: First failed with error: " ++ err1) False
    (Right _, Left err2) -> 
        counterexample ("Mismatch: Second failed with error: " ++ err2) False


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
  _ <- typeCheck c (startDate model)
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
    in eval model left ≈ eval model right
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
-- HUnit tests for optiona
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

-- Using an unsupported currency should fail
unit_test_unsupported_currency :: Assertion
unit_test_unsupported_currency = 
  let model    = exampleModel today 30
      contract = AcquireOn (date "05-02-2025") (One BGN)
      result   = eval model contract
  in case result of
       Left _    -> return ()  -- Expected failure, test passes
       Right pr  -> assertFailure ("Expected failure but got: " ++ show pr)

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
      let eps  = 1e-6
          same = prApproxEq eps prL prR
      in assertBool
           (  testName 
           ++ " mismatch:\nLeft:\n"     ++ show prL
           ++ "\nRight:\n"             ++ show prR 
           )
           same

----------------------------------------------------------------
-- Running all tests
----------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "All Tests"
  [ testGroup "Structural (no eval needed)"
      [ testProperty "doubleNegation"         prop_doubleNegation
      , testProperty "optimiseIdempotent"     prop_optimise_idempotent
      ]
  , testGroup "Valuation properties"
      [ testProperty "prop_optLayerPreservesMeanign" prop_optLayerPreservesMeanign
      , testProperty "orScaleDistributive"    prop_orScaleDistributive
      , testProperty "giveNegates"            prop_give_negates
      , testProperty "andAdds"                prop_and_adds
      , testProperty "orMax"                  prop_or_max
      ]
  , testGroup "HUnit tests"
      [ testCase "test_PR_structure"          unit_test_PR_structure
      , testCase "test_acquireOnStartDate"    unit_test_acquireOnStartDate
      , testCase "test_profitable_european"   unit_test_profitable_european
      , testCase "test_unprofitable_european" unit_test_unprofitable_european
      , testCase "test_profitable_american"   unit_test_profitable_american
      , testCase "test_unprofitable_american" unit_test_unprofitable_american
      , testCase "test_expired_c_acquisition" unit_test_expired_c_acquisition
      , testCase "test_unsupported_currency"  unit_test_unsupported_currency
      ]
  ]

