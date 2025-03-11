module TestSuite where

import Test.QuickCheck
import Data.Time (addDays)

import ContractsDSL  -- Assuming this module defines your contract DSL
import ModelUtils
import EvaluationEngine

instance Arbitrary Contract where
  arbitrary = sized genContract 

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
        oneof [ pure (And c1 c2)
              , pure (Or c1 c2)
              ]
    , do
        c <- genContract (n `div` 2)
        someDate <- genRandomDate
        oneof [ pure (AcquireOn someDate c)
              , pure (AcquireOnBefore someDate c)
              ]
    , do
        c <- genContract (n `div` 2)
        obs <- genObs
        pure (Scale obs c)
    ]

genObs :: Gen (Obs Double)
genObs = oneof
    [ Konst <$> arbitrary  
    , StockPrice <$> genRandomStock
    ]

genRandomCurrency :: Gen Currency
genRandomCurrency = elements [GBP, USD, EUR]

genRandomStock :: Gen Stock
genRandomStock = elements [DIS, TSLA, NVDA]

-- d, 1m, 3m, 6m, y - time steps

-- Generate a random Date in a fixed range.
genDateBetween :: Date -> Date -> Gen Date
genDateBetween startDay endDay = do
  let totalDays = daysBetween startDay endDay
  offset <- choose (0, totalDays)
  pure (addDays (toInteger offset) startDay)

genDateStrictlyAfter :: Date -> Date -> Gen Date
genDateStrictlyAfter minDate endDay =
  genDateBetween (addDays 1 minDate) endDay

genRandomDate :: Gen Date
genRandomDate = do
  let startDay = date "01-01-2025"
      endDay   = date "31-12-2030"
      totalDays = daysBetween startDay endDay
  -- Pick an offset within [0 .. totalDays]
  offset <- choose (0, totalDays)
  -- Convert to an actual Day
  pure (addDays (toInteger offset) startDay)

-------------------------------------------------

prop_doubleNegation :: Contract -> Bool
prop_doubleNegation c =
  optimiseContract (give (give c)) == optimiseContract c

prop_orScaleDistributive :: NonNegative Double -> Contract -> Contract -> Property
prop_orScaleDistributive (NonNegative x) c1 c2 = within 1000000 $
    let left  = Scale (konst x) (c1 `or_` c2)
        right = (Scale (konst x) c1) `or_` (Scale (konst x) c2)
    in eval model left === eval model right
  where
    model = exampleModel today 30

main :: IO ()
main = do
  -- quickCheck (withMaxSuccess 1000 prop_doubleNegation)
  verboseCheck (withMaxSuccess 100 prop_orScaleDistributive)


