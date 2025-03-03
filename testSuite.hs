import Test.QuickCheck
import Data.Time (addDays)

import ContractsDSL  -- Assuming this module defines your contract DSL
import ModelUtils
import EvaluationEngine

instance Arbitrary Contract where
  arbitrary = sized (genContract (date "01-01-2025"))

genContract :: Date -> Int -> Gen Contract
genContract parentDate 0 = frequency
  [ (1, pure None)
  , (8, One <$> genRandomCurrency)
  ]
genContract parentDate n = oneof
  [ Give <$> genContract parentDate (n `div` 2)
  , do
      leftSize <- choose (0, n `div` 2)
      c1 <- genContract parentDate leftSize
      c2 <- genContract parentDate (n `div` 2 - leftSize)
      oneof [ pure (And c1 c2), pure (Or c1 c2) ]
  , do
      d  <- genDateStrictlyAfter parentDate (date "31-12-2030")
      c  <- genContract d (n `div` 2)
      oneof [ pure (AcquireOn d c), pure (AcquireOnBefore d c) ]
  , do
      c <- genContract parentDate (n `div` 2)
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

-------------------------------------------------

prop_doubleNegation :: Contract -> Bool
prop_doubleNegation c =
  optimiseContract (give (give c)) == optimiseContract c

prop_orScaleDistributive :: NonNegative Double -> Contract -> Contract -> Property
prop_orScaleDistributive (NonNegative x) c1 c2 = within 1000000 $
    let left  = Scale (konst x) (c1 `or_` c2)
        right = (Scale (konst x) c1) `or_` (Scale (konst x) c2)
    in eval_contract model left === eval_contract model right
  where
    model = exampleModel today 365

main :: IO ()
main = do
  quickCheck (withMaxSuccess 1000 prop_doubleNegation)
  verboseCheck (withMaxSuccess 100 prop_orScaleDistributive)


