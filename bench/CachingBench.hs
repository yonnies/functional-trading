{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Test.QuickCheck

import ContractsDSL
import ValuationEngine         -- Original version with caching
import ValuationEngineNoCache  -- Version without caching
import ModelUtils
import ContractDefinitions

european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = 
  AcquireOn t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOn t None 

--------------------------------------------------------
-- Contracts without repetition
--------------------------------------------------------

simpleBond :: Contract
simpleBond = zcdb (date "06-11-2028") 1000 GBP

simpleOpt :: Contract
simpleOpt = european (date "01-03-2035") (Scale (StockPrice DIS) (One GBP)) 100

complexNonRepetitiveContract :: Contract
complexNonRepetitiveContract =
  AcquireOn (date "01-01-2030") $
    And
      (AcquireOnBefore (date "01-01-2035") $
        Scale (StockPrice TSLA - Konst 100) (One USD)
      )
      (Or
        (Give $ european (date "01-06-2030") (Scale (StockPrice NVDA) (One EUR)) 200)
        (AcquireOnBefore (date "01-01-2035") $
          And
            (zcdb (date "01-06-2035") 500 GBP)
            (Scale (Konst 0.5) (One USD))
        )
      )

--------------------------------------------------------
-- Contracts with repetition
--------------------------------------------------------

repetitiveAndContract :: Contract
repetitiveAndContract =
    let sharedSubcontract = Scale (StockPrice DIS) (One GBP)
    in AcquireOn (date "06-11-2035") 
        (And (And sharedSubcontract sharedSubcontract)
            (And (And sharedSubcontract sharedSubcontract)
                  (And sharedSubcontract sharedSubcontract)))

repetitiveScaleContract :: Contract
repetitiveScaleContract =
    let obs1 = StockPrice DIS + Konst 10
        obs2 = StockPrice TSLA * Konst 2
        sharedScale = Scale obs1 (Scale obs2 (One USD))
    in  AcquireOn (date "06-11-2035")  
          (And sharedScale (And sharedScale sharedScale))

deepNestedContract :: Contract
deepNestedContract =
    let sub1 = Scale (StockPrice DIS + Konst 5) (One EUR)
        sub2 = Scale (StockPrice NVDA * Konst 3) (One USD)
    in AcquireOn (date "06-11-2035")
        (Or sub1 (And sub2 (Or sub1 (And sub1 sub2))))

multipleNestedAcquisitions :: Contract
multipleNestedAcquisitions =
    let sub1 = AcquireOn (date "06-11-2035") 
                (Scale (StockPrice DIS + Konst 5) (One EUR))
        sub2 = AcquireOn (date "06-11-2035")
                (Give $ Scale (StockPrice NVDA * Konst 3) (One USD))
    in AcquireOn (date "06-11-2030")
        (Or sub1 (And sub2 (Or sub1 (And sub1 sub2))))

deeplyRedundantContract :: Int -> Contract
deeplyRedundantContract depth =
  let expensiveObs = foldl1 (Lift2D BAdd) (replicate 100 (StockPrice DIS * StockPrice TSLA))
      sharedSub = Scale expensiveObs (One USD)
      buildTree 0 = sharedSub
      buildTree n = And (buildTree (n-1)) (buildTree (n-1))
  in AcquireOn (date "01-01-2030") (buildTree depth)

multipleNestedOptionsContract :: Contract
multipleNestedOptionsContract =
  let
      -- Repeated sub-contracts
      option1 = european (date "01-01-2035") (One EUR) 1.15
      option2 = european (date "01-01-2035") (Scale (StockPrice NVDA) (One USD)) 200

      -- Not repeated
      option3 = european (date "01-01-2030") (Scale (StockPrice DIS) (One GBP)) 50
      
      -- Nested structure with repetition
      tree1 = And (Give option1) (Or option1 option2)
      tree2 = And (AcquireOn (date "01-01-2027") tree1) option3
      tree3 = Or tree2 (And tree1 tree2)
  in AcquireOn (date "01-01-2026") tree3


--------------------------------------------------------
-- Benchmark main
--------------------------------------------------------

main :: IO ()
main = do
  let today = date "01-11-2024"
  let model = exampleModel today 30
  randomContract10 <- generate (resize 10 arbitrary)
  randomContract50 <- generate (resize 50 arbitrary)
  randomContract100 <- generate (resize 100 arbitrary)

  defaultMain
    [ bgroup "Contracts without repetition"
      [ bgroup "Zero-Coupon Bond"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) simpleBond
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) simpleBond
        ]
      , bgroup "European Option"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) simpleOpt
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) simpleOpt
        ]
      , bgroup "Complex Example"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) complexNonRepetitiveContract
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) complexNonRepetitiveContract
        ]
      ]
    , bgroup "Contracts with repetition"
      [
        bgroup "Repetitive And Contract"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) repetitiveAndContract
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) repetitiveAndContract
        ]
      , bgroup "Repetitive Scale Contract"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) repetitiveScaleContract
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) repetitiveScaleContract
        ]
      , bgroup "Deeply Nested Contract"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) deepNestedContract
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) deepNestedContract
        ]
      , bgroup "Multiple Nested Acquisitions"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) multipleNestedAcquisitions
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) multipleNestedAcquisitions
        ]
      , bgroup "Deeply Redundant Contract"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) (deeplyRedundantContract 5)
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) (deeplyRedundantContract 5)
        ]
      , bgroup "Multiple Nested Options Contract"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) multipleNestedOptionsContract
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) multipleNestedOptionsContract
        ]
      ]
    , bgroup "Random Contracts"
      [ bgroup "Size 10"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) randomContract10
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) randomContract10
        ]
      , bgroup "Size 50"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) randomContract50
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) randomContract50
        ]
      , bgroup "Size 100"
        [ bench "With Cache"    $ nf (ValuationEngine.eval model) randomContract100
        , bench "Without Cache" $ nf (ValuationEngineNoCache.eval model) randomContract100
        ]
      ]
    ]
