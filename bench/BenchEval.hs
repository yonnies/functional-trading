{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Test.QuickCheck

import ContractsDSL
import EvaluationEngine         -- Original version with caching
import EvaluationEngineNoCache  -- Version without caching
import ModelUtils

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = 
  AcquireOn t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOn t None 

--------------------------------------------------------
-- Contracts without repetition
--------------------------------------------------------

simpleBond :: Contract
simpleBond = zcdb (date "06-11-2028") 1000 GBP

opt :: Contract
opt = european (date "01-03-2035") (Scale (StockPrice DIS) (One GBP)) 100

complexNonRepetitiveContract :: Contract
complexNonRepetitiveContract =
  AcquireOn (date "01-01-2030") $
    And
      (AcquireOnBefore (date "01-01-2035") $
        Scale (stockPrice TSLA - konst 100) (One USD)
      )
      (Or
        (Give $ european (date "01-06-2030") (Scale (stockPrice NVDA) (One EUR)) 200)
        (AcquireOnBefore (date "01-01-2035") $
          And
            (zcdb (date "01-06-2035") 500 GBP)
            (Scale (konst 0.5) (One USD))
        )
      )

--------------------------------------------------------
-- Contracts with repetition
--------------------------------------------------------

repetitiveAndContract :: Contract
repetitiveAndContract =
    let sharedSubcontract = Scale (stockPrice DIS) (one GBP)
    in AcquireOn (date "06-11-2035") 
        (and_ (and_ sharedSubcontract sharedSubcontract)
            (and_ (and_ sharedSubcontract sharedSubcontract)
                  (and_ sharedSubcontract sharedSubcontract)))

repetitiveScaleContract :: Contract
repetitiveScaleContract =
    let obs1 = stockPrice DIS + konst 10
        obs2 = stockPrice TSLA * konst 2
        sharedScale = Scale obs1 (Scale obs2 (one USD))
    in  AcquireOn (date "06-11-2035")  
          (and_ sharedScale (and_ sharedScale sharedScale))

deepNestedContract :: Contract
deepNestedContract =
    let sub1 = Scale (stockPrice DIS + konst 5) (one EUR)
        sub2 = Scale (stockPrice NVDA * konst 3) (one USD)
    in AcquireOn (date "06-11-2035")
        (or_ sub1 (and_ sub2 (or_ sub1 (and_ sub1 sub2))))

multipleNestedAcquisitions :: Contract
multipleNestedAcquisitions =
    let sub1 = AcquireOn (date "06-11-2035") 
                (Scale (stockPrice DIS + konst 5) (one EUR))
        sub2 = AcquireOn (date "06-11-2035")
                (Give $ Scale (stockPrice NVDA * konst 3) (one USD))
    in AcquireOn (date "06-11-2030")
        (or_ sub1 (and_ sub2 (or_ sub1 (and_ sub1 sub2))))

deeplyRedundantContract :: Int -> Contract
deeplyRedundantContract depth =
  let expensiveObs = foldl1 (Lift2D BAdd) (replicate 100 (stockPrice DIS * stockPrice TSLA))
      sharedSub = Scale expensiveObs (One USD)
      buildTree 0 = sharedSub
      buildTree n = And (buildTree (n-1)) (buildTree (n-1))
  in AcquireOn (date "01-01-2030") (buildTree depth)

multipleNestedOptionsContract :: Contract
multipleNestedOptionsContract =
  let
      -- Repeated sub-contracts
      option1 = european (date "01-01-2035") (One EUR) 1.15
      option2 = european (date "01-01-2035") (Scale (stockPrice NVDA) (One USD)) 200

      -- Not repeated
      option3 = european (date "01-01-2030") (Scale (stockPrice DIS) (One GBP)) 50
      
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
  let model = exampleModel today 30
  randomContract10 <- generate (resize 10 arbitrary)
  randomContract50 <- generate (resize 50 arbitrary)
  randomContract100 <- generate (resize 100 arbitrary)

  defaultMain
    [ bgroup "Contracts with NO repetition"
      [ bgroup "Zero-Coupon Bond"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) simpleBond
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) simpleBond
        ]
      , bgroup "European Option"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) opt
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) opt
        ]
      , bgroup "Complex Example"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) complexNonRepetitiveContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) complexNonRepetitiveContract
        ]
      ]
    , bgroup "Contracts with repetition"
      [
        bgroup "Repetitive And Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) repetitiveAndContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) repetitiveAndContract
        ]
      , bgroup "Repetitive Scale Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) repetitiveScaleContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) repetitiveScaleContract
        ]
      , bgroup "Deeply Nested Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) deepNestedContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) deepNestedContract
        ]
      , bgroup "Multiple Nested Acquisitions"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) multipleNestedAcquisitions
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) multipleNestedAcquisitions
        ]
      , bgroup "Deeply Redundant Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) (deeplyRedundantContract 5)
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) (deeplyRedundantContract 5)
        ]
      , bgroup "Multiple Nested Options Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) multipleNestedOptionsContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) multipleNestedOptionsContract
        ]
      ]
    , bgroup "Random Contracts"
      [ bgroup "Size 10"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) randomContract10
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) randomContract10
        ]
      , bgroup "Size 50"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) randomContract50
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) randomContract50
        ]
      , bgroup "Size 100"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) randomContract100
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) randomContract100
        ]
      ]
    ]
