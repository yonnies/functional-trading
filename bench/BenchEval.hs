{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import ContractsDSL
import EvaluationEngine         -- Original version with caching
import EvaluationEngineNoCache  -- Version without caching
import ModelUtils

--------------------------------------------------------
-- Simple Contracts without repetition
--------------------------------------------------------
zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = 
  AcquireOn t (underlying `And` Give (Scale (Konst strikePrice) (One GBP))) 
  `Or` AcquireOn t None 

simpleBond, payAndGive, opt :: Contract
simpleBond  = zcdb (date "06-11-2028") 10 GBP
payAndGive  = And (zcdb (date "06-11-2028") 10 GBP) (Give (Scale (Konst 5) (One GBP)))
opt         = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100

--------------------------------------------------------
-- Contracts with repetition
--------------------------------------------------------

repetitiveAndContract :: Contract
repetitiveAndContract =
    let sharedSubcontract = Scale (konst 2) (one GBP)
    in and_ (and_ sharedSubcontract sharedSubcontract)
            (and_ (and_ sharedSubcontract sharedSubcontract)
                  (and_ sharedSubcontract sharedSubcontract))

repetitiveScaleContract :: Contract
repetitiveScaleContract =
    let obs1 = stockPrice DIS + konst 10
        obs2 = stockPrice TSLA * konst 2
        sharedScale = Scale obs1 (Scale obs2 (one USD))
    in and_ sharedScale (and_ sharedScale sharedScale)

repetitiveOptionContract :: Contract
repetitiveOptionContract =
    let strike = 100
        underlying = stockPrice NVDA
        condition = underlying %> konst strike
        payoff = Scale (underlying - konst strike) (one EUR)
        option = acquireWhen condition payoff
    in or_ option (and_ option option)  -- Reuse the same option

recursiveRepetitiveContract :: Int -> Contract
recursiveRepetitiveContract n
    | n <= 0 = one GBP
    | otherwise = 
        let shared = recursiveRepetitiveContract (n-1)
        in and_ shared (and_ shared shared)  -- Exponential repetition



--------------------------------------------------------
-- Benchmark main
--------------------------------------------------------

main :: IO ()
main = do
  let model = exampleModel today 30
  -- randomContract10 <- generate (resize 10 arbitrary)
  -- randomContract50 <- generate (resize 50 arbitrary)

  defaultMain
    [ 
    -- bgroup "Contracts with NO repetition"
    --   [ bgroup "Zero-Coupon Bond"
    --     [ bench "With Cache"    $ nf (EvaluationEngine.eval model) simpleBond
    --     , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) simpleBond
    --     ]
    --   , bgroup "Pay and Give"
    --     [ bench "With Cache"    $ nf (EvaluationEngine.eval model) payAndGive
    --     , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) payAndGive
    --     ]
    --   , bgroup "European Option"
    --     [ bench "With Cache"    $ nf (EvaluationEngine.eval model) opt
    --     , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) opt
    --     ]
    --   ]
    -- , 
    bgroup "Contracts with repetition"
      [ bgroup "Repetitive And Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) repetitiveAndContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) repetitiveAndContract
        ]
      , bgroup "Repetitive Scale Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) repetitiveScaleContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) repetitiveScaleContract
        ]
      , bgroup "Repetitive Option Contract"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) repetitiveOptionContract
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) repetitiveOptionContract
        ]
      , bgroup "Recursive Repetitive Contract (Depth 5)"
        [ bench "With Cache"    $ nf (EvaluationEngine.eval model) (recursiveRepetitiveContract 5)
        , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) (recursiveRepetitiveContract 5)
        ]
      ]
      

    -- , bgroup "Random Contracts"
    --   [ bgroup "Size 10"
    --     [ bench "With Cache"    $ nf (EvaluationEngine.eval model) randomContract10
    --     , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) randomContract10
    --     ]
    --   , bgroup "Size 50"
    --     [ bench "With Cache"    $ nf (EvaluationEngine.eval model) randomContract50
    --     , bench "Without Cache" $ nf (EvaluationEngineNoCache.eval model) randomContract50
    --     ]
    --   ]
    ]
