{-# LANGUAGE OverloadedStrings #-}

module BenchEval where

import Criterion.Main
import ContractsDSL
import EvaluationEngine 
import ModelUtils
import Main
import Test.QuickCheck

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

main :: IO ()
main = do
  let model = exampleModel today 30
      simpleBond  = zcdb (date "06-11-2028") 10 GBP
      payAndGive  = And (zcdb (date "06-11-2028") 10 GBP) (Give (Scale (Konst 5) (One GBP)))
      opt         = european (date "01-03-2025") (Scale (StockPrice DIS) (One GBP)) 100

  randomContract10 <- generate (resize 10 arbitrary)
  randomContract50 <- generate (resize 50 arbitrary)

  defaultMain
    [ bgroup "eval_contract-Examples"
      [ bench "Bond (zcdb)" $
          nf (eval model) simpleBond

      , bench "payAndGive" $
          nf (eval model) payAndGive

      , bench "european option" $
          nf (eval model) opt
      ]
    , bgroup "eval_contract-Random"
      [ bench "randomContract-size10" $
          nf (eval model) randomContract10

      , bench "randomContract-size50" $
          nf (eval model) randomContract50
      ]
    ]
