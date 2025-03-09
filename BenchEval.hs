{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import ContractsDSL
import EvaluationEngine 
import ModelUtils
import TestSuite
import TestEvaluation
import Test.QuickCheck
import System.Random

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
          nf (eval_contract model) simpleBond

      , bench "payAndGive" $
          nf (eval_contract model) payAndGive

      , bench "european option" $
          nf (eval_contract model) opt
      ]
    , bgroup "eval_contract-Random"
      [ bench "randomContract-size10" $
          nf (eval_contract model) randomContract10

      , bench "randomContract-size50" $
          nf (eval_contract model) randomContract50
      ]
    ]
