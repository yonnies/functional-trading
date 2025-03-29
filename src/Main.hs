module Main where

import ContractsDSL
import EvaluationEngine 
import ModelUtils

import Control.Monad.State


evalTest :: Model -> Contract -> Bool -> Either String (PR Double)
evalTest model c optContract= do
  _ <- validityCheck c (startDate model)
  let optC = if optContract then optimiseContract c else c
  (result, _finalState) <- runStateT (evalC model optC (startDate model)) emptyEvalState
  return result

main :: IO ()
main = 
    let c = Or (AcquireWhen (Lift2B CGE (Lift2D BAdd (StockPrice NVDA) (LiftD UNegate (Konst 2.5238095238095237))) (Konst (-0.6))) (AcquireOn (date "15-11-2027") (Or (Give (One GBP)) None))) (And (AcquireOn (date "24-02-2028") (Give (Scale (StockPrice NVDA) (One USD)))) (Scale (Konst 123.1176470588236) (AcquireOn (date "09-02-2029") (One EUR))))
        opt_c = optimiseContract c 
        model = exampleModel today 30
    in do
        putStrLn (show c)
        putStrLn (show opt_c)
        putStrLn (show $ evalTest model c True)
        putStrLn (show $ evalTest model c False)