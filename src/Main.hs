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
    let 
        not_expired_nested  = (One GBP)
        expired_nested      = AcquireOn (date "05-02-2025") $ Scale (Konst 2000) (One GBP) 
        combined = AcquireOnBefore (date "05-02-2030") (not_expired_nested `And` expired_nested)
        evalResult1 = eval model combined
        -- evalResult2 = eval model c2
        model = exampleModel today 30
    in do
        putStrLn (show $ evalResult1)
        -- putStrLn (show $ evalResult2)

        