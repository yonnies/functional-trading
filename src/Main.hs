module Main where

import ContractsDSL
import EvaluationEngine         -- Original version with caching
import ModelUtils



main :: IO ()
main = 
    let 
        model = exampleModel today 30
        obs1 = stockPrice DIS + konst 10
        obs2 = stockPrice TSLA * konst 2
        sharedScale = Scale obs1 (Scale obs2 (one USD))
        result = eval model (AcquireOn (date "06-11-2026") (and_ sharedScale (and_ sharedScale sharedScale)))
    in  case result of
            Left err  -> putStrLn $ "Error: " ++ err   -- Handle error
            Right res -> putStrLn $ "Evaluation result: " ++ show res  -- Print the result



        -- putStrLn "Hello, this is the main application!"