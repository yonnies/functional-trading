{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import ContractsDSL
import EvaluationEngine (optimiseContract, eval)
import ModelUtils

-- 1. Nested temporal structures with scaling and logical operations
complexTemporal :: Contract
complexTemporal =
  AcquireOn (date "2025-01-01") $
    Give (And
      (AcquireOn (date "2025-01-01")  -- Same date optimization
        (Or None 
          (Scale (Konst 2) 
            (Scale (StockPrice TSLA + Konst 10) 
              (One USD)))))
      (AcquireOnBefore (date "2026-01-01") $
        And (Give (Give (One EUR))))  -- Double Give elimination
    )

-- 2. Deep logical nesting with scale folding
deepLogicalScaling :: Contract
deepLogicalScaling =
  Or (Scale (Konst 2) 
     (And (Scale (Konst 3) (One USD)
         (Or (Scale (Konst 3) (One USD)  -- Identical branch
             (Scale (Konst 4) None))))))
    (And (Scale (Konst 1)  -- Unit scale elimination
         (AcquireOn (date "2024-01-01")  -- Early date elimination
             (Give (AcquireOn (date "2025-01-01") (One GBP))))))
       

-- 3. Mixed boolean and temporal with observable scaling
mixedObservable :: Contract
mixedObservable =
  AcquireWhen (StockPrice NVDA >. Konst 100) $
    And (Scale (StockPrice TSLA) 
        (Or (Scale (Konst 2) (One USD)
           (Scale (Konst 2) (One USD)))))  -- Common scale factor
      (AcquireOn (date "2025-01-01") $
        Give (And None 
          (Scale (Konst 3) 
            (Scale (Konst 4) (One EUR)))))

-- 4. Complex scaling with nested temporal constructs
layeredScaling :: Contract
layeredScaling =
  Scale (Konst 0.5) $
    AcquireOnBefore (date "2026-01-01") $
      Or (Scale (StockPrice DIS) 
          (And (Scale (Konst 2) (One USD)
               (Scale (Konst 3) (One USD)))))
         (AcquireOn (date "2025-01-01") $
           Give (Scale (Konst 6) 
             (Scale (Konst 0.5) (One GBP))))

-- 5. Full combination challenge
megaCombination :: Contract
megaCombination =
  Give $ And
    (AcquireOn (date "2025-01-01") $
      Or (Scale (Konst 2) 
           (And (Give (Give (One EUR)))
               (AcquireOn (date "2025-01-01")  -- Same date
                  (Scale (StockPrice TSLA) None)))
        (AcquireOnBefore (date "2026-01-01") $
          And (Scale (Konst 3) 
               (Scale (Konst 4) (One USD)))
              (Or (Scale (Konst 3) (One USD))  -- Common factor
                  (Scale (Konst 12) 
                    (Scale (Konst 0.25) (One USD)))))))

-- 6. Deeply nested redundant structures
deepRedundancy :: Contract
deepRedundancy =
  And (Or (And None (One USD))  -- Redundant None
          (And (AcquireOn (date "2024-01-01")  -- Early date
                  (Give (One EUR)))
               (Or (One USD) (One USD))))  -- Duplicate branch
      (Scale (Konst 2) 
        (Scale (Konst 3) 
          (Scale (Konst 4) (One JPY))))

-- 7. Interleaved temporal and scaling operations
temporalScalingMix :: Contract
temporalScalingMix =
  AcquireOn (date "2025-01-01") $
    Or (AcquireOnBefore (date "2026-01-01") 
          (Scale (Konst 2) 
            (Give (AcquireOn (date "2025-01-01")  -- Same date
                (Scale (Konst 3) None)))
       (And (Scale (StockPrice NVDA) 
              (Scale (Konst 0.5) (One USD)))
           (Scale (Konst 2) 
              (Scale (Konst 0.5) (One USD))))

-- 8. Complex observable handling with multiple optimizations
observableHeavy :: Contract
observableHeavy =
  Scale (StockPrice TSLA * Konst 2 + Konst 10) $
    And (AcquireWhen (StockPrice DIS >. Konst 50) 
            (Scale (Konst 0)  -- Zero scaling
                (One EUR)))
        (Or (Scale (Konst 2) 
               (Scale (Konst 3) (One USD)))
            (Scale (Konst 6) (One USD))))  -- Equivalent scaling

-- 9. Multiple layer acquisition nesting
multiLayerAcquisition :: Contract
multiLayerAcquisition =
  AcquireOn (date "2025-01-01") $
    Give (AcquireOn (date "2025-01-01")  -- Same date
        (And (AcquireOnBefore (date "2024-01-01")  -- Early
                (One GBP))
             (Or (Scale (Konst 2) (One USD))
                 (Scale (Konst 2) (One USD)))))

-- 10. Ultimate stress test
optimizationStressTest :: Contract
optimizationStressTest =
  Scale (Konst 0.5) $
    And (Give (And (AcquireOn (date "2025-01-01")
                    (Scale (Konst 2)
                      (AcquireOn (date "2025-01-01")
                          (One USD))))
                 (Or None
                     (Scale (Konst 4)
                       (Scale (Konst 0.25) (One EUR))))))
        (AcquireWhen (StockPrice NVDA >. Konst 200)
            (Or (And (Give (Give (One GBP)))
                     (Scale (Konst 3) (One USD)))
                (Scale (Konst 3) (One USD)))))

main :: IO ()
main = do
  let model = exampleModel today 30
  defaultMain
    [ bgroup "Optimization Benchmark"
      [ bgroup "Complex Temporal" 
        [ bench "Optimization" $ nf optimiseContract complexTemporal
        , bench "Evaluation" $ nf (eval model) complexTemporal
        ]
      , bgroup "Logical Scaling"
        [ bench "Optimization" $ nf optimiseContract deepLogicalScaling
        , bench "Optimized Eval" $ nf (eval model << optimiseContract) deepLogicalScaling
        ]
      , bgroup "Observable Mix"
        [ bench "Optimization" $ nf optimiseContract mixedObservable
        , bench "Evaluation Diff" $ nfIO $ do
            let orig = eval model mixedObservable
                opt = eval model (optimiseContract mixedObservable)
            return (orig, opt)
        ]
      , bgroup "Mega Combination"
        [ bench "Full Optimization" $ nf optimiseContract megaCombination
        , bench "Optimized Evaluation" $ nf (eval model << optimiseContract) megaCombination
        ]
      , bgroup "Stress Test"
        [ bench "Optimization Time" $ nf optimiseContract optimizationStressTest
        , bench "Eval Performance" $ nf (eval model << optimiseContract) optimizationStressTest
        ]
      ]
    ]