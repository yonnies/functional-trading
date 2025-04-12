{-# LANGUAGE OverloadedStrings #-}
module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)
import Control.Monad (void)
import Text.Printf (printf)
import ContractsDSL
import ModelUtils
import EvaluationEngine
import ContractParser 

-- Parse and evaluate the contract
parseContractRequest :: String -> [String] -> Either String Contract
parseContractRequest "europeanStockCall" [date, strikePrice, stock] =
  Right $ europeanStockCall (read date) (read strikePrice) (read stock)
parseContractRequest "europeanStockPut" [date, strikePrice, stock] =
  Right $ europeanStockPut (read date) (read strikePrice) (read stock)
parseContractRequest "americanStockCall" [date, strikePrice, stock] =
  Right $ americanStockCall (read date) (read strikePrice) (read stock)
parseContractRequest "americanStockPut" [date, strikePrice, stock] =
  Right $ americanStockPut (read date) (read strikePrice) (read stock)
parseContractRequest "zcdb" [date, value, currency] =
  Right $ zcdb (read date) (read value) (read currency)
parseContractRequest "upAndInOption" [barrierPrice, stock, payoff] =
  Right $ upAndInOption (read barrierPrice) (read stock) (read payoff)
parseContractRequest "downAndInOption" [barrierPrice, stock, payoff] =
  Right $ downAndInOption (read barrierPrice) (read stock) (read payoff)
parseContractRequest "shortfallGrainYieldC" [date, goalYield, actualYield] =
  Right $ shortfallGrainYieldC (read date) (read goalYield) (read actualYield)
parseContractRequest "customContract" [contractString] =
  case parseContract contractString of
    Left err -> Left $ "Parse error: " ++ err
    Right contract -> Right contract
parseContractRequest _ prms = Left ("Invalid contract type or parameters????" ++ (prms !! 0))

-- currency exchange rate lattice
-- stock price lattice

europeanStockCall :: Date -> Double -> Stock -> Contract
europeanStockCall t strikePrice stk = 
    AcquireOn t 
        (Scale (StockPrice stk) (One GBP) `And` 
        Give (Scale (Konst strikePrice) (One GBP))) 
    `Or` AcquireOn t None

europeanStockPut :: Date -> Double -> Stock -> Contract
europeanStockPut t strikePrice stk = 
    AcquireOn t 
        (Scale (Konst strikePrice) (One GBP) `And` 
        Give (Scale (StockPrice stk) (One GBP)))
    `Or` AcquireOn t None

americanStockCall :: Date -> Double -> Stock -> Contract
americanStockCall t strikePrice stk = 
    AcquireOnBefore t 
        (Scale (StockPrice stk) (One GBP) `And` 
        Give (Scale (Konst strikePrice) (One GBP))) 
    `Or` AcquireOn t None


americanStockPut :: Date -> Double -> Stock -> Contract
americanStockPut t strikePrice stk = 
    AcquireOnBefore t 
        (Scale (Konst strikePrice) (One GBP) `And` 
        Give (Scale (StockPrice stk) (One GBP)))
    `Or` AcquireOn t None

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

upAndInOption :: Double -> Stock -> Double -> Contract
upAndInOption barrierPrice stk payoff =
            AcquireWhen (StockPrice stk %>= Konst barrierPrice) 
                        (Scale (Konst payoff) (One GBP)) -- Activate the contract if the barrier is breached

downAndInOption :: Double -> Stock -> Double -> Contract
downAndInOption barrierPrice stk payoff =
            AcquireWhen (StockPrice stk %<= Konst barrierPrice) 
                        (Scale (Konst payoff) (One GBP)) -- Activate the contract if the barrier is breached

shortfall :: Double -> Double -> Obs Double
shortfall goalYield actualYield = MaxObs (Konst 0) (Konst goalYield - GrainYield actualYield)

shortfallGrainYieldC :: Date -> Double -> Double -> Contract
shortfallGrainYieldC t goalYield actualYield =
    AcquireOn t (Scale ((shortfall goalYield actualYield) * Konst 3) (One USD))

        
formatPR :: PR Double -> String
formatPR (PR layers) = unlines $
    [ "<svg xmlns='http://www.w3.org/2000/svg' width='800' height='600'>"
    , "<rect width='100%' height='100%' fill='white'/>"
    ] ++ concatMap renderLayer (zip [0..] layers) ++ ["</svg>"]
    where
        nodeRadius = 23
        xSpacing = 80 -- Horizontal spacing between layers
        ySpacing = 80  -- Vertical spacing between nodes in the same layer
        svgWidth = 800
        svgHeight = 400

        -- Calculate the position of a node (left-to-right layout)
        nodePosition :: Int -> Int -> (Double, Double)
        nodePosition layerIndex nodeIndex =
            let x = fromIntegral layerIndex * xSpacing + 50 -- Horizontal position based on layer
                y = if layerIndex + 1 < length layers
                    then    let (_, yChild1) = nodePosition (layerIndex + 1) nodeIndex
                                (_, yChild2) = nodePosition (layerIndex + 1) (nodeIndex + 1)
                            in (yChild1 + yChild2) / 2 -- Position in the middle of the two children
                    else fromIntegral svgHeight / 2 + fromIntegral (nodeIndex - layerIndex `div` 2) * ySpacing -- Default for the last layer
            in (x, y)

        -- Render a single layer of nodes and edges
        renderLayer :: (Int, [Double]) -> [String]
        renderLayer (layerIndex, values) =
            let nodes = zip [0..] values
                edges = if layerIndex + 1 < length layers -- Render edges for all layers except the last one
                        then concatMap (renderEdge layerIndex) [0..length values - 1]
                        else []
            in edges ++ map (renderNode layerIndex) nodes

        -- Render a single node
        renderNode :: Int -> (Int, Double) -> String
        renderNode layerIndex (nodeIndex, value) =
            let (x, y) = nodePosition layerIndex nodeIndex
                halfSize = fromIntegral nodeRadius -- Half the size of the square
            in "<rect x='" ++ show (x - halfSize) ++ "' y='" ++ show (y - halfSize) ++ "' width='" ++ show (2 * halfSize) ++ "' height='" ++ show (2 * halfSize) ++ "' fill='lightblue' stroke='black' />"
                ++ "<text x='" ++ show (x - 10) ++ "' y='" ++ show (y + 5) ++ "' font-size='15' fill='black'>" ++ printf "%.2f" value ++ "</text>"

        -- Render edges between layers
        renderEdge :: Int -> Int -> [String]
        renderEdge layerIndex nodeIndex =
            if layerIndex + 1 >= length layers
            then [] -- No edges if there is no next layer
            else
                let (x1, y1) = nodePosition layerIndex nodeIndex
                    (x2, y2) = nodePosition (layerIndex + 1) nodeIndex
                    (x3, y3) = nodePosition (layerIndex + 1) (nodeIndex + 1)
                in [ "<line x1='" ++ show x1 ++ "' y1='" ++ show y1 ++ "' x2='" ++ show x2 ++ "' y2='" ++ show y2 ++ "' stroke='black' />"
                    , "<line x1='" ++ show x1 ++ "' y1='" ++ show y1 ++ "' x2='" ++ show x3 ++ "' y2='" ++ show y3 ++ "' stroke='black' />"
                    ]