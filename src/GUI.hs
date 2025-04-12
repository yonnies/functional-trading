{-# LANGUAGE OverloadedStrings #-}
module GUI where

import Text.Printf (printf)

import ContractsDSL
import ModelUtils
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
parseContractRequest _ prms = Left "Invalid contract type or parameters"

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
    [ "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show svgWidth ++ "' height='" ++ show svgHeight ++ "'>"
    , "<rect width='100%' height='100%' fill='none'/>"
    ] ++ concatMap renderLayer (zip [0..] layers) ++ ["</svg>"]
    where
        layerCount = length layers
        nodeRadius = 23
        xSpacing = 80 -- Horizontal spacing between layers
        ySpacing = 80  -- Vertical spacing between nodes in the same layer
        svgWidth = layerCount * xSpacing + 50
        svgHeight = layerCount * ySpacing + 50

        -- Calculate the position of a node (left-to-right layout)
        nodePosition :: Int -> Int -> (Double, Double)
        nodePosition layerIndex nodeIndex =
            let x = fromIntegral layerIndex * fromIntegral xSpacing + 50 -- Convert xSpacing to Double
                y = if layerIndex + 1 < length layers
                    then let (_, yChild1) = nodePosition (layerIndex + 1) nodeIndex
                             (_, yChild2) = nodePosition (layerIndex + 1) (nodeIndex + 1)
                        in (yChild1 + yChild2) / 2 -- Position in the middle of the two children
                    else fromIntegral svgHeight / 2
                        + fromIntegral (nodeIndex - layerIndex `div` 2) * fromIntegral ySpacing -- Convert ySpacing to Double
            in (x, y)

        -- Render a single layer of nodes and edges
        renderLayer :: (Int, [Double]) -> [String]
        renderLayer (layerIndex, values) =
            let nodes = zip [0..] values
                edges = if layerIndex + 1 < length layers -- Render edges for all layers except the last one
                        then concatMap (renderEdge layerIndex) [0..length values - 1]
                        else []
            in edges ++ map (renderNode layerIndex) nodes

        renderNode :: Int -> (Int, Double) -> String
        renderNode layerIndex (nodeIndex, value) =
            let (x, y) = nodePosition layerIndex nodeIndex
                valStr = printf "%.2f" value -- Format the number to two decimal places
                textWidth = fromIntegral (length valStr) * 8 -- Approximate width of the text (8px per character)
                rectWidth = max (textWidth + 10) 40 -- Add padding and ensure a minimum width
                rectHeight = rectWidth -- Fixed height for the rectangle
            in "<rect x='" ++ show (x - rectWidth / 2) ++ "' y='" ++ show (y - rectHeight / 2) ++ "' width='" ++ show rectWidth ++ "' height='" ++ show rectHeight ++ "' fill='white' stroke='gray'  />"
                ++ "<text x='" ++ show x ++ "' y='" ++ show (y+2) ++ "' font-size='15' fill='black' text-anchor='middle' dominant-baseline='middle'>" ++ valStr ++ "</text>"


        -- Render edges between layers
        renderEdge :: Int -> Int -> [String]
        renderEdge layerIndex nodeIndex =
            if layerIndex + 1 >= length layers
            then [] -- No edges if there is no next layer
            else
                let (x1, y1) = nodePosition layerIndex nodeIndex
                    (x2, y2) = nodePosition (layerIndex + 1) nodeIndex
                    (x3, y3) = nodePosition (layerIndex + 1) (nodeIndex + 1)
                in [ "<line x1='" ++ show x1 ++ "' y1='" ++ show y1 ++ "' x2='" ++ show x2 ++ "' y2='" ++ show y2 ++ "' stroke='gray' />"
                    , "<line x1='" ++ show x1 ++ "' y1='" ++ show y1 ++ "' x2='" ++ show x3 ++ "' y2='" ++ show y3 ++ "' stroke='gray' />"
                    ]