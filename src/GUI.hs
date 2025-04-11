{-# LANGUAGE OverloadedStrings #-}
module GUI where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (delete)
import Control.Monad (void)
import Text.Printf (printf)
import ContractsDSL
import ModelUtils
import EvaluationEngine

-- currency exchange rate lattice
-- stock price lattice

europeanStockCall :: Date -> Double -> Stock -> Contract
europeanStockCall t strikePrice stk = 
    acquireOn t 
        (Scale (stockPrice stk) (one GBP) `And` 
        give (Scale (Konst strikePrice) (one GBP))) 
    `Or` acquireOn t none

europeanStockPut :: Date -> Double -> Stock -> Contract
europeanStockPut t strikePrice stk = 
    acquireOn t 
        (Scale (Konst strikePrice) (one GBP) `And` 
        give (Scale (stockPrice stk) (one GBP)))
    `Or` acquireOn t none

americanStockCall :: Date -> Double -> Stock -> Contract
americanStockCall t strikePrice stk = 
    acquireOnBefore t 
        (Scale (stockPrice stk) (one GBP) `And` 
        give (Scale (Konst strikePrice) (one GBP))) 
    `Or` acquireOn t none


americanStockPut :: Date -> Double -> Stock -> Contract
americanStockPut t strikePrice stk = 
    acquireOnBefore t 
        (Scale (Konst strikePrice) (one GBP) `And` 
        give (Scale (stockPrice stk) (one GBP)))
    `Or` acquireOn t none

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AcquireOn t (Scale (Konst val) (One cur))

upAndInOption :: Date -> Double -> Stock -> Double -> Contract
upAndInOption t barrierPrice stk payoff =
            acquireWhen (stockPrice stk %>= konst barrierPrice) 
                        (Scale (Konst payoff) (One GBP)) -- Activate the contract if the barrier is breached

downAndInOption :: Date -> Double -> Stock -> Double -> Contract
downAndInOption t barrierPrice stk payoff =
            acquireWhen (stockPrice stk %<= konst barrierPrice) 
                        (Scale (Konst payoff) (One GBP)) -- Activate the contract if the barrier is breached

shortfall :: Double -> Double -> Obs Double
shortfall goalYield actualYield = maxObs (Konst 0) (Konst goalYield - GrainYield actualYield)

shortfallGrainYieldC :: Date -> Double -> Double -> Contract
shortfallGrainYieldC t goalYield actualYield =
    AcquireOn t (Scale ((shortfall goalYield actualYield) * Konst 3) (one USD))

mainGUI :: IO ()
mainGUI = do
    startGUI defaultConfig { jsStatic = Just "." } setup

setup :: Window -> UI ()
setup window = do
    -- Add type annotation to resolve the ambiguous type
    runFunction $ ffi ("document.title = %1" :: String) ("Contract Evaluator" :: String)

    -- Link the external CSS file
    UI.addStyleSheet window "style.css"

    -- Input elements (with explicit numeric types)
    contractInput <- UI.textarea
        # set UI.rows (show (10 :: Int))
        # set UI.cols (show (50 :: Int))
        # set UI.value "AcquireOn (date \"05-02-2026\") (One GBP)"
    evalButton <- UI.button # set UI.text "Evaluate"
    resultOutput <- UI.div # set UI.style [ ("padding", "10px")]

    

    -- Layout (unchanged)
    UI.getBody window #+
        [ UI.h1 # set UI.text "Contract Evaluation"
        , UI.element contractInput
        , UI.element evalButton
        , UI.element resultOutput
        ]

    -- Button click handler (with void and element)
    on UI.click evalButton $ \_ -> do
        input <- get UI.value contractInput
        case parseContract input of
            Left err -> void $ element resultOutput # set UI.text ("Error: " ++ err)
            Right c -> do
                let model = exampleModel today 30
                    result = eval model c
                case result of
                    Left err -> void $ element resultOutput # set UI.text ("Evaluation Error: " ++ err)
                    Right pr -> void $ element resultOutput # set UI.html (formatPR pr)
    where
        parseContract :: String -> Either String Contract
        parseContract s = Right $ AcquireOn (date "05-02-2026") (One GBP)
        
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
                            then let (_, yChild1) = nodePosition (layerIndex + 1) nodeIndex
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