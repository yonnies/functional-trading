{-# LANGUAGE OverloadedStrings #-}
module ContractVisualiser where

import Text.Printf (printf)

import ModelUtils

formatPR :: PR Double -> String
formatPR (PR layers) = unlines $
    [ "<svg xmlns='http://www.w3.org/2000/svg' width='" ++ show svgWidth ++ "' height='" ++ show svgHeight ++ "'>"
    , "<rect width='100%' height='100%' fill='none'/>"
    ] ++ concatMap renderLayer (zip [0..] layers) ++ ["</svg>"]
    where
        layerCount = length layers
        largestVal :: Double
        largestVal = maximum (concat layers)
        largestValInt :: Int
        largestValInt = floor largestVal
        nodeSize = (length (show largestValInt) + 2) * 8 + 20 -- Approximate width of the text (8px per character)
        xSpacing = nodeSize + 25 -- Horizontal spacing between layers
        ySpacing = nodeSize + 25  -- Vertical spacing between nodes in the same layer
        svgWidth = layerCount * xSpacing + 50
        svgHeight = layerCount * ySpacing + 80

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
                halfNodeSize = fromIntegral nodeSize / 2.0 -- Convert nodeSize to Double
            in "<rect x='" ++ show (x - halfNodeSize) ++ "' y='" ++ show (y - halfNodeSize) ++ "' width='" ++ show (fromIntegral nodeSize :: Double) ++ "' height='" ++ show (fromIntegral nodeSize :: Double) ++ "' fill='white' stroke='gray'  />"
                ++ "<text x='" ++ show x ++ "' y='" ++ show (y + 2.0) ++ "' font-size='15' fill='black' text-anchor='middle' dominant-baseline='middle'>" ++ valStr ++ "</text>"

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