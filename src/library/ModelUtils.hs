{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ModelUtils where

import Control.DeepSeq (NFData(..))
import Data.Time (addDays)

import ContractsDSL

type TimeStep = Int 
type StepSize = Days

type LatticeModel a = [[a]]

type ValSlice a = [a]
newtype PR a = PR ([ValSlice a])

instance (Show a) => Show (PR a) where
    show (PR layers) = unlines $ zipWith formatLayer [0..] layers
      where
        formatLayer :: Int -> ValSlice a -> String
        formatLayer n vals = "Step " ++ show n ++ ": " ++ unwords (map showVal vals)

        showVal :: (Show a) => a -> String
        showVal x =
            case castToDouble x of
                Just (y :: Double) -> show (fromIntegral (round (y * 100) :: Integer) / 100.0 :: Double) -- Round numbers
                Nothing -> show x  -- Print non-numeric types normally

        -- Function to safely cast numbers to Double, returns Nothing for non-numeric types
        castToDouble :: forall b. (Show b) => b -> Maybe Double
        castToDouble v = case reads (show v) :: [(Double, String)] of
                            [(num, "")] -> Just num
                            _ -> Nothing

-- Required for benchmarking
instance (NFData a) => NFData (PR a) where
  rnf (PR slices) = rnf slices

instance Num a => Num (PR a) where
    (+) = lift2Preserve (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum
    fromInteger n = PR (initLatticeModel [fromInteger n] (\x -> x) (\x -> x))
    negate = lift negate

lift :: (a -> b) -> PR a -> PR b
lift f (PR xss) = PR [[f x | x <- xs] | xs <- xss]

lift2Preserve :: (a -> a -> a) -> PR a -> PR a -> PR a    
lift2Preserve f (PR pr1) (PR pr2) = PR $ combine pr1 pr2
    where
        combine xss [] = xss
        combine [] yss = yss
        combine (xs:xss) (ys:yss) = 
            [f x y | (x, y) <- zip xs ys] : combine xss yss
    
lift2 :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2 f (PR xss) (PR yss) = PR [[f x y | (x,y) <- zip xs ys ] | (xs,ys) <- zip xss yss]

maxValToday :: PR Double -> PR Double -> PR Double
maxValToday (PR []) pr = pr
maxValToday pr (PR []) = pr
maxValToday (PR (xs:xss)) (PR (ys:yss)) = if xs !! 0 > ys !! 0 then PR (xs:xss) else PR (ys:yss)

maxPR :: Ord a => PR a -> PR a -> PR a
maxPR = lift2Preserve max

initLatticeModel :: ValSlice a -> (a -> a) -> (a -> a) -> LatticeModel a
initLatticeModel prevLayer upFactor downFactor = 
    prevLayer : (initLatticeModel curLayer upFactor downFactor) 
        where curLayer = ((upFactor $ head prevLayer) : map downFactor prevLayer)


data Model = Model {
    startDate       :: Date,
    stepSize        :: StepSize,
    constPr         :: Double -> PR Double,
    discDate        :: Date -> PR Double -> Either Error (PR Double),
    discObs         :: Int -> PR Double -> Either Error (PR Double),
    snell           :: Date -> PR Double -> Either Error (PR Double),
    exchange        :: Currency -> Either Error (PR Double),
    stockModel      :: Stock -> Either Error (PR Double),
    findHorizon     :: [ValSlice Bool] -> Int -> Either Error Int
    }

type Error = String

exampleModel :: Date -> StepSize -> Model
exampleModel start step = Model {
    startDate = start,
    stepSize = step,
    constPr = constPr',
    discDate = discDate',
    discObs = discObs',
    snell = snell',
    exchange = exchange',
    stockModel = stockModel',
    findHorizon = findHorizon'
    }

    where
        constPr' :: a -> PR a
        constPr' val = PR (constLayer 1 val)

        constLayer :: Int -> a -> [ValSlice a]
        constLayer n x = replicate n x : constLayer (n+1) x

        -- For simplicity base currency is GBP
        exchange' :: Currency -> Either Error (PR Double)
        exchange' cur = 
            case lookup cur exchangeRates of 
                Just lm -> Right lm
                Nothing -> Left ("Exchange rate of currency " ++ (show cur) ++ " not found")
            where
                -- Volatility is annualised
                exchangeRates :: [(Currency, PR Double)]
                exchangeRates = 
                    [ (GBP, constPr' 1.0) -- assuming base currency is GBP
                    , (EUR, exchRateModel 0.83 0.0596 (stepSizeD / 365))
                    , (USD, exchRateModel 0.77 0.1819 (stepSizeD / 365))
                    ]

                exchRateModel :: Double -> Double -> Double -> PR Double
                exchRateModel initVal vol timeS = PR $ _CCRModel initVal vol timeS

        stockModel' :: Stock -> Either Error (PR Double)
        stockModel' stk =
            case lookup stk stockPrices of 
                    Just lm -> Right lm
                    Nothing -> Left ("Stock model for stock " ++ (show stk) ++ " not found")

            where
                -- Volatility is monthly
                stockPrices :: [(Stock, PR Double)]
                stockPrices =   
                    [ (DIS, stockRates 109.12 0.2253 (stepSizeD / 30)) 
                    , (TSLA, stockRates 338.74 0.9899 (stepSizeD / 30))
                    , (NVDA, stockRates 140.15 0.3821 (stepSizeD / 30))
                    , (RACE, PR $ (stockWithDividend 400.00 (addDays 90 start) 20)) -- Dummy stock that pays dividend for american options
                    , (AAPL, PR $ (stockWithSpike 100.00 (addDays 90 start) 1.5)) -- Dummy stock with spike for barrier options
                    ]

                stockRates :: Double -> Double -> Double -> PR Double
                stockRates initVal vol timeS = PR $ _CCRModel initVal vol timeS

                stockWithDividend :: Double -> Date -> Double -> LatticeModel Double
                stockWithDividend value dividendDate dividend =
                    constPrWithChange value dividendDate (\v -> v - dividend)

                stockWithSpike :: Double -> Date -> Double -> LatticeModel Double
                stockWithSpike value spikeDate spike =
                    constPrWithChange value spikeDate (\v -> v * spike)

        -- Generate a const process with a modification applied after a specific layer
        constPrWithChange :: Double -> Date -> (Double -> Double) -> LatticeModel Double
        constPrWithChange value changeDate modify = [generateLayer n | n <- [0..]]
            where
                changeLayer = (daysBetween start changeDate) `div` step
                modifiedVal = modify value

                generateLayer :: Int -> ValSlice Double
                generateLayer n
                    | n >= changeLayer = replicate (n + 1) modifiedVal -- Apply modification
                    | otherwise = replicate (n + 1) value -- Constant values

        -- Volatility is annualised
        interest_rates :: LatticeModel Double
        interest_rates = _HLIRModel 0.05 0.01 (stepSizeD / 365)

        discObs' :: Int -> PR Double -> Either Error (PR Double)
        discObs' horizon pr = discount horizon pr

        discDate' :: Date -> PR Double -> Either Error (PR Double)
        discDate' d = discount ((daysBetween start d) `div` step)               

        findHorizon' :: [ValSlice Bool] -> Int -> Either Error Int
        findHorizon' _ n
            | n > 500 = Left "Exceeded 500 iterations in findHorizon!"
        findHorizon' [] _ = Left "Passed an empty process or no stopping condition was reached in findHorizon!"
        findHorizon' (bvs:bvss) n
            | and bvs   = Right n
            | otherwise = findHorizon' bvss (n+1)

        discount :: Int -> PR Double -> Either Error (PR Double)
        discount lattice_depth (PR pr) = Right $ PR (discount' 1)
                where 
                discount' :: TimeStep -> [ValSlice Double]
                discount' t 
                    | t >= lattice_depth + 1 = [pr !! lattice_depth]       
                    | otherwise = case discount' (t + 1) of
                        [] -> error "Unexpected empty list in discount'"
                        restSlices@(nextSlice:_) -> 
                            let curSlice = discountSlice t nextSlice
                            in curSlice : restSlices

        snell' :: Date -> PR Double -> Either Error (PR Double)
        snell' d (PR pr) = Right $ PR (snell'' 1)
            where 
                lattice_depth = ((daysBetween start d) `div` step) + 1     
                underlying_process_len = length (take lattice_depth pr) 

                maxByAverage :: [Double] -> [Double] -> [Double]
                maxByAverage xs ys
                    | avg xs >= avg ys = xs
                    | otherwise        = ys
                    where
                        avg lst = sum lst / fromIntegral (length lst)

                snell'' :: TimeStep -> [ValSlice Double]
                snell'' t 
                    | t == underlying_process_len && t == lattice_depth = [pr !! (underlying_process_len - 1)]
                    | t == underlying_process_len && t < lattice_depth = (pr !! (underlying_process_len - 1)) : snell'' (t+1)
                    | t > underlying_process_len && t < lattice_depth = (replicate (t+1) 0) : snell'' (t+1)
                    | t == lattice_depth = [replicate (t+1) 0]
                    | otherwise = case snell'' (t + 1) of
                        [] -> error "Unexpected empty list in snell''"
                        restSlices@(nextSlice:_) -> 
                            let curSlice = maxByAverage (discountSlice t nextSlice) (pr !! (t-1))
                            in curSlice : restSlices


        discountSlice :: TimeStep -> ValSlice Double -> ValSlice Double
        discountSlice t prevSlice =  
            [(prev_val1 + prev_val2) / (2 * (1 + ir)) | (ir, (prev_val1, prev_val2)) <- zip irs pairs]
                where   pairs = zip (tail prevSlice) (init prevSlice)
                        irs = interest_rates !! (t)         
                
        stepSizeD :: Double
        stepSizeD = fromIntegral step

        -- For exchange rates and stock prices 
        _CCRModel :: Double -> Double -> Double -> LatticeModel Double
        _CCRModel initVal vol time = initLatticeModel [initVal] up down
            where
                up x = x * exp (vol * sqrt time)
                down x = x * exp (-vol * sqrt time)

        -- For interest rates
        _HLIRModel :: Double -> Double -> Double -> LatticeModel Double
        _HLIRModel initVal vol time = initLatticeModel [initVal] up down
            where
                up x = x + vol * sqrt time
                down x = x - vol * sqrt time