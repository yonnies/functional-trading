module ModelUtils where

import Control.DeepSeq (NFData(..))

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
                Just (y :: Double) -> show (fromIntegral (round (y * 100)) / 100)  -- Round numbers
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
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum
    fromInteger n = PR (initLatticeModel [fromInteger n] (\x -> x) (\x -> x))
    negate = lift negate

instance (Num a, Fractional a) => Fractional (PR a) where
  (/) = lift2 (/) 
  recip = error "recip not implemented for processes"
  fromRational = error "fromRational not implemented for processes"

instance Eq a => Eq (PR a) where
    (PR pr1) == (PR pr2) = pr1 == pr2

instance Ord a => Ord (PR a) where  
    (PR pr1) <= (PR pr2) = pr1 <= pr2

lift :: (a -> b) -> PR a -> PR b
lift f (PR xss) = PR [[f x | x <- xs] | xs <- xss]

lift2 :: (a -> b -> c) -> PR a -> PR b -> PR c
lift2 f (PR xss) (PR yss) = PR [[f x y | (x,y) <- zip xs ys ] | (xs,ys) <- zip xss yss]

initLatticeModel :: ValSlice a -> (a -> a) -> (a -> a) -> LatticeModel a
initLatticeModel prevLayer upFactor downFactor = 
    prevLayer : (initLatticeModel curLayer upFactor downFactor) 
        where curLayer = ((upFactor $ head prevLayer) : map downFactor prevLayer)


data Model = Model {
    startDate       :: Date,
    stepSize        :: StepSize,
    constPr         :: Double -> PR Double,
    discDate        :: Date -> PR Double -> Either Error (PR Double),
    discObs         :: PR Bool -> PR Double -> Either Error (PR Double),
    snell           :: Date -> PR Double -> Either Error (PR Double),
    exchange        :: Currency -> Either Error (PR Double),
    stockModel      :: Stock -> Either Error (PR Double),
    datePr          :: Date -> PR Bool
    -- dfltPr          :: Date -> PR Bool
    }

type Error = String

exampleModel :: Date -> StepSize -> Model
exampleModel startDate stepSize = Model {
    startDate = startDate,
    stepSize = stepSize,
    constPr = constPr,
    discDate = discDate,
    discObs = discObs,
    snell = snell,
    exchange = exchange,
    stockModel = stockModel,
    datePr = datePr
    -- dfltPr = dfltPr
    }

    where
        constPr :: a -> PR a
        constPr val = PR (constSlice 1 val)

        constSlice :: Int -> a -> [ValSlice a]
        constSlice n x = replicate n x : constSlice (n+1) x

        datePr :: Date -> PR Bool
        datePr d = PR (datePr' 1)
            where 
                date_loc = ((daysBetween startDate d) `div` stepSize) + 1

                datePr' :: Int -> [ValSlice Bool]
                datePr' n 
                    | n == date_loc = replicate n True : datePr' (n+1)
                    | otherwise = replicate n False : datePr' (n+1)


        -- for simplicity base currency is GBP
        exchange :: Currency -> Either Error (PR Double)
        exchange cur = 
            case lookup cur exchangeRates of 
                Just lm -> Right lm
                Nothing -> Left ("Exchange rate of currency " ++ (show cur) ++ " not found")
            where
                -- Volatility is annualised
                exchangeRates :: [(Currency, PR Double)]
                exchangeRates = 
                    [ (GBP, constPr 1.0) -- assuming base currency is GBP
                    , (EUR, exchRateModel 0.83 0.0596 (stepSizeD / 365))
                    , (USD, exchRateModel 0.77 0.1819 (stepSizeD / 365))
                    ]

                exchRateModel :: Double -> Double -> Double -> PR Double
                exchRateModel initVal vol timeS = PR $ _CCRModel initVal vol timeS

        stockModel :: Stock -> Either Error (PR Double)
        stockModel stk =
            case lookup stk stockPrices of 
                    Just lm -> Right lm
                    Nothing -> Left ("Stock model for stock " ++ (show stk) ++ " not found")

            where
                -- Volatility is monthly
                stockPrices :: [(Stock, PR Double)]
                stockPrices =   
                    [ (DIS, stockModel 109.12 0.2253 (stepSizeD / 30)) 
                    , (TSLA, stockModel 338.74 0.9899 (stepSizeD / 30))
                    , (NVDA, stockModel 140.15 0.3821 (stepSizeD / 30))
                    ]

                stockModel :: Double -> Double -> Double -> PR Double
                stockModel initVal vol timeS = PR $ _CCRModel initVal vol timeS

        -- Volatility is annualised
        interest_rates :: LatticeModel Double
        interest_rates = _HLIRModel 0.05 0.01 (stepSizeD / 365)

        discount :: Int -> PR Double -> Either Error (PR Double)
        discount lattice_depth (PR pr)
            | lattice_depth < 1 = Left "Contract acquisition has been specified to an earlier date than model start date"
            | lattice_depth > length (take lattice_depth pr) = Left "Lattice depth exceeds PR slices in discount"
            | otherwise = Right $ PR (discount' 1)
            where
                discount' :: TimeStep -> [ValSlice Double]
                discount' t 
                    | t == lattice_depth + 1 = [pr !! lattice_depth]       
                    | otherwise = curSlice : restSlices 
                        where 
                            restSlices@(nextSlice:_) = discount' (t + 1) 
                            curSlice = (discountSlice (t+1) nextSlice)

        discObs :: PR Bool -> PR Double -> Either Error (PR Double)
        discObs (PR bpr) = discount (findHorizon bpr 0)

        discDate :: Date -> PR Double -> Either Error (PR Double)
        discDate d = discount ((daysBetween startDate d) `div` stepSize)

        findHorizon :: [ValSlice Bool] -> Int -> Int
        findHorizon [] _ = -1 
        findHorizon (bvs:bvss) n
            | and bvs = n
            | otherwise = findHorizon bvss (n+1)

    
        snell :: Date -> PR Double -> Either Error (PR Double)
        snell d (PR pr) 
            | lattice_depth < 1 = Left "Contract acquisition has been specified to an earlier date than model start date"
            | lattice_depth > length (take lattice_depth pr) = Left "Lattice depth exceeds PR slices in snell"
            | otherwise = Right $ PR (snell' 1)
            where 
                lattice_depth = ((daysBetween startDate d) `div` stepSize)  

                maxByAverage :: [Double] -> [Double] -> [Double]
                maxByAverage xs ys
                    | avg xs >= avg ys = xs
                    | otherwise        = ys
                    where
                        avg lst = sum lst / fromIntegral (length lst)

                snell' :: TimeStep -> [ValSlice Double]
                snell' t 
                    | t == lattice_depth + 1 = [pr !! lattice_depth]
                    | otherwise = curSlice : restSlices 
                        where 
                            restSlices@(nextSlice:_) = snell' (t + 1) 
                            curSlice = maxByAverage (discountSlice (t+1) nextSlice) (pr !! (t-1))


        discountSlice :: TimeStep -> ValSlice Double -> ValSlice Double
        discountSlice t prevSlice =  
            [(prev_val1 + prev_val2) / (2 * (1 + ir)) | (ir, (prev_val1, prev_val2)) <- zip irs pairs]
                where   pairs = zip (tail prevSlice) (init prevSlice)
                        irs = interest_rates !! (t - 1)         
                
        stepSizeD :: Double
        stepSizeD = fromIntegral stepSize

        -- for exchange rates and stock prices 
        _CCRModel :: Double -> Double -> Double -> LatticeModel Double
        _CCRModel initVal vol time = initLatticeModel [initVal] up down
            where
                up x = x * exp (vol * sqrt time)
                down x = x * exp (-vol * sqrt time)

        -- for interest rates
        _HLIRModel :: Double -> Double -> Double -> LatticeModel Double
        _HLIRModel initVal vol time = initLatticeModel [initVal] up down
            where
                up x = x + vol * sqrt time
                down x = x - vol * sqrt time



maximumValToday :: PR Double -> PR Double -> PR Double
maximumValToday (PR (xs:xss)) (PR (ys:yss)) = if xs !! 0 > ys !! 0 then PR (xs:xss) else PR (ys:yss)