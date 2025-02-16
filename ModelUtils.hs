module ModelUtils where

import ContractsDSL

type TimeStep = Int 
type StepSize = Days

type LatticeModel a = [[a]]

type ValSlice a = [a]
newtype PR a = PR ([ValSlice a])

instance Show a => Show (PR a) where
    show (PR layers) = unlines $ zipWith formatLayer [0..] layers
      where
        formatLayer :: Int -> ValSlice a -> String
        formatLayer n vals = "Step " ++ show n ++ ": " ++ unwords (map show vals)

instance Num a => Num (PR a) where
    (+) = lift2 (+)
    (-) = lift2 (-)
    (*) = lift2 (*)
    abs = lift abs
    signum = lift signum
    fromInteger n = PR (initLatticeModel [fromInteger n] (\x -> x) (\x -> x))
    negate = lift negate

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
    discount        :: Date -> PR Double -> PR Double,
    snell           :: Date -> PR Double -> PR Double,
    exchange        :: Currency -> PR Double,
    stockModel      :: Stock -> PR Double
    }


exampleModel :: Date -> StepSize -> Model
exampleModel startDate stepSize = Model {
    startDate = startDate,
    stepSize = stepSize,
    constPr = constPr,
    discount = discount,
    snell = snell,
    exchange = exchange,
    stockModel = stockModel
    }

    where
        constPr :: Double -> PR Double
        constPr val = PR (initLatticeModel [val] (\x -> x) (\x -> x))

        -- for simplicity base currency is GBP
        exchange :: Currency -> PR Double
        exchange cur = 
            case lookup cur exchangeRates of 
                Just lm -> lm
                Nothing -> error $ "Exchange rate of currency " ++ (show cur) ++ " not found"
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

        stockModel :: Stock -> PR Double
        stockModel stk =
            case lookup stk stockPrices of 
                    Just lm -> lm
                    Nothing -> error $ "Stock model for stock " ++ (show stk) ++ " not found"

            where
                -- volatility is monthly
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

        discount :: Date -> PR Double -> PR Double
        discount d (PR pr) = PR (reverse (discount' lattice_depth (pr !! lattice_depth)))
            where
                lattice_depth = ((daysBetween startDate d) `div` stepSize) 

                discount' :: TimeStep -> ValSlice Double -> [ValSlice Double]
                discount' (-1) _ = []
                discount' t prevSlice = prevSlice : discount' (t - 1) (discountedSlice t prevSlice)
    
        snell :: Date -> PR Double -> PR Double
        snell d (PR pr) = PR (reverse (snell' lattice_depth pr (pr !! lattice_depth)))
            where 
                lattice_depth = ((daysBetween startDate d) `div` stepSize)  

                snell' :: TimeStep -> [ValSlice Double] -> ValSlice Double -> [ValSlice Double]
                snell' (-1) _ _ = []
                snell' t immediateVals prevSlice = prevSlice : snell' (t - 1) immediateVals (max (discountedSlice t prevSlice) (immediateVals !! (t-1)))

        discountedSlice :: TimeStep -> ValSlice Double -> ValSlice Double
        discountedSlice t prevSlice =  
            [(prev_val1 + prev_val2) / (2 * (1 + ir)) | (ir, (prev_val1, prev_val2)) <- zip irs pairs]
                where   pairs = zip (tail prevSlice) (init prevSlice)
                        irs = interest_rates !! (t - 1)         
                
        stepSizeD :: Double
        stepSizeD = fromIntegral stepSize

        -- for exchange rates and stock prices 
        _CCRModel :: Double -> Double -> Double -> LatticeModel Double
        _CCRModel initVal volatility volNormFac = 
            initLatticeModel [initVal] (\prev -> prev * exp (volatility * sqrt volNormFac)) (\prev -> prev * exp (- volatility * sqrt volNormFac)) 

        -- for interest rates
        _HLIRModel :: Double -> Double -> Double -> LatticeModel Double
        _HLIRModel initVal volatility volNormFac = 
            initLatticeModel [initVal] (\prev -> prev + volatility * sqrt volNormFac) (\prev -> prev - volatility * sqrt volNormFac)
    
-- _CCRModel :: Double -> Double -> Double -> LatticeModel
-- _CCRModel initVal vol time = initLatticeModel [initVal] up down
--   where
--     up x = x * exp (vol * sqrt time)
--     down x = x * exp (-vol * sqrt time)

-- _HLIRModel :: Double -> Double -> Double -> LatticeModel
-- _HLIRModel initVal vol time = initLatticeModel [initVal] up down
--   where
--     up x = x + vol * sqrt time
--     down x = x - vol * sqrt time