import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, diffUTCTime, addUTCTime)

------------------ Time and dates ------------------------
type Date = UTCTime
type Days = Int
type TimeStep = Int
type StepSize = Days

today :: UTCTime
today = date "01-11-2024"

-- Convert a string to a Date
date :: String -> Date -- "29-04-2002" expected date format
date s = case parseTimeM True defaultTimeLocale "%d-%m-%Y" s of 
                Just a -> a
                Nothing -> error $ s ++ " is not a valid date."

-- Calculate the number of days between two dates
daysBetween :: Date -> Date -> TimeStep
daysBetween d1 d2 = round (realToFrac (diffUTCTime d2 d1) / 86400.0)

------------------- Contracts ---------------------

data Contract
    = None                          -- Represents a contract with no obligations
    | One Currency                  -- Represents a contract for one unit of a currency
    | Give Contract                 -- Represents the opposite position of a contract
    | And Contract Contract         -- Adds the value of two contracts together
    | Or Contract Contract          -- Takes the contract that will yield the maximum value today
    | AquireAt Date Contract        -- Sets the aquisition date of a contract where the discounting should begin
    | Scale (Obs Double) Contract   -- Scales a contract by a numeric observable 


data Currency = GBP | USD | EUR
    deriving (Eq, Show)
                              
data Stock = DIS | TSLA | NVDA
    deriving (Eq, Show)

------------------ Observables ---------------------

data Obs a where
    Konst :: a -> Obs a
    StockPrice :: Stock -> Obs a

------------------- Evaluation ---------------------
-- Everything here is Evaluation model dependent

type LatticeModel = [[Double]]
type ValSlice = [Double]

newtype ValProc = VP ([ValSlice])
    deriving Show

instance Num ValProc where
    vp1 + vp2 = lift2 (+) vp1 vp2 
    vp1 - vp2 = lift2 (-) vp1 vp2 
    vp1 * vp2 = lift2 (*) vp1 vp2 
    abs vp = lift abs vp
    signum vp = lift signum vp
    fromInteger n = VP [[fromInteger n]] -- this seems wrong
    negate vp = lift negate vp


lift :: (Double -> Double) -> ValProc -> ValProc
lift f (VP xss) = VP [[f x | x <- xs] | xs <- xss]

lift2 :: (Double -> Double -> Double) -> ValProc -> ValProc -> ValProc
lift2 f (VP xss) (VP yss) = VP [[f x y | (x,y) <- zip xs ys ] | (xs,ys) <- zip xss yss]


eval_contract :: Model -> Contract -> ValProc
eval_contract model@(Model startDate stepSize constPr discount exchange getStockModel) = eval
    where 
        eval None = constPr 0
        eval (One cur) = exchange cur
        eval (Give c) = negate (eval c)
        eval (And c1 c2) = (eval c1) + (eval c2)
        eval (Or c1 c2) = maximumValToday (eval c1) (eval c2)
        eval (AquireAt d c) = discount d (eval c) 
        eval (Scale obs c) = (eval_obs model obs) * (eval c)


eval_obs :: Model -> Obs Double -> ValProc
eval_obs (Model startDate stepSize constPr discount exchange getStockModel) = eval
    where 
        eval (Konst k) = constPr k
        eval (StockPrice stk) = getStockModel stk


maximumValToday :: ValProc -> ValProc -> ValProc
maximumValToday (VP (xs:xss)) (VP (ys:yss)) = if xs !! 0 > ys !! 0 then VP (xs:xss) else VP (ys:yss)

unwrapVP :: ValProc -> [ValSlice]
unwrapVP (VP x) = x

exampleModelInstance = exampleModel today 1

data Model = Model {
    startDate       :: Date,
    stepSize        :: StepSize,
    constPr         :: Double -> ValProc,
    discount        :: Date -> ValProc -> ValProc,
    exchange        :: Currency -> ValProc,
    getStockModel   :: Stock -> ValProc
    }


exampleModel :: Date -> StepSize -> Model
exampleModel startDate stepSize = Model {
    startDate = startDate,
    stepSize = stepSize,
    constPr = constPr,
    discount = discount,
    exchange = exchange,
    getStockModel = getStockModel
    }

    where
        constPr :: Double -> ValProc
        constPr val = VP (initLatticeModel [val] (\x -> x) (\x -> x))

        -- for simplicity base currency is GBP
        exchange :: Currency -> ValProc
        exchange cur = 
            case lookup cur exchangeRates of 
                Just lm -> lm
                Nothing -> error $ "Exchange rate of currency " ++ (show cur) ++ " not found"

            where
                -- yearly volatility, we want 3 moths lattice model for bonds
                exchangeRates :: [(Currency, ValProc)]
                exchangeRates = 
                    [ (GBP, constPr 1.0) -- assuming base currency is GBP
                    , (EUR, exchRateModel 0.83 0.0596 (stepSizeD / 365))
                    , (USD, exchRateModel 0.77 0.1819 (stepSizeD / 365))
                    ]

                exchRateModel :: Double -> Double -> Double -> ValProc
                exchRateModel initVal vol timeS = VP $ _CCRModel initVal vol timeS

        getStockModel :: Stock -> ValProc
        getStockModel stk =
            case lookup stk stockPrices of 
                    Just lm -> lm
                    Nothing -> error $ "Stock model for stock " ++ (show stk) ++ " not found"

            where
                -- volatility is monthly, but we want weekly lattice model 
                stockPrices :: [(Stock, ValProc)]
                stockPrices =   
                    [ (DIS, stockModel 109.12 0.2253 (stepSizeD / 30)) 
                    , (TSLA, stockModel 0.83 0.9790 (stepSizeD / 30))
                    , (NVDA, stockModel 0.77 0.3821 (stepSizeD / 30))
                    ]

                stockModel :: Double -> Double -> Double -> ValProc
                stockModel initVal vol timeS = VP $ _CCRModel initVal vol timeS

        discount :: Date -> ValProc -> ValProc
        discount d (VP vp) = VP (reverse (propagateLattice (lattice_depth) (vp !! (lattice_depth))))
            where
                lattice_depth = ((daysBetween startDate d) `div` stepSize) - 1

                propagateLattice :: TimeStep -> ValSlice -> [ValSlice]
                propagateLattice (-1) _ = []
                propagateLattice t valProc = 
                    valProc : propagateLattice (t - 1) [(prev_val1 + prev_val2) / (2 * (1 + ir)) | (ir, (prev_val1, prev_val2)) <- zip irs pairs]
                        where   pairs = zip (tail valProc) (init valProc)
                                irs = interest_rates !! (t - 1) 

                -- Volatility is annualised and time step is six months
                interest_rates :: LatticeModel
                interest_rates = _HLIRModel 0.05 0.01 (stepSizeD / 365)
                
        stepSizeD :: Double
        stepSizeD = fromIntegral stepSize

        initLatticeModel :: ValSlice -> (Double -> Double) -> (Double -> Double) -> LatticeModel
        initLatticeModel prevLayer upFactor downFactor = 
            prevLayer : (initLatticeModel curLayer upFactor downFactor) 
                where curLayer = ((upFactor $ head prevLayer) : map downFactor prevLayer)

        -- for exchange rates and stock prices 
        _CCRModel :: Double -> Double -> Double -> LatticeModel
        _CCRModel initVal volatility volNormFac = 
            initLatticeModel [initVal] (\prev -> prev * exp (volatility * sqrt volNormFac)) (\prev -> prev * exp (- volatility * sqrt volNormFac)) 

        -- for interest rates
        _HLIRModel :: Double -> Double -> Double -> LatticeModel
        _HLIRModel initVal volatility volNormFac = 
            initLatticeModel [initVal] (\prev -> prev + volatility * sqrt volNormFac) (\prev -> prev - volatility * sqrt volNormFac)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

-------------------- Example for testing ------------------------

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AquireAt t (Scale (Konst val) (One cur))

zcdbE :: Contract
zcdbE = zcdb (date "06-11-2028") 10 GBP