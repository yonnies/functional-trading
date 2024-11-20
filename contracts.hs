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
    | AquireOnBefore Date Contract  -- Shows oppportunity to aquire a contract anytime before a certain date 
    | Scale (Obs Double) Contract   -- Scales a contract by a numeric observable 

------------------ Observables ---------------------

data Obs a where
    Konst :: a -> Obs a
    StockPrice :: Stock -> Obs a

none = None
one = One
give = Give
and = And
or = Or
aquireAt = AquireAt
scale = Scale
konst = Konst
stockPrice = StockPrice

----------------------------------------------------

data Currency = GBP | USD | EUR
    deriving (Eq, Show)
                              
data Stock = DIS | TSLA | NVDA
    deriving (Eq, Show)

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
eval_contract model@(Model startDate stepSize constPr discount snell exchange getStockModel) = eval
    where 
        eval None = constPr 0
        eval (One cur) = exchange cur
        eval (Give c) = negate (eval c)
        eval (And c1 c2) = (eval c1) + (eval c2)
        eval (Or c1 c2) = maxValOnStartDate (eval c1) (eval c2)
        eval (AquireAt d c) = discount d (eval c) 
        eval (AquireOnBefore d c) = snell d (eval c)
        eval (Scale obs c) = (eval_obs model obs) * (eval c)


eval_obs :: Model -> Obs Double -> ValProc
eval_obs (Model startDate stepSize constPr discount snell exchange getStockModel) = eval
    where 
        eval (Konst k) = constPr k
        eval (StockPrice stk) = getStockModel stk

-- Would be more accurate to be implemented using max on the whole process in casse max val on start date are equal
maxValOnStartDate :: ValProc -> ValProc -> ValProc
maxValOnStartDate (VP (xs:xss)) (VP (ys:yss)) = if xs !! 0 > ys !! 0 then VP (xs:xss) else VP (ys:yss)

data Model = Model {
    startDate       :: Date,
    stepSize        :: StepSize,
    constPr         :: Double -> ValProc,
    discount        :: Date -> ValProc -> ValProc,
    snell           :: Date -> ValProc -> ValProc,
    exchange        :: Currency -> ValProc,
    getStockModel   :: Stock -> ValProc
    }


exampleModel :: Date -> StepSize -> Model
exampleModel startDate stepSize = Model {
    startDate = startDate,
    stepSize = stepSize,
    constPr = constPr,
    discount = discount,
    snell = snell,
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
                -- Volatility is annualised
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
                -- volatility is monthly
                stockPrices :: [(Stock, ValProc)]
                stockPrices =   
                    [ (DIS, stockModel 109.12 0.2253 (stepSizeD / 30)) 
                    , (TSLA, stockModel 338.74 0.9899 (stepSizeD / 30))
                    , (NVDA, stockModel 140.15 0.3821 (stepSizeD / 30))
                    ]

                stockModel :: Double -> Double -> Double -> ValProc
                stockModel initVal vol timeS = VP $ _CCRModel initVal vol timeS

        -- Volatility is annualised
        interest_rates :: LatticeModel
        interest_rates = _HLIRModel 0.05 0.01 (stepSizeD / 365)

        discount :: Date -> ValProc -> ValProc
        discount d (VP vp) = VP (reverse (discount' lattice_depth (vp !! lattice_depth)))
            where
                lattice_depth = ((daysBetween startDate d) `div` stepSize) - 1

                discount' :: TimeStep -> ValSlice -> [ValSlice]
                discount' (-1) _ = []
                discount' t prevSlice = prevSlice : discount' (t - 1) (discountedSlice t prevSlice)
    
        snell :: Date -> ValProc -> ValProc
        snell d (VP vp) = VP (reverse (snell' lattice_depth vp (vp !! lattice_depth)))
            where 
                lattice_depth = ((daysBetween startDate d) `div` stepSize) - 1 

                snell' :: TimeStep -> [ValSlice] -> ValSlice -> [ValSlice]
                snell' (-1) _ _ = []
                snell' t immediateVals prevSlice = prevSlice : snell' (t - 1) immediateVals (max (discountedSlice t prevSlice) (immediateVals !! (t-1)))

        discountedSlice :: TimeStep -> ValSlice -> ValSlice
        discountedSlice t prevSlice =  
            [(prev_val1 + prev_val2) / (2 * (1 + ir)) | (ir, (prev_val1, prev_val2)) <- zip irs pairs]
                where   pairs = zip (tail prevSlice) (init prevSlice)
                        irs = interest_rates !! (t - 1)         
                
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

-------------------- Testing ------------------------

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AquireAt t (Scale (Konst val) (One cur))

-- Should subtract 5 after discounting
zcdb1 :: Contract
zcdb1 = And (zcdb (date "06-11-2028") 10 GBP) (Give $ (Scale (Konst 5) (One GBP)))
-- Example result:
--      VP [[3.6415112274834627],[3.900756564307965,4.246417013407305],[4.345794392523365,4.523809523809524,4.708737864077669],[5.0,5.0,5.0,5.0]] 

-- Should subtract 5 before discounting
zcdb2 :: Contract
zcdb2 = AquireAt (date "06-11-2028") (And (Scale (Konst 10) (One GBP)) (Give $ (Scale (Konst 5) (One GBP))))
-- Example result:
--      VP [[4.320755613741731],[4.450378282153983,4.623208506703652],[4.672897196261682,4.761904761904762,4.854368932038835],[5.0,5.0,5.0,5.0]]


european :: Date -> Contract -> Double -> Contract
european t underlying strikePrice = AquireAt t (underlying `And` give (scale (konst strikePrice) (one GBP))) `Or` none 

-- profitable
european1 :: Contract
european1 = european (date "05-04-2025") (Scale (StockPrice DIS) (One GBP)) 100 
-- Should return the underlying 

european2 :: Contract
european2 = european (date "05-03-2025") (Scale (StockPrice DIS) (One GBP)) 120 -- Too expesive
-- Should return 0 


-- Helpers
unwrapVP :: ValProc -> [ValSlice]
unwrapVP (VP x) = x

exampleModelInstance = exampleModel today 30



contract1 = AquireOnBefore (date "05-04-2025") ((Scale (StockPrice DIS) (One GBP)) `And` give (scale (konst 110) (one GBP)))



safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs
