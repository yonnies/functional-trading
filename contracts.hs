import Data.Time (UTCTime, parseTimeM, defaultTimeLocale, diffUTCTime, addUTCTime)

------------------ Time and dates ------------------------
type Date = UTCTime
type TimeStep = Int

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


------------------ Observables ---------------------

newtype Obs a = Obs (TimeStep -> a)

konst :: Double -> Obs Double
konst x = Obs (\_ -> x)


------------------- Evaluation -----------------------
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


eval_contract :: LatticeModel -> TimeStep -> Contract -> ValProc
eval_contract m t None = VP [replicate (t + 1) 0.0]
eval_contract m t (One cur) = VP [replicate (t + 1) 1.0]
eval_contract m t (Give c) = negate (eval_contract m t c)
eval_contract m t (And c1 c2) = (eval_contract m t c1) + (eval_contract m t c2)
eval_contract m t (Or c1 c2) = maximumValToday (eval_contract m t c1) (eval_contract m t c2)
eval_contract m t (AquireAt d c) = discount m (daysBetween today d) (eval_contract m (daysBetween today d) c)
eval_contract m t (Scale obs c) = (eval_obs obs t) * (eval_contract m t c)


eval_obs :: Obs Double -> TimeStep -> ValProc
eval_obs (Obs f) t = VP [replicate (t + 1) (f t)] 


maximumValToday :: ValProc -> ValProc -> ValProc
maximumValToday (VP (xs:xss)) (VP (ys:yss)) = if xs !! 0 > ys !! 0 then VP (xs:xss) else VP (ys:yss)


-- we will use lazy evaluation here since we don't know the expiry
initLatticeModel :: ValSlice -> Double -> LatticeModel
initLatticeModel prevLayer change = prevLayer : (initLatticeModel (((head prevLayer) + change) : [el - change | el <- prevLayer]) change)

simple_ir_model :: LatticeModel
simple_ir_model = (initLatticeModel [0.05] 0.01) 

-- reversing so today's value is first
discount :: LatticeModel -> TimeStep -> ValProc -> ValProc
discount m t (VP (vp:none)) = VP (reverse (propagateLattice m t vp))

-- I feel like this should be broken down so it can be used with different formulas apart from the intrest rate one
-- Missing timeStep difference in the formula
propagateLattice :: LatticeModel -> TimeStep -> ValSlice -> [ValSlice]
propagateLattice _ (-1) _ = []
propagateLattice m t valProc = valProc : propagateLattice m (t - 1) [(prev_val1 + prev_val2) / (2 * (1 + ir)) | (ir , (prev_val1, prev_val2)) <- zip irs pairs]
                            where   pairs = zip (tail valProc) (init valProc)
                                    irs = m !! (t - 1) 



-------------------- Example for testing ------------------------

zcdb :: Date -> Double -> Currency -> Contract
zcdb t val cur = AquireAt t (Scale (konst val) (One cur))

zcdbE :: Contract
zcdbE = zcdb (date "04-11-2024") 10 GBP 