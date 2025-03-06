{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module ContractsDSL where 

import Data.Time (Day, parseTimeM, defaultTimeLocale, diffDays)

--------------------------- Time and dates ---------------------------

type Date = Day
type Days = Int  

-- Convert a string to a Date
date :: String -> Date -- "29-04-2002" expected date format
date s = case parseTimeM True defaultTimeLocale "%d-%m-%Y" s of 
                Just a -> a
                Nothing -> error $ s ++ " is not a valid date."

-- Today’s date
-- Example placeholder, should be dynamically fetched in a real system
today :: Date
today = date "01-11-2024"

-- Used for contracts with infinite horizon (expiry date)
infiniteHorizon :: Date
infiniteHorizon = date "01-01-9999"

-- Calculate the number of days between two dates
daysBetween :: Date -> Date -> Days
daysBetween d1 d2 = fromIntegral (diffDays d2 d1)

----------------------------- Contracts ------------------------------

data Contract
    = None                          -- Contract with no obligations
    | One Currency                  -- Contract for one unit of a currency
    | Give Contract                 -- Opposite position of a contract
    | And Contract Contract         -- Adds the value of two contracts together
    | Or Contract Contract          -- Takes the contract that will yield the maximum value today
    | AcquireOn Date Contract       -- Sets the aquisition date of a contract where the discounting should begin
    | AcquireOnBefore Date Contract -- Shows oppportunity to acquire a contract anytime before a certain date 
    | Scale (Obs Double) Contract   -- Scales a contract by a numeric observable 
    | When (Obs Bool) Contract
    deriving (Show, Eq, Ord)

----------------------------- Observables -----------------------------

data Obs a where
    Konst :: Double -> Obs Double             -- Observable whose value remains unchanged at any point in time
    StockPrice :: Stock -> Obs Double    -- Observable that represents price of a stock in different points of time
    DateO :: Date -> Obs Bool
    
deriving instance Show a => Show (Obs a)
deriving instance Eq a => Eq (Obs a)
deriving instance Ord a => Ord (Obs a)

----------------------------------------------------
-- Lower case notation to prevent typo bugs 

none = None
one = One
give = Give
and_ = And
or_ = Or
acquireOn = AcquireOn
scale = Scale
konst = Konst
stockPrice = StockPrice
dateO = DateO
when = When

----------------------------------------------------
-- Supported Currencies & Stocks 

data Currency = GBP | USD | EUR
    deriving (Eq, Show, Ord)
                              
data Stock = DIS | TSLA | NVDA
    deriving (Eq, Show, Ord)




