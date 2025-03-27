{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module ContractsDSL where 

import Data.Time (Day, parseTimeM, defaultTimeLocale, diffDays)
import Test.QuickCheck
import Data.Time (addDays)

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
    = None                              -- Contract with no obligations
    | One Currency                      -- Contract for one unit of a currency
    | Give Contract                     -- Opposite position of a contract
    | And Contract Contract             -- Adds the value of two contracts together
    | Or Contract Contract              -- Takes the contract that will yield the maximum value today
    | AcquireOn Date Contract           -- Sets the horizon based on a date of a contract where the discounting should begin
    | AcquireOnBefore Date Contract     -- Shows oppportunity to acquire a contract anytime before a certain date 
    | Scale (Obs Double) Contract       -- Scales a contract by a numeric observable 
    | AcquireWhen (Obs Bool) Contract   -- Sets the horizon based on a boolean event of a contract where the discounting should begin
    deriving (Show, Eq, Ord)

----------------------------- Observables -----------------------------

data Obs a where
    Konst       :: Double -> Obs Double   -- Observable whose value remains unchanged at any point in time
    StockPrice  :: Stock -> Obs Double    -- Observable that represents price of a stock in different points of time
    -- DateO       :: Date -> Obs Bool
    LiftD       :: UnaryOp -> Obs Double -> Obs Double
    Lift2D      :: BinaryOp -> Obs Double -> Obs Double -> Obs Double
    Lift2B      :: CompareOp -> Obs Double -> Obs Double -> Obs Bool
    MaxObs      :: Obs Double -> Obs Double -> Obs Double
    
deriving instance Show a => Show (Obs a)
deriving instance Eq a => Eq (Obs a)
deriving instance Ord a => Ord (Obs a)

instance Num (Obs Double) where
  fromInteger = Konst . fromInteger
  o1 + o2 = Lift2D BAdd o1 o2
  o1 * o2 = Lift2D BMul o1 o2
  o1 - o2 = Lift2D BSub o1 o2
  negate o = LiftD UNegate o
  abs o  = LiftD UAbs o
  signum o  = LiftD USignum o

instance Fractional (Obs Double) where
  o1 / o2 = Lift2D BDiv o1 o2
  recip = error "recip not implemented for Obs Double"
  fromRational = error "fromRational not implemented for Obs Double"

(%<), (%<=), (%=), (%>=), (%>) :: Ord Double => Obs Double -> Obs Double -> Obs Bool
o1 %> o2 = Lift2B CGT o1 o2
o1 %>= o2 = Lift2B CGE o1 o2
o1 %< o2 = Lift2B CLT o1 o2
o1 %<= o2 = Lift2B CLE o1 o2
o1 %= o2 = Lift2B CEQ o1 o2

-- Enumerated types for unary and binary numeric operations.
-- These are needed because Haskell does not support Eq or Ord 
-- for function types (e.g., (Double -> Double)), which means we 
-- cannot directly store raw function values in our Obs data type.
data UnaryOp
  = UNegate
  | UAbs  
  | USignum
  deriving (Eq, Show, Ord)

data BinaryOp
  = BAdd
  | BSub
  | BMul
  | BDiv      
  deriving (Eq, Show, Ord)

data CompareOp
  = CLT    -- less than
  | CLE    -- less or equal
  | CEQ    -- equals
  | CGE    -- greater or equal
  | CGT    -- greater
  deriving (Eq, Show, Ord)


unaryOpMap :: UnaryOp -> (Double -> Double)
unaryOpMap UNegate = negate
unaryOpMap UAbs    = abs
unaryOpMap USignum = signum

binaryOpMap :: BinaryOp -> (Double -> Double -> Double)
binaryOpMap BAdd = (+)
binaryOpMap BSub = (-)
binaryOpMap BMul = (*)
binaryOpMap BDiv = (/)

compareOpMap :: CompareOp -> (Double -> Double -> Bool)
compareOpMap CLT = (<)
compareOpMap CLE = (<=)
compareOpMap CEQ = (==)
compareOpMap CGE = (>=)
compareOpMap CGT = (>)

----------------------------------------------------
-- Supported Currencies & Stocks 

data Currency = GBP | USD | EUR | BGN
    deriving (Eq, Show, Ord)
                              
data Stock = DIS | TSLA | NVDA
    deriving (Eq, Show, Ord)

----------------------------------------------------
-- Lower case notation to prevent typo bugs 

one :: Currency -> Contract
one = One

give :: Contract -> Contract
give = Give

and_ :: Contract -> Contract -> Contract
and_ = And

or_ :: Contract -> Contract -> Contract
or_ = Or

acquireOn :: Date -> Contract -> Contract
acquireOn = AcquireOn

acquireOnBefore :: Date -> Contract -> Contract
acquireOnBefore = AcquireOnBefore

scale :: Obs Double -> Contract -> Contract
scale = Scale

konst :: Double -> Obs Double
konst = Konst

stockPrice :: Stock -> Obs Double
stockPrice = StockPrice

acquireWhen :: Obs Bool -> Contract -> Contract
acquireWhen = AcquireWhen

maxObs :: Obs Double -> Obs Double -> Obs Double
maxObs = MaxObs

-------------------------------------------------
-- Contract generation rules 
-------------------------------------------------

-- Arbitrary instance for generating random contracts
instance Arbitrary Contract where
  arbitrary = sized genContract 

-- Generate random contracts with a size limit
genContract :: Int -> Gen Contract
genContract 0 = frequency
  [ (1, pure None)
  , (8, One <$> genRandomCurrency)
  ]
genContract n = oneof
  [ Give <$> genContract (n `div` 2)
  , do
      leftSize <- choose (0, n `div` 2)
      c1 <- genContract leftSize
      c2 <- genContract (n `div` 2 - leftSize)
      oneof [ pure (And c1 c2), pure (Or c1 c2) ]
  , do
      c <- genContract (n `div` 2)
      someDate <- genRandomDate
      oneof [ pure (AcquireOn someDate c), pure (AcquireOnBefore someDate c) ]
  , do
      c <- genContract (n `div` 2)
      obs <- genObsDouble
      pure (Scale obs c)
  , do
      c <- genContract (n `div` 2)
      obs <- genObsBool
      pure (AcquireWhen obs c)
  ]

-- Generate random numeric observables
genObsDouble :: Gen (Obs Double)
genObsDouble = sized $ \n -> 
  if n <= 0 then baseCase else frequency
    [ (4, baseCase)
    , (2, LiftD <$> genUnaryOp <*> resize (n-1) genObsDouble)
    , (2, Lift2D <$> genBinaryOp <*> resize (n `div` 2) genObsDouble <*> resize (n `div` 2) genObsDouble)
    ]
  where
    baseCase = frequency
      [ (3, Konst <$> arbitrary)
      , (2, StockPrice <$> genRandomStock)
      ]

-- Generate random boolean observables
genObsBool :: Gen (Obs Bool)
genObsBool = Lift2B <$> genCompareOp <*> genObsDouble <*> genObsDouble

-- Generate random operators for expressions
genUnaryOp :: Gen UnaryOp
genUnaryOp = elements [UNegate]

genBinaryOp :: Gen BinaryOp
genBinaryOp = elements [BAdd, BSub, BMul]

genCompareOp :: Gen CompareOp
genCompareOp = elements [CLT, CLE, CEQ, CGE, CGT]

-- Generate random currencies and stocks
genRandomCurrency :: Gen Currency
genRandomCurrency = elements [GBP, USD, EUR]

genRandomStock :: Gen Stock
genRandomStock = elements [DIS, TSLA, NVDA]

-- Generate a random date in a fixed range
genRandomDate :: Gen Date
genRandomDate = do
  let startDay = today
      endDay   = date "31-12-2030"
  offset <- choose (0, daysBetween startDay endDay)
  pure (addDays (toInteger offset) startDay)
