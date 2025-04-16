{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))
import Control.Monad.IO.Class (liftIO)
import Data.Time (getCurrentTime, utctDay)

import ContractsDSL
import ValuationEngine
import ModelUtils
import ContractVisualiser 
import ContractParser
import ContractDefinitions

-- Define the input and output types
data ContractInput = ContractInput
  { contractType :: String
  , parameters   :: [String]
  } deriving (Generic, Show)

instance FromJSON ContractInput
instance ToJSON ContractInput

data ContractResult = ContractResult
  { result :: String
  , svg    :: String
  } deriving (Generic, Show)

instance FromJSON ContractResult
instance ToJSON ContractResult

-- Define the API
type API = "evaluate" :> ReqBody '[JSON] ContractInput :> Post '[JSON] ContractResult

-- Parse and evaluate the contract
parseContractRequest :: String -> [String] -> Either String Contract
parseContractRequest "europeanStockCall" [date', strikePrice, stock] =
  Right $ europeanStockCall (read date') (read strikePrice) (read stock)
parseContractRequest "europeanStockPut" [date', strikePrice, stock] =
  Right $ europeanStockPut (read date') (read strikePrice) (read stock)
parseContractRequest "americanStockCall" [date', strikePrice, stock] =
  Right $ americanStockCall (read date') (read strikePrice) (read stock)
parseContractRequest "americanStockPut" [date', strikePrice, stock] =
  Right $ americanStockPut (read date') (read strikePrice) (read stock)
parseContractRequest "zcdb" [date', value, currency] =
  Right $ zcdb (read date') (read value) (read currency)
parseContractRequest "upAndInOption" [barrierPrice, stock, payoff] =
  Right $ upAndInOption (read barrierPrice) (read stock) (read payoff)
parseContractRequest "downAndInOption" [barrierPrice, stock, payoff] =
  Right $ downAndInOption (read barrierPrice) (read stock) (read payoff)
parseContractRequest "shortfallGrainYieldC" [date', goalYield, actualYield] =
  Right $ shortfallGrainYieldC (read date') (read goalYield) (read actualYield)
parseContractRequest "customContract" [contractString] =
  case parseContract contractString of
    Left err -> Left $ "Parse error: " ++ err
    Right contract -> Right contract
parseContractRequest _ _ = Left "Invalid contract type or parameters"

-- currency exchange rate lattice
-- stock price lattice

server :: Date -> Server API
server today (ContractInput _contractType params) = liftIO $ do
  putStrLn $ "Received request: " ++ _contractType ++ " with params: " ++ show params
  case parseContractRequest _contractType params of
    Left err -> do
      putStrLn $ "Error: " ++ err
      return $ ContractResult ("Error: " ++ err) ""
    Right contract -> do
      let model = exampleModel today 30
      case eval model contract of
        Left evalErr -> do
          putStrLn $ "Evaluation Error: " ++ evalErr
          return $ ContractResult ("Evaluation Error: " ++ evalErr) ""
        Right pr -> do
          let resultText = show contract
          let svgContent = formatPR pr -- Generate the SVG using formatPR
          putStrLn $ "Sending response: " ++ resultText
          return $ ContractResult resultText svgContent

-- Run the server
main :: IO ()
main = do
  let port = 12125
  let corsPolicy = simpleCorsResourcePolicy
        { corsOrigins = Just (["http://localhost:12126"], True) -- Allow only this origin
        , corsMethods = ["GET", "POST", "OPTIONS"] -- Allow these HTTP methods
        , corsRequestHeaders = ["Content-Type"] -- Allow these headers
        }
  today <- utctDay <$> getCurrentTime
  putStrLn $ "Server is listening on port " ++ show port
  run port $ cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy API) (server today)
