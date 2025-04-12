{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Servant
import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import Control.Monad.IO.Class (liftIO)
import Network.Wai (Middleware)
import Network.Wai.Middleware.Cors (cors, simpleCorsResourcePolicy, CorsResourcePolicy(..))

-- Define the input and output types
data ContractInput = ContractInput
  { contractType :: String
  , parameters   :: [String]
  } deriving (Generic, Show)

instance FromJSON ContractInput
instance ToJSON ContractInput

data ContractResult = ContractResult
  { result :: String
  } deriving (Generic, Show)

instance FromJSON ContractResult
instance ToJSON ContractResult

-- Define the API
type API = "evaluate" :> ReqBody '[JSON] ContractInput :> Post '[JSON] ContractResult

-- Define the server logic
server :: Server API
server (ContractInput contractType params) = liftIO $ do
  putStrLn $ "Received request: " ++ contractType ++ " with params: " ++ show params
  let result = "Evaluated contract: " ++ contractType ++ " with params: " ++ show params
  putStrLn $ "Sending response: " ++ result
  return $ ContractResult result

-- Run the server
main :: IO ()
main = do
  let port = 12125
  let corsPolicy = simpleCorsResourcePolicy
        { corsOrigins = Just (["http://localhost:12126"], True) -- Allow only this origin
        , corsMethods = ["GET", "POST", "OPTIONS"] -- Allow these HTTP methods
        , corsRequestHeaders = ["Content-Type"] -- Allow these headers
        }
  putStrLn $ "Server is listening on port " ++ show port
  run port $ cors (const $ Just corsPolicy) $ serve (Proxy :: Proxy API) server