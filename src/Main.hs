{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Main where


import           Control.Monad.Trans.Either
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics
import           Servant
import           Servant.Client

type GeoLocAPI = "geolocation" :> "v1" :> "geolocate" :> QueryParam "key" String :> ReqBody '[JSON] GeoReq :> Post '[JSON] GeoResp

data RadioType = LTE
               | GSM
               | CDMA
               | WCDMA
               deriving (Eq, Ord, Generic)
instance ToJSON RadioType
instance Show RadioType where
  show LTE = "lte"
  show GSM = "gsm"
  show CDMA = "cdma"
  show WCDMA = "wcdma"


newtype BoolLower = BoolLower Bool  deriving (Eq, Ord, Generic)
instance ToJSON BoolLower
instance Show BoolLower where
  show (BoolLower True) = "true"
  show (BoolLower False) = "false"

data GeoReq = GeoReq { homeMobileCountryCode :: Maybe String
                     , homeMobileNetworkCode :: Maybe String
                     , radioType             :: Maybe RadioType
                     , carrier               :: Maybe String
                     , considerIp            :: Maybe BoolLower
                     , cellTowers            :: Maybe [Tower]
                     , wifiAccessPoints      :: [WAP]
                     } deriving (Show, Eq, Ord, Generic)
-- instance ToJSON GeoReq
instance ToJSON GeoReq where
      toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }

data Tower = Tower { cellId            :: Maybe String
                   , locationAreaCode  :: String
                   , mobileCountryCode :: String
                   , mobileNetworkCode :: String
                   } deriving (Show, Eq, Ord, Generic)
instance ToJSON Tower where
      toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }


data WAP = WAP { macAddress         :: String
               , signalStrength     :: Maybe Float
               , age                :: Maybe Int
               , channel            :: Maybe Int
               , signalToNoiseRatio :: Maybe Float
               } deriving (Show, Eq, Ord, Generic)
instance ToJSON WAP where
      toJSON = genericToJSON $ defaultOptions { omitNothingFields = True }


data GeoResp = GeoResp { location :: Location
                       , accuracy :: Float
                       } deriving (Show, Eq, Ord, Generic)
data Location = Location { lat :: Float
                         , lng :: Float
                         } deriving (Show, Eq, Ord, Generic)
instance FromJSON GeoResp
instance FromJSON Location

api :: Proxy GeoLocAPI
api = Proxy

geolocate :: Maybe String -> GeoReq -> EitherT ServantError IO GeoResp
geolocate = client api (BaseUrl Https "www.googleapis.com" 443)

queries :: String -> EitherT ServantError IO (GeoResp)
queries = geolocate (Just key) req

main :: IO ()
main = do key <- getEnv "API_KEY"
          r <- runEitherT $ queries $ fromJust key
          case r of
           Left err -> print err
           Right resp -> print resp
req = defaultGeoReq
defaultGeoReq = GeoReq { homeMobileCountryCode = Nothing
                       , homeMobileNetworkCode = Nothing
                       , radioType             = Nothing
                       , carrier               = Nothing
                       , considerIp            = Nothing
                       , cellTowers            = Nothing
                       , wifiAccessPoints      = [WAP { macAddress = "b8:ca:3a:6c:f7:00"
                                                      , signalStrength =  Nothing
                                                      , age =  Nothing
                                                      , channel =  Nothing
                                                      , signalToNoiseRatio =  Nothing
                                                      }]
                       }
