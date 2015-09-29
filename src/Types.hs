{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}
module Types where
import           Data.Aeson
import           Data.Aeson.TH
import           GHC.Generics


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


newtype Bool' = Bool' Bool  deriving (Eq, Ord, Generic)
instance ToJSON Bool'
instance Show Bool' where
  show (Bool' True) = "true"
  show (Bool' False) = "false"

data GeoReq = GeoReq { homeMobileCountryCode :: Maybe String
                     , homeMobileNetworkCode :: Maybe String
                     , radioType             :: Maybe RadioType
                     , carrier               :: Maybe String
                     , considerIp            :: Maybe Bool'
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
