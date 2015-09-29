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
import           System.Environment

import           Types

type GeoLocAPI = "geolocation" :> "v1" :> "geolocate" :> QueryParam "key" String :> ReqBody '[JSON] GeoReq :> Post '[JSON] GeoResp

api :: Proxy GeoLocAPI
api = Proxy

geolocate :: Maybe String -> GeoReq -> EitherT ServantError IO GeoResp
geolocate = client api (BaseUrl Https "www.googleapis.com" 443)

queries :: String -> EitherT ServantError IO (GeoResp)
queries key = geolocate (Just key) req

main :: IO ()
main = do key <- getEnv "API_KEY"
          r <- runEitherT $ queries key
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
