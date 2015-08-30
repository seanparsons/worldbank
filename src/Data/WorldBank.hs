module Data.WorldBank (
  getCountryInfo
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy hiding (putStrLn)
import Data.Text
import Data.WorldBank.TH
import Data.Conduit
import Data.Conduit.List
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Vector as V

data WBPageInfo = WBPageInfo
                { piPage    :: Int
                , piPages   :: Int
                } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbPageInfoFieldMappings} ''WBPageInfo)

data WBCountryInfo = WBCountryInfo
                   { ciName     :: Text
                   , ciId       :: Text
                   , ciIso2Code :: Text
                   } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbCountryInfoFieldMappings} ''WBCountryInfo)

data WBResultPair a = WBResultPair
                    { rpPageInfo      :: WBPageInfo
                    , rpCountryInfo   :: [WBCountryInfo]
                    }

instance (FromJSON a) => FromJSON (WBResultPair a) where
  parseJSON v =
    let checkArray a = case V.toList a of
                                         [first, second]  -> WBResultPair <$> parseJSON first <*> parseJSON second
                                         _                -> typeMismatch "Invalid result pairing." v
    in  withArray "Expected array at top level." checkArray v

decodeResult :: Response ByteString -> Maybe (WBResultPair WBCountryInfo)
decodeResult response = decode' $ responseBody response

pullInfo :: Int -> Manager -> ConduitM () [WBCountryInfo] (ResourceT IO) ()
pullInfo page manager = do
  request <- liftIO $ parseUrl ("http://api.worldbank.org/countries/all?format=json&page=" ++ show page)
  response <- liftIO $ httpLbs request manager
  let morePages v = yield v >> pullInfo (page + 1) manager
  case (decodeResult response) of
                                            Just (WBResultPair WBPageInfo{..} countryInfo)  -> if piPage == piPages then yield countryInfo else morePages countryInfo
                                            Nothing                                         -> liftIO $ fail "Couldn't decode response."

getCountryInfo :: IO [WBCountryInfo]
getCountryInfo = do
  fmap join $ runResourceT $ (bracketP (newManager tlsManagerSettings) closeManager (pullInfo 1) $$ consume)

someFunc :: IO ()
someFunc = putStrLn "someFunc"
