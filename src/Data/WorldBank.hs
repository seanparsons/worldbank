module Data.WorldBank (
    pullAllInfo
  , WBIncomeLevelInfo
  , WBCountryInfo
  , WBSourceInfo
  , WBTopicInfo
  , countriesUrl
  , incomeLevelsUrl
  , sourcesUrl
  , topicsUrl
) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.ByteString.Lazy hiding (putStrLn, unpack)
import Data.Foldable
import Data.Text
import Data.WorldBank.TH
import Data.Conduit
import Data.Conduit.List
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Scientific
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import qualified Data.Vector as V

data WBPageInfo = WBPageInfo
                { piPage    :: Int
                , piPages   :: Int
                } deriving (Eq, Show)

parseAsInt :: Value -> Parser Int
parseAsInt (Number n) = case (floatingOrInteger n) of
                                                      Right i   -> return i
                                                      Left _    -> mzero
parseAsInt (String t) = case (reads $ unpack t) of
                                          [(i, _)]  -> return i
                                          _         -> mzero
parseAsInt _          = mzero

instance FromJSON WBPageInfo where
  parseJSON (Object v)  = WBPageInfo <$>
                          ((v .: "page") >>= parseAsInt) <*>
                          ((v .: "pages") >>= parseAsInt)
  parseJSON _           = mzero

data WBCountryInfo = WBCountryInfo
                   { ciName     :: Text
                   , ciId       :: Text
                   , ciIso2Code :: Text
                   } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbCountryInfoFieldMappings} ''WBCountryInfo)

data WBIncomeLevelInfo = WBIncomeLevelInfo
                       { ilId     :: Text
                       , ilValue  :: Text
                       } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbIncomeLevelInfoFieldMappings} ''WBIncomeLevelInfo)

data WBSourceInfo = WBSourceInfo
                  { siId    :: Text
                  , siName  :: Text
                  } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbSourceInfoFieldMappings} ''WBSourceInfo)

data WBLendingTypeInfo = WBLendingTypeInfo
                       { ltId         :: Text
                       , ltIso2Code   :: Text
                       , ltValue      :: Text
                       } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbLendingTypeInfoFieldMappings} ''WBLendingTypeInfo)

data WBTopicInfo = WBTopicInfo
                 { tiId           :: Text
                 , tiValue        :: Text
                 , tiSourceNote   :: Text
                 } deriving (Eq, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = fieldTwiddle wbTopicInfoFieldMappings} ''WBTopicInfo)

data WBResultPair a = WBResultPair
                    { rpPageInfo  :: WBPageInfo
                    , rpResults   :: [a]
                    } deriving (Eq, Show)

instance FromJSON a => FromJSON (WBResultPair a) where
  parseJSON v =
    let checkArray a = case V.toList a of
                                         [first, second]  -> WBResultPair <$> parseJSON first <*> parseJSON second
                                         _                -> typeMismatch "Invalid result pairing." v
    in  withArray "Expected array at top level." checkArray v

decodeResult :: FromJSON a => Response ByteString -> Either String (WBResultPair a)
decodeResult response = eitherDecode' $ responseBody response

pullInfo :: FromJSON a => (Int -> String) -> Int -> Manager -> ConduitM () a (ResourceT IO) ()
pullInfo makeUrl page manager = do
  request <- liftIO $ parseUrl $ makeUrl page
  response <- liftIO $ httpLbs request manager
  let unwind v = traverse_ yield v
  let morePages v = unwind v >> pullInfo makeUrl (page + 1) manager
  case (decodeResult response) of
                                  Right (WBResultPair WBPageInfo{..} result) -> if piPage == piPages then unwind result else morePages result
                                  Left failure                               -> liftIO $ fail failure

pullAllInfo :: FromJSON a => (Int -> String) -> IO [a]
pullAllInfo makeUrl = do
  runResourceT $ (bracketP (newManager tlsManagerSettings) (\_ -> return ()) (pullInfo makeUrl 1) $$ consume)

allQueriesUrl :: String -> Int -> String
allQueriesUrl elementType page = "http://api.worldbank.org/v2/" ++ elementType ++ "/all?format=json&page=" ++ show page

countriesUrl :: Int -> String
countriesUrl = allQueriesUrl "countries"

incomeLevelsUrl :: Int -> String
incomeLevelsUrl = allQueriesUrl "incomeLevel"

sourcesUrl :: Int -> String
sourcesUrl = allQueriesUrl "sources"

lendingTypesUrl :: Int -> String
lendingTypesUrl = allQueriesUrl "lendingTypes"

topicsUrl :: Int -> String
topicsUrl = allQueriesUrl "topics"