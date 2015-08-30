module Data.WorldBank.TH where

import Data.Maybe
import Data.List

fieldTwiddle :: [(String, String)] -> String -> String
fieldTwiddle mappings name = fromMaybe name $ fmap snd $ find (\(key, _) -> key == name) mappings

wbPageInfoFieldMappings :: [(String, String)]
wbPageInfoFieldMappings = [("piPage", "page"), ("piPages", "pages")]

wbCountryInfoFieldMappings :: [(String, String)]
wbCountryInfoFieldMappings = [("ciName", "name"), ("ciId", "id"), ("ciIso2Code", "iso2Code")]