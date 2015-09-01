module Data.WorldBank.TH where

import Data.Maybe
import Data.List

fieldTwiddle :: [(String, String)] -> String -> String
fieldTwiddle mappings name = fromMaybe name $ fmap snd $ find (\(key, _) -> key == name) mappings

wbPageInfoIFieldMappings :: [(String, String)]
wbPageInfoIFieldMappings = [("piiPage", "page"), ("piiPages", "pages")]

wbPageInfoSFieldMappings :: [(String, String)]
wbPageInfoSFieldMappings = [("pisPage", "page"), ("pisPages", "pages")]

wbCountryInfoFieldMappings :: [(String, String)]
wbCountryInfoFieldMappings = [("ciName", "name"), ("ciId", "id"), ("ciIso2Code", "iso2Code")]

wbIncomeLevelInfoFieldMappings :: [(String, String)]
wbIncomeLevelInfoFieldMappings = [("ilId", "id"), ("ilValue", "value")]

wbSourceInfoFieldMappings :: [(String, String)]
wbSourceInfoFieldMappings = [("siId", "id"), ("siName", "name")]

wbLendingTypeInfoFieldMappings :: [(String, String)]
wbLendingTypeInfoFieldMappings = [("ltId", "id"), ("ltIso2Code", "iso2code"), ("ltValue", "value")]

wbTopicInfoFieldMappings :: [(String, String)]
wbTopicInfoFieldMappings = [("tiId", "id"), ("tiValue", "value"), ("tiSourceNote", "sourceNote")]
