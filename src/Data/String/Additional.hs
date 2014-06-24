module Data.String.Additional where

import Data.Char (toLower)
import Data.Char (isUpper)
import Data.Char (isLower)

-- | Convert "CamelCASEDString" to "camel_cased_string"
camelToUnderscore :: String -> String
camelToUnderscore = map toLower . go2 . go1
    where go1 "" = ""
          go1 (x:u:l:xs) | isUpper u && isLower l = x : "_" ++ u : l : go1 xs
          go1 (x:xs) = x : go1 xs
          go2 "" = ""
          go2 (l:u:xs) | isLower l && isUpper u = l : "_" ++ u : go2 xs
          go2 (x:xs) = x : go2 xs
