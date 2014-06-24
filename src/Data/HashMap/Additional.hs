{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Data.HashMap.Additional where

import           Control.Arrow         (first)
import           Data.Aeson            (ToJSON (..), FromJSON (..))
import           Data.ByteString       (ByteString)
import           Data.Hashable         (Hashable)
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import qualified Data.ByteString.Base64 as B64
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

instance ToJSON ByteString where
    toJSON = toJSON . decodeUtf8 . B64.encode

instance FromJSON ByteString where
    parseJSON v = do
        t <- parseJSON v
        either fail return ((B64.decode . encodeUtf8) t)

mapKeys :: (Hashable b, Eq b)
        => (a -> b) -> HashMap a c -> HashMap b c
mapKeys f = HM.fromList . map (first f) . HM.toList
