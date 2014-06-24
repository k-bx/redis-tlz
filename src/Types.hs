{-# LANGUAGE TemplateHaskell #-}

module Types where

import           Common.Aeson            (jsonOptions)
import           Data.Aeson.TH           (deriveJSON)
import           Data.ByteString         (ByteString)
import           Data.HashMap.Additional ()
import           Data.HashMap.Strict     (HashMap)
import           Data.HashSet            (HashSet)
import           Data.Vector             (Vector)
import Data.Text (Text)

type RedisKey = ByteString

data Dump = Dump { _dItems :: [RedisKV] }
    deriving (Show)

data RedisKV = RedisKV { _rkvKey :: ByteString
                       , _rkvVal :: RedisValue }
    deriving Show

-- | TODO: instead of bytestrings, use BaseString64 newtype wrapper
-- and encode/decode with base64
data RedisValue = RVString ByteString | RVHash (HashMap Text ByteString)
                | RVList (Vector ByteString) | RVSet (HashSet ByteString)
                | RVZSet (Vector (ByteString, Double))
    deriving (Show)

-- -- | Just a hack to type-check that we cover all types covered in
-- -- return of redis @TYPE@ command
-- checkRedisTypesAreCovered :: RedisType -> RedisValue
-- checkRedisTypesAreCovered R.None = undefined
-- checkRedisTypesAreCovered R.String = RVString undefined
-- checkRedisTypesAreCovered R.Hash = RVHash undefined
-- checkRedisTypesAreCovered R.List = RVList undefined
-- checkRedisTypesAreCovered R.Set = RVSet undefined
-- checkRedisTypesAreCovered R.ZSet = RVZSet undefined

$(deriveJSON (jsonOptions 0) ''RedisValue)
$(deriveJSON (jsonOptions 4) ''RedisKV)
$(deriveJSON (jsonOptions 2) ''Dump)
