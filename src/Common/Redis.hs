{-# LANGUAGE DeriveDataTypeable #-}

module Common.Redis where

import Database.Redis
import Control.Monad.Catch
import qualified Database.Redis as R
import Data.Typeable (Typeable)
import Types
import Control.Applicative ((<$>))
import qualified Data.Vector as V
import Control.Monad (void)

data RedisError = RedisError String
                deriving (Show, Typeable)
data UnexpectedNothing = UnexpectedNothing
                      deriving (Show, Typeable)
instance Exception RedisError
instance Exception UnexpectedNothing

redisToExc :: (MonadThrow m) => Either R.Reply a -> m a
redisToExc erpl =
  case erpl of
    Left rpl -> throwM (RedisError (show rpl))
    Right a -> return a

runRedisE :: R.Connection -> R.Redis (Either R.Reply a)
          -> IO a
runRedisE conn ra = do
  res <- R.runRedis conn ra
  redisToExc res

liftMaybeToExc :: (MonadThrow m) => Maybe a -> m a
liftMaybeToExc = maybe (throwM UnexpectedNothing) return

getRedisValue :: R.Connection -> RedisType -> RedisKey -> IO RedisValue
getRedisValue redis rt key =
  case rt of
    R.String -> RVString <$> ((runRedisE redis $ get key) >>= liftMaybeToExc)
    R.ZSet -> (RVZSet . V.fromList) <$> (runRedisE redis $ zrangeWithscores key 0 (-1))

storeRedisValue :: R.Connection -> RedisKV -> IO ()
storeRedisValue redis kv =
  case _rkvVal kv of
    RVString bs -> void $ runRedisE redis $ set (_rkvKey kv) bs
    RVZSet v -> void $ runRedisE redis $ zadd (_rkvKey kv) $ map flipPair $ V.toList v

flipPair :: (t1, t) -> (t, t1)
flipPair (a,b) = (b,a)
