{-# LANGUAGE OverloadedStrings #-}

module Control.JSBSim.Database (allocateMongoPipes, writeTestData) where

import           Control.JSBSim.Types
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Database.MongoDB

allocateMongoPipes :: (MonadResource m, HasSimdConfig cfg) =>
  cfg -> m (Pipe, Pipe)
allocateMongoPipes cfg =
  case cfg ^. simdMongoConfig of
    Right rs    -> do
      rs' <- snd <$> allocate (openReplicaSet rs) closeReplicaSet
      liftIO $ (,) <$> primary rs' <*> secondaryOk rs'
    Left h -> do
      p <- snd <$> allocate (connect h) close
      pure (p,p)

accessMongo :: (HasSimdState st, MonadReader st m, MonadIO m) =>
  AccessMode -> Action m a -> m a
accessMongo m a = do
  st <- ask
  let (p,s) = st ^. simdState . simdMongo
      db = st ^. simdState . simdMongoDatabase
  case m of
    ReadStaleOk -> access s m db a
    _           -> access p m db a

writeTestData :: (HasSimdState st, MonadReader st m, MonadIO m) => m ()
writeTestData = accessMongo (ConfirmWrites ["w" =: (1 :: Int)]) $ do
    save "testCollection" [ "foo" =: ("bar" :: String)]
