{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

module Control.JSBSim.Database
  ( allocateMongoPipes, ensureIndices
  , JsbSimScriptRecord, HasJsbSimScriptRecord(..)
  , insertJsbSimScript, updateJsbSimScript, lookupJsbSimScript
  , lookupJsbSimScriptDigest, existsJsbSimScript
  ) where

import Control.Lens.Fold
import           Control.JSBSim.Types
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Lens.Operators
import           Control.Monad.Reader
import           Control.Monad.Trans.Resource
import           Data.JSBSim.Types
import           Data.Typeable
import           Database.MongoDB
import Data.Time.Clock
import qualified Data.Bson as Bson
import Data.Bson as Bson (val, Field(..), (!?))
import Data.JSBSim.Helper
import Data.JSBSim.Script
import Data.Aeson
import Data.Text (Text)
import Crypto.Hash.Algorithms
import Crypto.Hash
import qualified Data.Bson.Binary as Bson
import Data.Binary.Put
import qualified  Data.ByteString.Lazy as BL
import qualified Data.ByteArray as BA


type ScriptValue t =
  (Eq t, Show t, Real t, Typeable t, ToJSON t, FromJSON t, Fractional t)

data JsbSimScriptRecord t = JsbSimScriptRecord {
  _scriptId           :: Maybe ObjectId,
  _scriptCollection   :: Collection,
  _scriptLastModified :: UTCTime,
  _scriptScript       :: JsbSimScript t,
  _scriptETag         :: Digest SHA3_256
  } deriving (Typeable, Eq, Show)

makePrisms 'JsbSimScriptRecord
makeClassy 'JsbSimScriptRecord

instance HasJsbSimScript (JsbSimScriptRecord t) t where
  jsbSimScript = scriptScript

instance (Typeable d, HashAlgorithm d) => Val (Digest d) where
  cast' (Bson.Bin (Bson.Binary bs)) = digestFromByteString bs
  cast' _ = Nothing
  val = Bson.Bin . Bson.Binary . BA.convert


instance (ScriptValue t) => Bson.Val (JsbSimScriptRecord t) where
  val = Bson.Doc . jsbSimScriptRecordDoc
  cast' (Doc d) = JsbSimScriptRecord
    <$> d !? "_id"
    <*> d !? "collection"
    <*> d !? "lastModified"
    <*> (d !? "script" >>= preview _JsonBson')
    <*> (d !? "etag")
  cast' _ = Nothing

collectionScripts :: Collection
collectionScripts = "scripts"

jsbSimScriptRecordDoc :: (HasJsbSimScriptRecord a t, Eq t, Show t, Real t, Typeable t, ToJSON t, FromJSON t, Fractional t) => a -> Document
jsbSimScriptRecordDoc r = [
  "collection" := val (r ^. scriptCollection),
  "lastModified" := val (r ^. scriptLastModified),
  "script" := (_JsonBson' # (r ^. scriptScript) ),
  "etag" := val (r ^. scriptETag)
  ] ++ (maybe [] (pure . ("_id" :=) . val) (r ^. scriptId))


allocateMongoPipes :: (MonadResource m, HasSimdConfig cfg) =>
  cfg -> m (Pipe, Pipe)
allocateMongoPipes cfg = case cfg ^. simdMongoConfig of
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

ensureIndices :: (HasSimdState st, MonadReader st m, MonadIO m) => m ()
ensureIndices = accessMongo (ConfirmWrites ["w" =: (1 :: Int)]) $ do
  sequence_ $ ensureIndex <$> is
  where
    is = [ (index collectionScripts [ "collection" =: (1 :: Int)
                                   , "script.scriptName" =: (1 :: Int) ])
           { iUnique = True }
          ]
insertJsbSimScript ::
  (HasSimdState st, MonadReader st m, MonadIO m, HasJsbSimScript a t, ScriptValue t)
  => Collection -> a -> m (ObjectId, Digest SHA3_256)
insertJsbSimScript c s = do
  t <- liftIO getCurrentTime
  let s' = s ^. jsbSimScript
      dig = hashJsonBson s'
      sr = _JsbSimScriptRecord # (Nothing, c, t, s', dig)
      d = jsbSimScriptRecordDoc sr
  oid <- accessMongo (ConfirmWrites ["w" =: (1 :: Int)]) $ insert collectionScripts d

  (,) <$> Bson.cast oid <*> pure dig



selectScript :: (Select aQueryOrSelection) => Collection -> Text ->
  aQueryOrSelection
selectScript c n =
  select [ "collection" := val c, "script.scriptName" := val n ] collectionScripts


updateJsbSimScript ::
  (HasSimdState st, MonadReader st m, MonadIO m, HasJsbSimScript a t, ScriptValue t)
  => Digest SHA3_256 -> Collection -> a -> m (Maybe (Digest SHA3_256))
updateJsbSimScript dig' c s = do
  let s' = s ^. jsbSimScript
      dig = hashJsonBson s'
      n = s' ^. scriptName
  if (dig == dig') then pure Nothing else do
    t <- liftIO getCurrentTime
    let q = selectScript c n
        m = [ "lastModified" := val t
            , "etag" := val dig
            , "script" := (_JsonBson' # s')
            ]
    accessMongo (ConfirmWrites ["w" =: (1 :: Int)]) $ modify q m
    return (Just dig)


existsJsbSimScript :: (HasSimdState st, MonadReader st m, MonadIO m) =>
  Collection -> Text -> m Bool
existsJsbSimScript c n  = lookupJsbSimScriptDigest c n >>=
  pure . maybe False (const True)




lookupJsbSimScriptDigest :: (HasSimdState st, MonadReader st m, MonadIO m) =>
  Collection -> Text -> m (Maybe (Digest SHA3_256))
lookupJsbSimScriptDigest c n = do
  let q = (selectScript c n) { limit = 1, project = [ "etag" =: (1 :: Int) ]  }
  join . fmap (!? "etag") <$> (accessMongo ReadStaleOk . findOne $ q)





lookupJsbSimScript :: (HasSimdState st, MonadReader st m, MonadIO m, ScriptValue t) =>
  Collection -> Text -> m (Maybe (JsbSimScriptRecord t))
lookupJsbSimScript c n =
  let q = selectScript c n
  in (accessMongo ReadStaleOk . findOne $ q) >>=
     maybe (pure Nothing) (fmap pure . Bson.cast . Doc)


hashJsonBson :: (ToJSON a, FromJSON a, HashAlgorithm d) => a -> Digest d
hashJsonBson s = case _JsonBson' # s of
  (Doc d') -> hash . BL.toStrict . runPut $ Bson.putDocument d'
  _ -> error "expected bson doc."
