{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE TemplateHaskell        #-}

module Data.JSBSim.Types where


import           Control.Lens.At
import           Control.Lens.Fold
import           Control.Lens.Getter
import           Control.Lens.Iso
import           Control.Lens.Operators                               hiding
                                                                       ((*~))
import           Control.Lens.Prism
import           Control.Lens.Review
import           Control.Lens.TH
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Aeson
import qualified Data.Bson                                            as Bson
import qualified Data.ByteString.Lazy                                 as BL
import           Data.Conduit
import qualified Data.Conduit.List                                    as CL
import           Data.HashMap.Lazy                                    (HashMap)
import qualified Data.HashMap.Lazy                                    as HashMap
import           Data.JSBSim.Helper
import           Data.JSBSim.Script
import           Data.JSBSim.XML
import           Data.Maybe
import           Data.Text                                            (Text)
import           Data.Text                                            (Text,
                                                                       unpack)
import qualified Data.Text                                            as T
import           Data.Typeable                                        (Typeable)
import           Data.Vector                                          (Vector)
import qualified Data.Vector                                          as V
import           Data.XML.Types
import qualified Data.XML.Types                                       as XML
import           GHC.Generics                                         (Generic)
import           Numeric.Units.Dimensional.NonSI
import           Numeric.Units.Dimensional.Prelude                    as Dim
import           Numeric.Units.Dimensional.UnitNames                  (UnitName)
import           Numeric.Units.Dimensional.UnitNames.InterchangeNames (HasInterchangeName (..),
                                                                       InterchangeName)
import qualified Prelude
import           Servant.API
import           Text.Read
import           Text.Read
import           Text.XML.Stream.Parse
import qualified Text.XML.Stream.Render                               as XR



newtype SimulatorInstanceId = SimulatorInstanceId Int
  deriving (Show, Eq, Ord, Typeable, Generic)

makePrisms ''SimulatorInstanceId
makeClassy ''SimulatorInstanceId

data SimulatorInstance = SimulatorInstance  {
  _simulatorId :: SimulatorInstanceId
  } deriving (Show, Eq, Typeable, Generic)

makeClassy ''SimulatorInstance
makePrisms ''SimulatorInstance

instance HasSimulatorInstanceId SimulatorInstance where
  simulatorInstanceId = simulatorId

data SimulatorInstanceCommand
  = SimulatorHalt
  | SimulatorRun
  deriving (Eq, Show, Typeable, Generic)

instance ToJSON SimulatorInstanceCommand
instance FromJSON SimulatorInstanceCommand

instance ToJSON SimulatorInstanceId
instance FromJSON SimulatorInstanceId

instance ToJSON SimulatorInstance
instance FromJSON SimulatorInstance

instance FromHttpApiData SimulatorInstanceId where
  parseUrlPiece a =
    maybe (Left a) Right . fmap SimulatorInstanceId . readMaybe . T.unpack $ a

instance ToHttpApiData SimulatorInstanceId where
  toUrlPiece  = T.pack . show .  view _SimulatorInstanceId

parseJsbSimScriptFile ::
  (Show a, Read a, Fractional a, MonadResource m, Eq a, MonadParseXML m) =>
  FilePath -> ConduitT i Void m (JsbSimScript a)
parseJsbSimScriptFile fn = parseFile def fn .| filterEmptyContent .|
  force "expected script file" fromXML





t :: (Show a, Read a, Eq a, Fractional a, MonadUnliftIO m, MonadParseXML m) =>
  m (JsbSimScript a)
t = runResourceT . runConduit . parseJsbSimScriptFile $ "/tmp/x.xml"

tt :: (ToXML a) => a -> IO Text
tt a = fmap mconcat . runConduit $ toXML a .| XR.renderText cfg .| CL.consume
  where cfg = XR.def { XR.rsPretty = True }

ttt = t >>= tt >>= (putStr . T.unpack)

t2 :: (MonadUnliftIO m, MonadParseXML m) =>
  m [XML.Event]
t2 = runResourceT . runConduit $ (parseFile def "/tmp/x.xml" .| filterEmptyContent .| CL.consume)
