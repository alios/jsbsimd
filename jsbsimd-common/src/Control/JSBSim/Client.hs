{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.JSBSim.Client
  where
  {-
  ( MonadClient, ClientCommand, ClientCommandF, HasClientEnv(..)
  , clientGetInstances, clientGetInstance, clientPostInstance
  , liftClientCommand
  ) where
  -}

import           Control.Monad.Free
import           Control.Monad.Free.TH
import           Data.JSBSim.Api
import           Data.JSBSim.Types
import           Data.Proxy
import           Servant.API
import           Servant.Client.Core
import           Data.Text (Text)

data ClientCommandF next where
  ClientScripts ::
    Text -> ([JsbSimScriptName] -> next) -> ClientCommandF next
  ClientGetScript ::
    Text -> JsbSimScriptName -> ((Headers '[Header "ETag" Text] (JsbSimScript Double)) -> next) ->
    ClientCommandF next
  ClientPostScript ::
    Text -> JsbSimScript Double -> next -> ClientCommandF next

instance Functor ClientCommandF where
  fmap f (ClientScripts c g)     = ClientScripts c (f . g)
  fmap f (ClientGetScript c n g)    = ClientGetScript c n (f . g)
  fmap f (ClientPostScript c s g) = ClientPostScript c s (f g)

type ClientCommand = Free ClientCommandF

type MonadClient m = MonadFree ClientCommandF m

class HasClientEnv m e | m -> e where
  runClientEnv :: e -> ClientCommand a -> m a


makeFree_ ''ClientCommandF

clientScripts    :: MonadClient m =>
  Text -> m [JsbSimScriptName]
clientGetScript  :: MonadClient m =>
  Text -> JsbSimScriptName -> m (Headers '[Header "ETag" Text] (JsbSimScript Double))
clientPostScript :: MonadClient m =>
  Text -> JsbSimScript Double -> m ()


data JSBSimdClient m = JSBSimdClient
  { getScripts  :: Text -> m [JsbSimScriptName]
  , getScript   :: JsbSimScriptName -> m (Headers '[Header "ETag" Text] (JsbSimScript Double))
  , postScript  :: Text -> JsbSimScript Double -> m (Headers '[Header "Location" Text, Header "ETag" Text] NoContent)
  }

runClientCommand' :: (Monad m) => JSBSimdClient m -> ClientCommand a -> m a
runClientCommand' c = iterM (r c)
  where
    r cl (ClientScripts c next)      = getScripts cl c >>= next
    r cl (ClientGetScript c n next)  = getScript cl c n >>= next
    r cl (ClientPostScript c s next) = postScript cl c s >> next




liftClientCommand :: (RunClient m) => ClientCommand a -> m a
liftClientCommand = runClientCommand' apiClient



apiClient :: forall m . RunClient m => JSBSimdClient m
apiClient = JSBSimdClient { .. }
  where getScripts :<|> getScript :<|> postScript
          = Proxy @(JSBSimdApi Double) `clientIn` Proxy @m
