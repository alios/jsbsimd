{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeSynonymInstances  #-}

module Main where

main :: IO ()
main = pure ()

{-
import           Control.JSBSim.Client   as Client
import           Control.Monad.Reader
import           Data.Default.Class
import           Network.Connection
import           Network.HTTP.Client.TLS
import           Servant.Client



instance HasClientEnv IO ClientEnv where
  runClientEnv e m =
    runClientM (liftClientCommand m) e >>= either (fail . show) pure


main :: IO ()
main = do
  let cfg = def { settingDisableCertificateValidation = True }
      ss = mkManagerSettings cfg Nothing
  mgr <- newTlsManagerWith ss
  let e = mkClientEnv mgr $ BaseUrl Https "localhost" 3000 ""


  a <- runClientEnv e (clientScripts "default")
  liftIO $ print a
-}
