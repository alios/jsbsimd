{-# LANGUAGE TemplateHaskell #-}


module Control.JSBSim.JSBSim where


import           Control.Lens.Operators
import           Control.Lens.TH
import           System.Process.Typed

data JsbsimProcConfig = JsbsimProcConfig
  { _jsbsimProcExec       :: FilePath
  , _jsbsimProcRootDir    :: FilePath
  , _jsbsimProcIsRealtime :: Bool
  , _jsbsimProcIsNice     :: Bool
  , _jsbsimProcScript     :: FilePath
  } deriving (Show, Eq)

makeClassy ''JsbsimProcConfig
makePrisms ''JsbsimProcConfig


foo i cfg =
  let args =
        [ cfg ^. jsbsimProcScript
        , "--root=" ++ cfg ^. jsbsimProcRootDir
        , "--suspend"
        ]
        ++ if cfg ^. jsbsimProcIsNice then pure "--nice" else mempty
        ++ if cfg ^. jsbsimProcIsRealtime then pure "--realtime" else mempty
  in do
    procRoot <- undefined


    pure . setWorkingDir procRoot . proc (cfg ^. jsbsimProcExec) $ args
