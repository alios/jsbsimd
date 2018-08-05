{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
module Control.JSBSim.Handler where


import           Control.JSBSim.Database
import           Control.JSBSim.Types
import           Control.Lens.Operators
import           Control.Monad.Logger
import           Control.Monad.Reader
import           Crypto.Hash
import           Crypto.Hash.Algorithms
import qualified Data.ByteString.Lazy    as BL
import           Data.JSBSim.Api
import           Data.JSBSim.Types
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Time.Clock
import           Servant

type SimdM = LoggingT (ReaderT SimdState Handler)




getScripts :: Text -> SimdM [JsbSimScriptName]
getScripts c = do
  return mempty

getScript :: Maybe String -> Text -> Text ->
  SimdM (Headers ResourceHeaders (JsbSimScript Double))
getScript et c n = do
  r <- lookupJsbSimScript c n
  case r of
    Nothing -> throwError err404
    Just r' ->
      let dig = r' ^. scriptETag
          s = r' ^. scriptScript
          t = r' ^. scriptLastModified
      in if et == Just (show dig) then throwError err304
         else do
        te <- addUTCTime 600 <$> liftIO getCurrentTime
        return . addHeader (T.pack . show $ dig) . addHeader t  . addHeader te $ s



postScript :: Text -> JsbSimScript Double ->
  SimdM (Headers '[Header "Location" Text, Header "ETag" Text] NoContent)
postScript c s =
  let sn = s ^. scriptName
      ll = mconcat ["/", toUrlPiece $ getScriptLink c sn]
  in do
    et <- postScript' c s
    pure . addHeader ll . addHeader (T.pack . show $ et) $ NoContent


postScript' :: Text -> JsbSimScript Double -> SimdM (Digest SHA3_256)
postScript' c s =
  let sn = s ^. scriptName
  in do
    r <- existsJsbSimScript c sn
    if r then throwError $ err409 {
        errBody = mconcat [
            "a script with name '", BL.fromStrict $ T.encodeUtf8 sn,
            "' already exists in collection ", BL.fromStrict $ T.encodeUtf8 c
            ]
        }
      else do
        (oid, dig) <- insertJsbSimScript c s
        logInfoN . mconcat $ [
          "postScript inserted '", sn, "' to ", c,
          " (oid:", T.pack . show $ oid, ")" ]
        return dig
