{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BDCS.API.Results(guardReturnResults,
                        returnImage,
                        returnResults)
 where

import           BDCS.API.Compose(ComposeStatus(..), mkComposeStatus)
import           BDCS.API.Config(ServerConfig(..))
import           BDCS.API.Error(createAPIError)
import           BDCS.API.QueueStatus(queueStatusEnded)
import qualified Codec.Archive.Tar as Tar
import qualified Control.Exception as CE
import           Control.Monad(filterM)
import           Control.Monad.Except(liftIO, runExceptT, throwError)
import qualified Data.ByteString.Lazy as LBS
import           Data.String.Conversions(cs)
import           GHC.TypeLits(KnownSymbol)
import           Servant
import           System.Directory(doesFileExist)
import           System.FilePath.Posix((</>))

guardReturnResults :: ServerConfig -> String -> Handler ComposeStatus
guardReturnResults ServerConfig{..} uuid = do
    result <- liftIO $ runExceptT $ mkComposeStatus cfgResultsDir (cs uuid)
    case result of
        Left _                    -> throwError $ createAPIError err400 False [cs uuid ++ " is not a valid build UUID"]
        Right s@ComposeStatus{..} ->
            if not (queueStatusEnded csQueueStatus)
            then throwError $ createAPIError err400 False ["Build " ++ cs uuid ++ " not in FINISHED or FAILED state."]
            else return s

returnImage :: ServerConfig -> String -> Handler (FilePath, LBS.ByteString)
returnImage cfg@ServerConfig{..} uuid = do
    ComposeStatus{..} <- guardReturnResults cfg (cs uuid)

    liftIO (readArtifactFile $ cfgResultsDir </> cs uuid) >>= \case
        Nothing -> throwError $ createAPIError err400 False ["Build " ++ cs uuid ++ " is missing image file."]
        Just fn -> do contents <- liftIO $ LBS.readFile (cfgResultsDir </> fn)
                      return (fn, contents)
 where
    readArtifactFile :: FilePath -> IO (Maybe String)
    readArtifactFile dir =
        CE.catch (Just <$> readFile (dir </> "ARTIFACT"))
                 (\(_ :: CE.IOException) -> return Nothing)

returnResults :: KnownSymbol h => ServerConfig -> String -> Maybe FilePath -> [FilePath] -> Handler (Headers '[Header h String] LBS.ByteString)
returnResults cfg@ServerConfig{..} uuid resultSuffix files = do
    let composeResultsDir = cfgResultsDir </> cs uuid

    ComposeStatus{..} <- guardReturnResults cfg uuid
    files'            <- filterM (\f -> liftIO $ doesFileExist (composeResultsDir </> f)) files
    tar               <- liftIO $ Tar.pack composeResultsDir files'

    case resultSuffix of
        Just suffix -> return $ addHeader ("attachment; filename=" ++ uuid ++ "-" ++ suffix ++ ".tar;") (Tar.write tar)
        Nothing     -> return $ addHeader ("attachment; filename=" ++ uuid ++ ".tar;") (Tar.write tar)
