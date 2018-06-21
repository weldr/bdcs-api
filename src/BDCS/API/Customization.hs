-- Copyright (C) 2018 Red Hat, Inc.
--
-- This file is part of bdcs-api.
--
-- bdcs-api is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- bdcs-api is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with bdcs-api.  If not, see <http://www.gnu.org/licenses/>.

{-| Customizations applies to the content of an export -}
module BDCS.API.Customization(RecipeCustomization(..),
                              RecipeSshKey(..),
                              emptyCustomization,
                              processCustomization)
 where

import           BDCS.DB
import           BDCS.Export.Customize(Customization(..))
import           Control.Monad.IO.Class(MonadIO, liftIO)
import           Data.Aeson
import           Data.Bits((.|.))
import qualified Data.ByteString.Char8 as C8
import           Data.Maybe(catMaybes)
import qualified Data.Text as T
import           Data.Time.Clock(getCurrentTime)
import           Data.Time.Clock.POSIX(POSIXTime, utcTimeToPOSIXSeconds)
import           System.FilePath.Posix((</>))
import           System.Posix.Files(directoryMode, regularFileMode)

-- | Recipe customization commands
data RecipeCustomization =
    RecipeCustomization { rcHostName :: Maybe String    -- ^ System hostname
                        , rcSshKeys  :: [RecipeSshKey]  -- ^ Ssh keys to install
    } deriving (Eq, Show)

instance FromJSON RecipeCustomization where
  parseJSON = withObject "recipe customization" $ \o -> do
      rcHostName <- o .:? "hostname"
      rcSshKeys  <- o .:? "sshkey".!= []
      return RecipeCustomization{..}

instance ToJSON RecipeCustomization where
  toJSON RecipeCustomization{..} = let
      maybeHostname = ("hostname" .=) <$> rcHostName
      maybeSshKeys  = if null rcSshKeys then Nothing
                      else Just ("sshkey" .= toJSONList rcSshKeys)
   in object $ catMaybes [maybeHostname, maybeSshKeys]

emptyCustomization :: RecipeCustomization
emptyCustomization = RecipeCustomization Nothing []

-- | A sshkey customization
data RecipeSshKey =
    RecipeSshKey { rcSshUser :: String -- ^ User to which to apply the key
                 , rcSshKey  :: String -- ^ Key to install
    } deriving (Eq, Show)

instance FromJSON RecipeSshKey where
  parseJSON = withObject "ssh key" $ \o -> do
      rcSshUser <- o .: "user"
      rcSshKey  <- o .: "key"
      return RecipeSshKey{..}

instance ToJSON RecipeSshKey where
  toJSON RecipeSshKey{..} = object [
      "user" .= rcSshUser
    , "key"  .= rcSshKey ]


-- | Convert a 'RecipeCustomization' block into a list of bdcs 'Customization' directives
processCustomization :: MonadIO m => RecipeCustomization -> m [Customization]
processCustomization RecipeCustomization{..} = do
    currentTime <- utcTimeToPOSIXSeconds <$> liftIO getCurrentTime
    let hostnameCustomization = maybe [] (\h -> [processHostname currentTime h]) rcHostName
        sshKeyCustomizations  = concatMap (processSshKey currentTime) rcSshKeys
    return $ hostnameCustomization ++ sshKeyCustomizations
 where
    processHostname :: POSIXTime -> String -> Customization
    processHostname currentTime hostname =
        let hostnameData = C8.pack $ hostname ++ "\n"
            hostnameFile = Files "/etc/hostname"
                                 "root"
                                 "root"
                                 (floor currentTime)
                                 Nothing
                                 (fromIntegral $ regularFileMode .|. 0o0644)
                                 (C8.length hostnameData)
                                 Nothing
         in WriteFile hostnameFile (Just hostnameData)

    processSshKey :: POSIXTime -> RecipeSshKey -> [Customization]
    processSshKey currentTime RecipeSshKey{..} =
        let keyData = C8.pack $ rcSshKey ++ "\n"
            keyDir  = Files (T.pack $ "/home" </> rcSshUser </> ".ssh")
                            (T.pack rcSshUser)
                            (T.pack rcSshUser)
                            (floor currentTime)
                            Nothing
                            (fromIntegral $ directoryMode .|. 0o0700)
                            0
                            Nothing
            keyFile = Files (T.pack $ "/home" </> rcSshUser </> ".ssh" </> "authorized_keys")
                            (T.pack rcSshUser)
                            (T.pack rcSshUser)
                            (floor currentTime)
                            Nothing
                            (fromIntegral $ regularFileMode .|. 0o0644)
                            (C8.length keyData)
                            Nothing
         in [WriteFile keyDir Nothing, WriteFile keyFile (Just keyData)]
