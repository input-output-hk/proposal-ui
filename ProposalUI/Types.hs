{-# LANGUAGE OverloadedStrings #-}

module ProposalUI.Types
  ( DownloadVersionInfo(..)
  , MenuChoices(..)
  , ProposalUIState(..)
  , DownloadVersionJson(..)
  , InstallerData(..)
  , ClusterConfig(..)
  ) where

import           Data.Aeson         (ToJSON, toJSON, object, (.=), FromJSON(parseJSON), withObject, (.:))
import Turtle (Text, FilePath)

import Brick (EventM)
import qualified Brick.Widgets.List as L

import Arch (ArchMap)
import Types (Name, DialogReply)
import UpdateLogic (InstallersResults, BucketInfo)

import Iohk.Types (Environment)
import qualified Buildkite

-- | Intermediate data type for the daedalus download json file.
data DownloadVersionInfo = DownloadVersionInfo
  { dviVersion   :: Text
  , dviURL       :: Text
  , dviHash      :: Text
  , dviSHA256    :: Text
  , dviSignature :: Maybe Text
  } deriving Show

instance ToJSON DownloadVersionInfo where
  toJSON dvi = object
    [ "version"   .= dviVersion dvi
    , "URL"       .= dviURL dvi
    , "hash"      .= dviHash dvi
    , "SHA256"    .= dviSHA256 dvi
    , "signature" .= dviSignature dvi
    ]

data DownloadVersionJson = DownloadVersionJson
  { dvjDvis         :: ArchMap DownloadVersionInfo
  , dvjReleaseNotes :: Maybe Text
  } deriving Show

instance ToJSON DownloadVersionJson where
  toJSON (DownloadVersionJson dvis releaseNotes) = object [ "platforms" .= dvis, "release_notes" .= releaseNotes ]

data MenuChoices = SetDaedalusRev | ChooseBuildkiteBuild | FindInstallers | SignInstallers | S3Upload | UpdateVersionJSON | RehashInstallers | SelectCluster | SetGPGUser | LocalInstallers deriving Show

data ProposalUIState = ProposalUIState
  { psDaedalusRev :: Maybe String
  , psBuildkiteBuild :: Maybe Buildkite.BuildNumber
  , psCallback :: EventM Name DialogReply
  , psMenuState :: L.List Name MenuChoices
  , psInstallers :: Maybe InstallerData
  , psOutputDir :: Turtle.FilePath
  , psDownloadVersionInfo :: Maybe (ArchMap DownloadVersionInfo)
  , psBucket :: BucketInfo
  , psGPGUser :: Maybe Text
  , psEnvironment :: Environment
  }

data InstallerData = InstallerData
  { idResults :: InstallersResults
  }

data ClusterConfig = ClusterConfig
  { ccBucket :: Text
  , ccBucketURL :: Text
  , ccEnvironment :: Environment
  }

instance FromJSON ClusterConfig where
  parseJSON = withObject "ClusterConfig" $ \o -> ClusterConfig
    <$> o .: "bucket"
    <*> o .: "bucket-url"
    <*> o .: "environment"
