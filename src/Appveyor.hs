{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude -Wno-semigroup #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}

module Appveyor (
JobId,
BuildNumber,
Username,
Project,
build,
buildNumber,
jobs,
version,
jobId,
name,
fileName,
getArtifactUrl,
fetchAppveyorBuild,
fetchAppveyorArtifacts,
parseCiUrl,
AppveyorArtifact (AppveyorArtifact),
unBuildNumber,
unJobId
) where

import           Control.Lens.TH (makeFields)
import           Data.Aeson      (FromJSON, parseJSON, withObject, (.:))
import           Data.Monoid     ((<>))
import           Data.String     (IsString)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)
import           Iohk.Types      (ApplicationVersion (ApplicationVersion, getApplicationVersion))
import           Utils           (fetchJson)

newtype JobId = JobId { unJobId :: T.Text } deriving (Show, Eq, Monoid, Semigroup)
newtype BuildNumber = BuildNumber { unBuildNumber :: Int } deriving (Show, Eq)
newtype Username = Username { usernameToText :: T.Text } deriving (Show, Eq, Monoid, IsString, Semigroup)
newtype Project = Project { projectToText :: T.Text } deriving (Show, Eq, Monoid, IsString, Semigroup)

data ProjectBuildResults = ProjectBuildResults {
    _projectBuildResultsBuild :: Build
    } deriving (Show, Generic)

data Build = Build {
      _buildJobs        :: [ BuildJob ]
    , _buildBuildNumber :: BuildNumber
    , _buildVersion     :: ApplicationVersion
    } deriving (Show, Generic)

data BuildJob = BuildJob {
      _buildJobJobId :: JobId
  } deriving (Show, Generic)

data AppveyorArtifact = AppveyorArtifact {
      _appveyorArtifactFileName :: T.Text
    , _appveyorArtifactName     :: T.Text
  } deriving (Show, Generic, Eq)

makeFields ''ProjectBuildResults
makeFields ''Build
makeFields ''BuildJob
makeFields ''AppveyorArtifact

instance FromJSON ProjectBuildResults where
  parseJSON = withObject "AppveyorBuild" $ \v -> ProjectBuildResults
    <$> v .: "build"
instance FromJSON Build where
  parseJSON = withObject "AppveyorBuild2" $ \v -> Build
    <$> v .: "jobs"
    <*> (BuildNumber <$> v .: "buildNumber")
    <*> (ApplicationVersion <$> v .: "version")
instance FromJSON BuildJob where
  parseJSON = withObject "AppveyorBuild3" $ \v -> BuildJob
    . JobId <$> v .: "jobId"
instance FromJSON AppveyorArtifact where
  parseJSON = withObject "AppveyorArtifact" $ \v -> AppveyorArtifact
    <$> v .: "fileName"
    <*> v .: "name"

getArtifactUrl :: JobId -> T.Text -> T.Text
getArtifactUrl (JobId jobid) filename = "https://ci.appveyor.com/api/buildjobs/" <> jobid <> "/artifacts/" <> filename

-- input: "https://ci.appveyor.com/api/projects/jagajaga/daedalus/build/0.6.3356"
-- /projects/{accountName}/{projectSlug}/build/{buildVersion}:
-- from: https://github.com/kevinoid/appveyor-swagger/blob/master/swagger.yaml
fetchAppveyorBuild :: Username -> Project -> ApplicationVersion -> IO ProjectBuildResults
fetchAppveyorBuild user project version' = fetchJson $ "https://ci.appveyor.com/api/projects/" <> T.intercalate "/" [ usernameToText user, projectToText project, "builds", getApplicationVersion version' ]

fetchAppveyorArtifacts :: JobId -> IO [AppveyorArtifact]
fetchAppveyorArtifacts (JobId jobid) = fetchJson $ "https://ci.appveyor.com/api/buildjobs/" <> jobid <> "/artifacts"

-- input: https://ci.appveyor.com/project/jagajaga/daedalus/build/0.6.3356
-- output:
parseCiUrl :: T.Text -> (Username, Project, ApplicationVersion)
parseCiUrl input = case T.splitOn "/" input of
  ["https:", "", "ci.appveyor.com", "project", username, project, "builds", version'] -> (Username username, Project project, ApplicationVersion version')
  other -> error $ show other
