{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE NamedFieldPuns #-}

module Buildkite (loadBuildkiteToken
                 , buildkiteTokenFile
                 , BuildNumber (BuildNumber)
                 , APIToken(APIToken)
                 , Artifact(artifactFilename, artifactSha1sum)
                 , listBuildsForCommit
                 , listArtifactsForBuild
                 , getArtifactURL
                 , getBuildNumber
                 , hrBuildUrl
                 ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Safe                             (headMay, lastMay, readMay)
import           Control.Exception                (try)
import           System.IO.Error                  (ioeGetErrorString)
import           Turtle                           (MonadIO, d, die, format, fp, (</>),
                                                   makeFormat, printf, s, void, FilePath, filename,
                                                   w, (%), Managed)

import Buildkite.API ( APIToken(APIToken)
                     , Artifact(artifactFilename, artifactSha1sum)
                     , BuildNumber (BuildNumber)
                     , listBuildsForCommit
                     , listArtifactsForBuild
                     , getArtifactURL
                     , getBuildNumber
                     , hrBuildUrl
                     )

-- | Read the Buildkite token from a config file. This file is not
-- checked into git, so the user needs to create it themself.
-- If the file isn't present, the program exits.
loadBuildkiteToken :: IO (Either Text APIToken)
loadBuildkiteToken = try (T.readFile buildkiteTokenFile) >>= \case
  Right contents -> case process contents of
    Just token -> pure $ Right $ APIToken token
    Nothing -> pure $ Left $ format (st%" was empty.\n"%s) buildkiteTokenFile advice
  Left (e :: IOError) -> die $ format ("Could not read "%st%": "%st%s)
    buildkiteTokenFile (ioeGetErrorString e) advice
  where
    process = headMay . filter (not . T.null) . T.lines
    advice = "Obtain an API access token with read_builds and read_artifacts scope from\n" <>
             "https://buildkite.com/user/api-access-tokens\n" <>
             "Exiting!" :: Text
    st = makeFormat T.pack

buildkiteTokenFile = "static/buildkite_token" :: String
