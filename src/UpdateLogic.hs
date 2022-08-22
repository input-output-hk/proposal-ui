{-# OPTIONS_GHC -Weverything -Wno-unsafe -Wno-implicit-prelude -Wno-missing-local-signatures #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE NamedFieldPuns #-}

module UpdateLogic
  ( getInstallersResults
  , InstallersResults(..)
  , CIResult2(..)
  , CIResult(..)
  , CISystem(..)
  , BucketInfo(..)
  , InstallerPredicate
  , installerPredicates
  , selectBuildNumberPredicate
  , updateVersionJson
  , runAWS'
  , uploadHashedInstaller
  , uploadSignature
  , githubWikiRecord
  , printInstallersResults
  , hashInstallers
  ) where

import           Appveyor                         (AppveyorArtifact (AppveyorArtifact),
                                                   build, buildNumber,
                                                   fetchAppveyorArtifacts,
                                                   fetchAppveyorBuild,
                                                   getArtifactUrl, jobId,
                                                   parseCiUrl, unBuildNumber,
                                                   unJobId)
import qualified Appveyor
import           Buildkite.API                    (APIToken (APIToken),
                                                   Artifact, artifactFilename,
                                                   artifactSha1sum,
                                                   listArtifactsForBuild)
import qualified Buildkite.API                    as BK
import           Control.Applicative              ((<|>))
import           Control.Exception                (try)
import           Control.Lens                     (to, (^.), set, (<&>))
import qualified Control.Lens                     as Lens
import           Control.Monad                    (forM_, guard)
import           Control.Monad.IO.Class           (liftIO)
import Crypto.Hash (Digest, Blake2b_256, hash, SHA256)
import           Control.Monad.Trans.Resource     (runResourceT)
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString                  as BS
import qualified Data.HashMap.Strict              as HashMap
import           Data.List                        (find, nub)
import           Data.Monoid                      ((<>))
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import qualified Filesystem.Path.CurrentOS        as FP
import           GHC.Generics                     (Generic)
import           Github                           (Rev, Status, context,
                                                   fetchGithubStatus, statuses,
                                                   targetUrl)
import           Network.AWS                      (AWS, Credentials (Discover), newLogger,
                                                   Region, chunkedFile, envLogger,
                                                   defaultChunkSize, newEnv, LogLevel(Debug),
                                                   runAWS, send, toBody, within)
import           Network.AWS.S3.CopyObject        (coACL, copyObject)
import           Network.AWS.S3.GetBucketLocation (gblbrsLocationConstraint,
                                                   getBucketLocation)
import           Network.AWS.S3.PutObject         (poACL, poMetadata, putObject)
import           Network.AWS.S3.Types             (BucketName (BucketName),
                                                   ObjectCannedACL (OPublicRead),
                                                   ObjectKey (ObjectKey),
                                                   constraintRegion)
import           Network.URI                      (parseURI, uriPath)
import           Safe                             (headMay, lastMay, readMay)
import           System.Console.ANSI              (Color (Green),
                                                   ColorIntensity (Dull),
                                                   ConsoleLayer (Foreground),
                                                   SGR (Reset, SetColor),
                                                   setSGR)
import           System.IO.Error                  (ioeGetErrorString)
import           Turtle                           (MonadIO, d, die, format, fp, (</>),
                                                   makeFormat, printf, s, void, FilePath, filename,
                                                   w, (%), Managed)
import System.IO (stdout)

import           InstallerVersions                (GlobalResults (GlobalResults, grApplicationVersion, grCardanoCommit, grCardanoVersion, grDaedalusCommit, grDaedalusVersion),
                                                   InstallerNetwork (InstallerMainnet, InstallerStaging, InstallerTestnet),
                                                   findVersionInfo,
                                                   installerNetwork)
import           Iohk.Types                            (ApplicationVersion)
import           Utils                            (fetchCachedUrl,
                                                   fetchCachedUrlWithSHA1)
import Arch (Arch(Win64, Linux64, Mac64), ApplicationVersionKey, formatArch)

import Temp (installerHash)

data BucketInfo = BucketInfo
  { biBucket :: Text
  , biURLBase :: Text
  }

data CIResult = CIResult
  { ciResultSystem      :: CISystem
  , ciResultUrl         :: Text
  , ciResultDownloadUrl :: Text
  , ciResultBuildNumber :: Int
  , ciResultArch        :: Arch
  , ciResultSHA1Sum     :: Maybe Text
  , ciResultFilename    :: Turtle.FilePath
  } deriving (Show, Generic)

data CIResult2 = CIFetchedResult
  { cifResult :: CIResult
  , cifLocal :: Turtle.FilePath
  , cifBlakeCbor :: Digest Blake2b_256
  , cifSha256 :: Digest SHA256
  } deriving Show

data CISystem = Buildkite | AppVeyor deriving (Show, Eq, Bounded, Enum, Generic)

data InstallersResults = InstallersResults
  { ciResults    :: [CIResult2]
  , globalResult :: GlobalResults
  } deriving (Show, Generic)


-- | Allow selection of which CI artifacts to download.
type InstallerPredicate = CIResult -> Bool

-- | An 'InstallerPredicate' which filters by build numbers from the
-- CI system. Useful when the CI has multiple builds for a single git
-- revision. 'Nothing' designates no filter.
selectBuildNumberPredicate :: Maybe Int -- ^ Buildkite build number
                           -> Maybe Int -- ^ AppVeyor build number
                           -> InstallerPredicate
selectBuildNumberPredicate bk av =
  installerPredicates (buildNum Buildkite bk) (buildNum AppVeyor av)
  where
    buildNum _ Nothing    _ = True
    buildNum ci (Just num) r = ciResultSystem r /= ci || ciResultBuildNumber r == num

-- | Joins two installer predicates so that both must be satisfied.
installerPredicates :: InstallerPredicate -> InstallerPredicate -> InstallerPredicate
installerPredicates p q a = p a && q a

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
    advice = "Obtain an API access token with read_builds scope from\n" <>
             "https://buildkite.com/user/api-access-tokens\n" <>
             "Exiting!" :: Text
    st = makeFormat T.pack

cdnLink :: Text -> ObjectKey -> Text
cdnLink cInstallerURLBase (ObjectKey key) = mconcat [ "https://", cInstallerURLBase, "/", key ]

buildkiteTokenFile = "static/buildkite_token" :: String

realFindInstallers :: InstallerPredicate -> Rev -> Turtle.FilePath -> IO (Either Text [CIResult2])
realFindInstallers instP daedalusRev destDir = do
  eBuildkiteToken <- liftIO $ loadBuildkiteToken
  case eBuildkiteToken of
    Right buildkiteToken -> do
      st <- liftIO $ statuses <$> fetchGithubStatus "input-output-hk" "daedalus" daedalusRev
      let
        findStatus :: Status -> IO [CIResult2]
        findStatus status = do
          x <- findInstallersFromStatus buildkiteToken status
          -- handleCIResults is what does the fetch
          handleCIResults instP destDir x
      (Right . concat) <$> mapM findStatus st
    Left err -> pure $ Left err

getInstallersResults :: ApplicationVersionKey -> InstallerPredicate -> Rev -> Turtle.FilePath -> Managed InstallersResults
getInstallersResults keys instP daedalusRev destDir = do
  eciResults <- liftIO $ realFindInstallers instP daedalusRev destDir
  case eciResults of
    Right ciResults -> do
      let
        getInner :: CIResult2 -> CIResult
        getInner CIFetchedResult{cifResult} = cifResult
        innerResults = map getInner ciResults
      globalResult <- findVersionInfo keys daedalusRev
      liftIO $ validateCIResultCount innerResults
      pure $ InstallersResults ciResults globalResult

handleCIResults :: InstallerPredicate -> Turtle.FilePath -> Either Text [CIResult] -> IO [CIResult2]
handleCIResults instP destDir (Right rs) = do
  print rs
  let rs' = filter instP rs
  mapM_ (\result -> do
    print result
    print $ (installerNetwork . ciResultFilename) result
    ) rs
  print rs'
  fetchCIResults destDir rs'
handleCIResults _ _ (Left msg) = T.putStrLn msg >> pure []

validateCIResultCount :: [CIResult] -> IO ()
validateCIResultCount rs = forM_ (ciResultsBySystem rs) $ \(ci, rs') -> let
  urls = nub $ map ciResultUrl rs'
  in case length urls of
       0 -> printf ("warning: There are no CI results for "%w%"!\n") ci
       1 -> printf ("Found a single CI result for "%w%".\n") ci
       n -> do
         printf ("error: Found too many ("%d%") CI results for "%w%"!\n") n ci
         printf "The following builds were found:\n"
         forM_ urls $ printf ("  "%s%"\n")
         die $ format ("Use the --"%w%"-build-num option to choose one.\nExiting.") ci

ciResultsBySystem :: [CIResult] -> [(CISystem, [CIResult])]
ciResultsBySystem rs = [ (ci, filter ((== ci) . ciResultSystem) rs)
                       | ci <- enumFromTo minBound maxBound ]

buildkiteOrg     = "input-output-hk" :: Text
pipelineDaedalus = "daedalus"        :: Text

bkArtifactInstallerArch :: Artifact -> Maybe Arch
bkArtifactInstallerArch art | T.isSuffixOf ".pkg" fn = Just Mac64
                            | T.isSuffixOf ".bin" fn = Just Linux64
                            | T.isSuffixOf ".exe" fn = Just Win64
                            | otherwise = Nothing
  where fn = artifactFilename art

findInstallersBuildKite :: APIToken -> Int -> T.Text
                        -> IO (Either Text [CIResult])
findInstallersBuildKite apiToken buildNum buildUrl = do
  let buildDesc = format ("Buildkite build #"%d) buildNum
  arts <- listArtifactsForBuild apiToken buildkiteOrg pipelineDaedalus buildNum
  let arts' = [ (art, arch) | (art, Just arch) <- [ (art, bkArtifactInstallerArch art) | art <- arts ] ]
  let filterOutAarch64 = \(art, _) -> not ("aarch64" `T.isInfixOf` artifactFilename art)
  forInstallers buildDesc filterOutAarch64 arts' $ \(art, arch) -> do
    -- ask Buildkite what the download URL is
    url <- BK.getArtifactURL apiToken buildkiteOrg pipelineDaedalus buildNum art
    pure $ CIResult
      { ciResultSystem = Buildkite
      , ciResultUrl = buildUrl
      , ciResultDownloadUrl = url
      , ciResultBuildNumber = buildNum
      , ciResultArch = arch
      , ciResultSHA1Sum = (Just $ artifactSha1sum art)
      , ciResultFilename = FP.fromText (artifactFilename art)
      }

avArtifactIsInstaller :: AppveyorArtifact -> Bool
avArtifactIsInstaller (AppveyorArtifact _ name) = name == "Daedalus Win64 Installer"

findInstallersAppVeyor :: Text
                       -> Appveyor.Username -> Appveyor.Project -> ApplicationVersion
                       -> IO (Either Text [CIResult])
findInstallersAppVeyor url user project version = do
  appveyorBuild <- fetchAppveyorBuild user project version
  let jobid = appveyorBuild ^. build . Appveyor.jobs . to head . jobId
      jobDesc = format ("AppVeyor job "%s) (unJobId jobid)
  artifacts <- fetchAppveyorArtifacts jobid
  forInstallers jobDesc avArtifactIsInstaller artifacts $ \(AppveyorArtifact art _) ->
    pure CIResult
      { ciResultSystem      = AppVeyor
      , ciResultUrl         = url
      , ciResultDownloadUrl = getArtifactUrl jobid art
      , ciResultBuildNumber = appveyorBuild ^. build . buildNumber . to unBuildNumber
      , ciResultArch        = Win64
      , ciResultSHA1Sum     = Nothing
      , ciResultFilename    = filename (FP.fromText art)
      }

-- | Download artifacts into the nix store.
fetchCIResults :: Turtle.FilePath -> [CIResult] -> IO [CIResult2]
fetchCIResults destDir = mapM fetchResult
  where
    fetchResult :: CIResult -> IO CIResult2
    fetchResult r@CIResult{..} = do
      let
        localDest = destDir </> ciResultFilename
        fetchCached :: Text -> Turtle.FilePath -> Turtle.FilePath -> IO ()
        fetchCached = case ciResultSHA1Sum of
                            Just sha1 -> fetchCachedUrlWithSHA1 sha1
                            Nothing   -> fetchCachedUrl
      fetchCached ciResultDownloadUrl (filename ciResultFilename) localDest
      (blakecbor, sha256) <- hashInstallers localDest
      pure $ CIFetchedResult r localDest blakecbor sha256

hashInstallers :: Turtle.FilePath -> IO (Digest Blake2b_256, Digest SHA256)
hashInstallers path = do
  printf ("reading " % fp % " into ram...\n") path
  rawData <- BS.readFile $ FP.encodeString path
  printf "HASHing it twice...\n"
  let
    blakecbor = installerHash rawData
    sha256 :: Digest SHA256
    sha256 = hash rawData
  printf "done\n"
  pure (blakecbor, sha256)

forInstallers :: Text -> (a -> Bool) -> [a] -> (a -> IO CIResult) -> IO (Either Text [CIResult])
forInstallers job p arts action = case filter p arts of
  [] -> pure $ Left $ if null arts
    then "No artifacts for " <> job
    else "Installer package file not found in artifacts of " <> job
  files -> Right <$> mapM showResult files
    where showResult a = do
            b <- action a
            printCIResult b
            pure b

printCIResult :: CIResult -> IO ()
printCIResult CIResult{..} = do
  printf (w%" "%w%" URL: ") ciResultSystem ciResultArch
  setSGR [ SetColor Foreground Dull Green ]
  T.putStrLn ciResultUrl
  setSGR [ Reset ]

  printf (w%" "%w%" Installer: ") ciResultSystem ciResultArch
  setSGR [ SetColor Foreground Dull Green ]
  T.putStrLn ciResultDownloadUrl
  setSGR [ Reset ]

formatCIResults :: [CIResult2] -> Text
formatCIResults rs = T.unlines $ ["CI links:"] ++ ciLinks
                     ++ [""] ++ instLinks InstallerMainnet
                     ++ [""] ++ instLinks InstallerStaging
                     ++ [""] ++ instLinks InstallerTestnet
  where
    getInner :: CIResult2 -> CIResult
    getInner CIFetchedResult{cifResult} = cifResult
    innerResults = map getInner rs
    ciLinks :: [Text]
    ciLinks = nub $ map (("* " <>) . ciResultUrl) innerResults
    instLinks :: InstallerNetwork -> [Text]
    instLinks net = (format (w%" installers:") net:[ fmt res | res <- innerResults, isNet net res ])
    isNet :: InstallerNetwork -> CIResult -> Bool
    isNet net = (== Just net) . installerNetwork . ciResultFilename
    fmt :: CIResult -> Text
    fmt res = format (s%" - "%s) (formatArch $ ciResultArch res) (ciResultDownloadUrl res)

formatVersionInfo :: GlobalResults -> Text
formatVersionInfo GlobalResults{..} = T.unlines
  [ format ("Daedalus version:   "%s) grDaedalusVersion
  , format ("Daedalus rev:       "%s) grDaedalusCommit
  , ""
  , format ("Cardano SL version: "%s) grCardanoVersion
  , format ("Cardano SL rev:     "%s) grCardanoCommit
  , ""
  , format ("applicationVersion: "%d) grApplicationVersion
  ]

printInstallersResults :: InstallersResults -> IO ()
printInstallersResults InstallersResults{..} = T.putStr $ T.unlines
  [rule, formatVersionInfo globalResult, "", formatCIResults ciResults, rule]
  where
    rule = "============================================================" :: Text

data StatusContext = StatusContextAppveyor Appveyor.Username Appveyor.Project ApplicationVersion
                   | StatusContextBuildkite Text Int
                   deriving (Show, Eq)

parseStatusContext :: Status -> Maybe StatusContext
parseStatusContext status = parseAppveyor <|> parseBuildKite
  where
    parseAppveyor = guard isAppveyor >> pure (StatusContextAppveyor user project version)
      where (user, project, version) = parseCiUrl $ targetUrl status
    parseBuildKite, parseAppveyor :: Maybe StatusContext
    parseBuildKite = guard isBuildkite >> do
      let parts = T.splitOn "/" (context status)
      repo <- headMay . tail $ parts
      uri <- parseURI . T.unpack . targetUrl $ status
      lastPart <- lastMay . T.splitOn "/" . T.pack . uriPath $ uri
      buildNum <- readMay . T.unpack $ lastPart
      pure $ StatusContextBuildkite repo buildNum

    isAppveyor = context status == "continuous-integration/appveyor/branch"
    isBuildkite = "buildkite/" `T.isPrefixOf` context status

findInstallersFromStatus :: BK.APIToken -> Status -> IO (Either Text [CIResult])
findInstallersFromStatus buildkiteToken status =
  case parseStatusContext status of
    Just (StatusContextBuildkite _repo buildNum) ->
      findInstallersBuildKite buildkiteToken buildNum (targetUrl status)
    Just (StatusContextAppveyor user project version) ->
      findInstallersAppVeyor (targetUrl status) user project version
    Nothing -> pure $ Left $ "unrecognized CI status: " <> context status

githubWikiRecord :: InstallersResults -> Text
githubWikiRecord InstallersResults{..} = T.intercalate " | " cols <> "\n"
  where
    cols = [ format w $ grApplicationVersion globalResult
           , ""
           , githubLink grDaedalusCommit "daedalus"
           , githubLink grCardanoCommit "cardano-sl"
           , ciLink Mac64
           , ciLink Win64
           , "DATE TIME" ]

    githubLink rev project = githubLink' (rev globalResult) project
    githubLink' rev project = mdLink (T.take 6 rev) (format ("https://github.com/input-output-hk/"%s%"/commit/"%s) project rev)

    ciLink arch = maybe "*missing*" ciLink' $ find ((== arch) . ciResultArch) innerResults
    ciLink' CIResult{..} = mdLink (format d ciResultBuildNumber) ciResultUrl

    mdLink = format ("["%s%"]("%s%")")

    getInner :: CIResult2 -> CIResult
    getInner CIFetchedResult{cifResult} = cifResult
    innerResults = map getInner ciResults

updateVersionJson :: BucketInfo -> LBS.ByteString -> IO Text
updateVersionJson BucketInfo{biURLBase,biBucket} json = runAWS' . withinBucketRegion bucketName $ \_ -> do
  uploadFile json key
  pure $ cdnLink biURLBase key
  where
    key = ObjectKey "daedalus-latest-version.json"
    bucketName = BucketName biBucket
    uploadFile :: LBS.ByteString -> ObjectKey -> AWS ()
    uploadFile body remoteKey = void . send $
      makePublic $ putObject bucketName remoteKey (toBody body)
    makePublic = Lens.set poACL (Just OPublicRead)

bucketRegion :: BucketName -> AWS Region
bucketRegion = fmap getRegion . send . getBucketLocation
  where getRegion lc = constraintRegion (lc ^. gblbrsLocationConstraint)

runAWS' :: MonadIO io => AWS a -> io a
runAWS' action = liftIO $ do
  lgr  <- newLogger Debug stdout
  env <- newEnv Discover <&> set envLogger lgr
  runResourceT . runAWS env $ action

withinBucketRegion :: BucketName -> (Region -> AWS a) -> AWS a
withinBucketRegion bucketName action = do
  region <- bucketRegion bucketName
  within region $ action region

type AWSMeta = HashMap.HashMap Text Text

uploadHashedInstaller :: BucketInfo -> Turtle.FilePath -> GlobalResults -> (Text,Text) -> AWS Text
uploadHashedInstaller BucketInfo{biURLBase,biBucket} localPath GlobalResults{grDaedalusCommit,grCardanoCommit,grApplicationVersion} (hash', filename') = do
  let
    bucket = BucketName biBucket
    meta = HashMap.fromList
      [ ("daedalus-revision", grDaedalusCommit)
      , ("cardano-revision", grCardanoCommit)
      , ("application-version", (T.pack. show) grApplicationVersion)
      ] :: AWSMeta
    hashedPath = biBucket <> "/" <> hash'
    file1 = ObjectKey hash'
    file2 = ObjectKey filename'
  withinBucketRegion bucket $ \_ -> do
    uploadOneFile bucket localPath file1 meta
    copyObject' hashedPath file2
    pure $ cdnLink biURLBase file2

  where
    copyObject' :: Text -> ObjectKey -> AWS ()
    copyObject' source dest = void . send $ Lens.set coACL (Just OPublicRead) $ copyObject (BucketName biBucket) source dest

uploadSignature :: BucketInfo -> Turtle.FilePath -> AWS ()
uploadSignature BucketInfo{biBucket} localPath = withinBucketRegion bucketName . const $
  uploadOneFile bucketName localPath (simpleKey localPath) mempty
  where bucketName = BucketName biBucket

-- | S3 object key is just the base name of the filepath.
simpleKey :: Turtle.FilePath -> ObjectKey
simpleKey = ObjectKey . format fp . FP.filename

uploadOneFile :: BucketName -> Turtle.FilePath -> ObjectKey -> AWSMeta -> AWS ()
uploadOneFile bucket localPath remoteKey meta = do
  bdy <- chunkedFile defaultChunkSize (FP.encodeString localPath)
  void . send $ makePublic $ Lens.set poMetadata meta $ putObject bucket remoteKey bdy
  where
    makePublic = Lens.set poACL (Just OPublicRead)
