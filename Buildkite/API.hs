{-# LANGUAGE DeriveGeneric, RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}

-- | Module for accessing selected endpoints of Buildkite API.

-- fixme: need to have better error handling and return Eithers from endpoint functions.

module Buildkite.API
  ( APIToken(APIToken)
  , Artifact(artifactFilename, artifactSha1sum)
  , BuildNumber (BuildNumber)
  , listBuildsForCommit
  , listArtifactsForBuild
  , getArtifactURL
  , getBuildNumber
  , hrBuildUrl
  ) where

import           Network.HTTP.Types.URI (parseQuery)
import           Network.HTTP.Types.Header (HeaderName)
import           Network.HTTP.Simple (ResponseHeaders, Query, Response,  Header, Request, getResponseBody, httpJSON, setRequestPath, setRequestHost, setRequestPort, setRequestHeader, setRequestSecure, defaultRequest, httpNoBody, getRequestQueryString, addToRequestQueryString)
import           Network.HTTP.Conduit (redirectCount, responseHeaders, queryString, setQueryString, parseRequest)
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Data.Aeson (FromJSON(parseJSON), Value, genericParseJSON, withObject, withText, defaultOptions, (.:))
import           Data.Aeson.Types (Options(fieldLabelModifier), camelTo2)
import           Network.URI (URI, parseURI)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.String (IsString(fromString))
import Data.Foldable (find)
import Data.Functor
import qualified Data.Map.Strict as M
import Control.Monad.IO.Class (MonadIO)
import Data.Function ((&))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B

import Text.Megaparsec
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L


----------------------------------------------------------------------------
-- types

-- | Buildkite usually uses UUIDs to identify records.
newtype ID = ID { unID :: Text }
  deriving (Generic, Show, Eq)

-- | These tokens are associated with an access scope and can be
-- generated at https://buildkite.com/user/api-access-tokens
newtype APIToken = APIToken Text
  deriving (Eq, Show)

instance IsString APIToken where
  fromString = APIToken . T.pack

instance IsString ID where
  fromString = ID . T.pack

-- | Build artifact record from Buildkite, i.e. a file.
-- The spelling is theirs not mine.
data Artifact = Artifact
  { artifactId           :: ID
  , artifactJobId        :: ID
  , artifactURL          :: URI
  , artifactDownloadURL  :: URI
  , artifactState        :: Text
  , artifactPath         :: Text
  , artifactDirname      :: Text
  , artifactFilename     :: Text
  , artifactMimeType     :: Text
  , artifactFileSize     :: Int
  , artifactGlobPath     :: Maybe Text
  , artifactOriginalPath :: Maybe Text
  , artifactSha1sum      :: Text
  } deriving (Generic, Show, Eq)

data Build = Build
  { buildId          :: ID
  , buildUrl         :: URI  -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds/1",
  , buildWebUrl      :: URI  -- "https://buildkite.com/my-great-org/my-pipeline/builds/1",
  , buildNumber      :: Int   -- 1,
  , buildState       :: Text  -- "passed",
  , buildBlocked     :: Bool  -- false,
  , buildMessage     :: Text  -- "Bumping to version 0.2-beta.6",
  , buildCommit      :: Text  -- "abcd0b72a1e580e90712cdd9eb26d3fb41cd09c8",
  , buildBranch      :: Text  -- "master",
  , buildEnv         :: Value -- { },
  , buildSource      :: Text  -- "webhook",
  , buildJobs        :: [Job]
  , buildCreatedAt   :: Text  -- "2015-05-09T21:05:59.874Z",
  , buildScheduledAt :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , buildStartedAt   :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , buildFinishedAt  :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , buildMetaData    :: Value -- { },
  -- no need to deserialize these structure, so disable for now
  -- , buildCreator     :: User
  -- , buildPipeline    :: Pipeline
  } deriving (Generic, Show, Eq)

data Pipeline = Pipeline
  { pipelineId                              :: ID
  , pipelineUrl                             :: Text       -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline",
  , pipelineWebUrl                          :: Text       -- "https://buildkite.com/my-great-org/my-pipeline",
  , pipelineName                            :: Text       -- "great-pipeline",
  , pipelineSlug                            :: Text       -- "great-pipeline",
  , pipelineRepository                      :: Text       -- "git@github.com:my-great-org/my-pipeline",
  , pipelineProvider                        :: Buildprovider
  , pipelineSkipQueuedBranchBuilds          :: Bool       -- false,
  , pipelineSkipQueuedBranchBuildsFilter    :: Maybe Text -- null,
  , pipelineCancelRunningBranchBuilds       :: Bool       -- false,
  , pipelineCancelRunningBranchBuildsFilter :: Maybe Text -- null,
  , pipelineBuildsUrl                       :: Text       -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds",
  , pipelineBadgeUrl                        :: Text       -- "https://badge.buildkite.com/58b3da999635d0ad2daae5f784e56d264343eb02526f129bfb.svg",
  , pipelineCreatedAt                       :: Text       -- "2015-05-09T21:05:59.874Z",
  , pipelineScheduledBuildsCount            :: Int        -- 0,
  , pipelineRunningBuildsCount              :: Int        -- 0,
  , pipelineScheduledJobsCount              :: Int        -- 0,
  , pipelineRunningJobsCount                :: Int        -- 0,
  , pipelineWaitingJobsCount                :: Int        -- 0
  } deriving (Generic, Show, Eq)

data Buildprovider = Buildprovider
  { buildproviderId         :: Text -- "github",
  , buildproviderWebhookUrl :: Text
  } deriving (Generic, Show, Eq)

data Job = Job
  { jobId              :: ID          -- "b63254c0-3271-4a98-8270-7cfbd6c2f14e",
  , jobType            :: Text        -- "script",
  , jobName            :: Text        -- "scripts/build.sh",
  , jobAgentQueryRules :: [Text]      -- ["*"],
  , jobState           :: Text        -- "passed",
  , jobWebUrl          :: Text        -- "https://buildkite.com/my-great-org/my-pipeline/builds/1#b63254c0-3271-4a98-8270-7cfbd6c2f14e",
  , jobLogUrl          :: Text        -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds/1/jobs/b63254c0-3271-4a98-8270-7cfbd6c2f14e/log",
  , jobRawLogUrl       :: Text        -- "https://api.buildkite.com/v2/organizations/my-great-org/pipelines/my-pipeline/builds/1/jobs/b63254c0-3271-4a98-8270-7cfbd6c2f14e/log.txt",
  , jobCommand         :: Text        -- "scripts/build.sh",
  , jobExitStatus      :: Maybe Int   -- 0,
  , jobCreatedAt       :: Text        -- "2015-05-09T21:05:59.874Z",
  , jobScheduledAt     :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , jobStartedAt       :: Maybe Text  -- "2015-05-09T21:05:59.874Z",
  , jobFinishedAt      :: Maybe Text  -- "2015-05-09T21:05:59.874Z"
  , jobArtifactPaths   :: Maybe Text  -- "",
  -- no need to deserialize this structure, so disable for now
  -- , jobAgent           :: Agent
  } deriving (Generic, Show, Eq)

data ArtifactDownload = ArtifactDownload { unArtifactDownload :: Text }

data BuildNumber = BuildNumber Int
  deriving (Generic, Show, Eq, Ord)

getBuildNumber :: BuildNumber -> Int
getBuildNumber (BuildNumber bn) = bn

----------------------------------------------------------------------------
-- Parser

type Parser = Parsec Void Text

sc :: Parser ()
sc = L.space space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

data PageRelative = PageRelativeLast
                  | PageRelativePrev
                  | PageRelativeNext
                  | PageRelativeFirst
  deriving (Eq, Show, Ord)

type URL = Text
type PageRef = (PageRelative, URL)

pPageRelative :: Parser PageRelative
pPageRelative = do
  _ <- string "rel=\""
  rel <- choice
    [ string "next"  $> PageRelativeNext
    , string "prev"  $> PageRelativePrev
    , string "first" $> PageRelativeFirst
    , string "last"  $> PageRelativeLast
    ]
  _ <- string "\""
  _ <- optional (char ',')
  pure rel

pPageRef :: Parser PageRef
pPageRef = do
  _ <- char '<'
  url <- T.pack <$> manyTill L.charLiteral (char '>')
  _ <- char ';'
  _ <- sc
  pageRel <- pPageRelative
  _ <- sc
  pure $ (pageRel, url)
 
----------------------------------------------------------------------------
-- api calls

-- | Returns list of artifact records for all jobs of the given build.
-- Maximum 30 artifacts. (?page=1&per_page=30)
-- https://buildkite.com/docs/rest-api/artifacts#list-artifacts-for-a-build
listArtifactsForBuild :: APIToken      -- ^ token generated in buildkite settings
                      -> Text          -- ^ organization slug, e.g. input-output-hk
                      -> Text          -- ^ pipeline slug, e.g. iohk-ops
                      -> BuildNumber   -- ^ build number
                      -> IO [Artifact] -- ^ artifact data records belonging to the job
listArtifactsForBuild t org pipeline buildNum = getResponseBody <$> httpJSON req
  where
    req = makeRequest t path
    path = buildPath org pipeline buildNum ++ [ "artifacts" ]

-- | Returns list of build numbers matching commit sha (only works for
-- full sha, not for shortened ones).
listBuildsForCommit
  :: APIToken
  -- ^ Buildkite token for accessing Buildkite APIs, must have access
  -- to read builds of desired organization/project.
  -> Text
  -- ^ organization slug, e.g. input-output-hk
  -> Text
  -- ^ pipeline slug, e.g. iohk-ops
  -> Text
  -- ^ SHA256 long commit hash
  -> IO [BuildNumber]
  -- ^ List of build numbers for commit
listBuildsForCommit t org pipeline commit = handlePagination' t getResponseBody req
  where
    req =
      makeRequest t path
        & addToRequestQueryString ([("commit", Just . B.pack . T.unpack $ commit)])
    path = ["organizations", org, "pipelines", pipeline, "builds"]

-- | Human-friendly build URL
hrBuildUrl
  :: Text
  -- ^ organization slug, e.g. input-output-hk
  -> Text
  -- ^ pipeline slug, e.g. iohk-ops
  -> BuildNumber
  -- ^ build number
  -> Text
  -- ^ URL of that build
hrBuildUrl org pipeline (BuildNumber build) =
  "https://buildkite.com/" <> T.intercalate "/" [org, pipeline, "builds", T.pack $ show build]

handlePagination' :: (FromJSON body, Monoid m) => APIToken -> (Response body -> m) -> Request -> IO m
handlePagination' token f = handlePagination httpJSON (fmap (authRequest token) . parseRequest . T.unpack) f

-- hnPagination
--   :: (Response body -> Request)
--   -- ^ Determine next request from previous response
--   -> m [Response]
-- hnPagination 

-- TODO:
-- - Provide me with some way to determine next URL/queryparams
-- - I'll return list of responses
--
-- - Some way to set page size
-- - Some way to run a request
--
-- | Handle Buildkite API pagination.
handlePagination
  :: (MonadIO m, Monoid a)
  => (Request -> m (Response body))
  -> (Text -> m Request)
  -> (Response body -> a)
  -> Request
  -> m a
handlePagination runReq mkReq respF req = do
  let reqBigPage = setQueryParam "per_page" (Just . B.pack . show $ buildkitePageLimit) $ req
  loop reqBigPage

  where
    loop req = do
      resp <- runReq req
      let currentPage = respF resp
      let mNextUrl = findNextPageUrl $ responseHeaders resp 
      nextPages <- case mNextUrl of
          Left e -> pure mempty
          Right nextUrl -> do
            nextReq <- mkReq nextUrl
            loop nextReq
      pure $ currentPage <> nextPages

    setQueryParam :: ByteString -> Maybe ByteString -> Request -> Request
    setQueryParam key val req = 
      let
        removeQueryParam :: ByteString -> Query -> Query
        removeQueryParam qp = foldr (\(k, v) qps -> if k == qp then qps else (k, v) : qps) []

        newQueryString = ((key, val):) . removeQueryParam key . getRequestQueryString $ req
      in
        setQueryString newQueryString req

    findNextPageUrl :: ResponseHeaders -> Either Text Text
    findNextPageUrl resp = 
      let
        findHeader :: HeaderName -> ResponseHeaders -> Maybe Header
        findHeader hdrName = find (\(headerName, _) -> headerName == hdrName)

        headerBody :: Header -> ByteString
        headerBody = snd

        linkHeader = fmap headerBody . findHeader "Link" $ resp
      in do
        hdr <- maybe (Left "error") (Right) $ fmap (T.pack . B.unpack) linkHeader
        result <- either (Left . T.pack . errorBundlePretty) Right $ parse (many pPageRef) "Buildkite Respnse Link Header" hdr
        maybe (Left "Couldn't find next page") (Right) $ M.lookup PageRelativeNext $ toPageMap result
        -- relMap <- toPageMap <$> parse (many pPageRef) "Buildkite Respnse Link Header" hdr
        -- M.lookup PageRelativeNext relMap 

toPageMap :: [PageRef] -> M.Map PageRelative URL
toPageMap = M.fromList

-- | Buildkite API page limit https://buildkite.com/docs/apis/rest-api#pagination
buildkitePageLimit :: Int
buildkitePageLimit = 100

-- | Returns the URL for downloading an artifact.
-- Although the returned URL might be in our own public S3 bucket,
-- this can't be assumed, and it may only be valid for 60 seconds.
-- https://buildkite.com/docs/rest-api/artifacts#download-an-artifact
getArtifactURL :: APIToken       -- ^ token generated in buildkite settings
               -> Text           -- ^ organization slug, e.g. input-output-hk
               -> Text           -- ^ pipeline slug, e.g. iohk-ops
               -> BuildNumber    -- ^ build number
               -> Artifact       -- ^ Artifact record
               -> IO Text        -- ^ Temporary URL to download artifact from
getArtifactURL t org pipeline buildNum Artifact{..} =
  unArtifactDownload . getResponseBody <$> httpJSON req
  where
    req = makeRequest t path
    path = buildPath org pipeline buildNum ++
           [ "jobs", unID artifactJobId, "artifacts", unID artifactId, "download" ]

----------------------------------------------------------------------------
-- util

authRequest :: APIToken -> Request -> Request
authRequest token = setRequestSecure True
                    . setRequestPort 443
                    . addAuthHeader token
                    . addUserAgentHeader

makeRequest :: APIToken -> [Text] -> Request
makeRequest token path = setRequestPath ("/v2/" <> T.encodeUtf8 (T.intercalate "/" path))
                         $ setRequestHost "api.buildkite.com"
                         $ authRequest token
                         $ setRedirectCount 0
                         $ defaultRequest

buildPath :: Text -> Text -> BuildNumber -> [Text]
buildPath org pipeline (BuildNumber num) =
  [ "organizations", org, "pipelines", pipeline, "builds" , T.pack (show num) ]

addUserAgentHeader :: Request -> Request
addUserAgentHeader = setRequestHeader "User-Agent" ["https://github.com/input-output-hk/iohk-ops"]

addAuthHeader :: APIToken -> Request -> Request
addAuthHeader (APIToken token) = setRequestHeader "Authorization"
  ["Bearer " <> T.encodeUtf8 token]

setRedirectCount :: Int -> Request -> Request
setRedirectCount n req = req { redirectCount = n }

----------------------------------------------------------------------------
-- deserialization from api

instance FromJSON Artifact where
  parseJSON = genericParseJSON (parseOptions "artifact")

instance FromJSON Build where
  parseJSON = genericParseJSON (parseOptions "build")

instance FromJSON Job where
  parseJSON = genericParseJSON (parseOptions "job")

instance FromJSON Pipeline where
  parseJSON = genericParseJSON (parseOptions "pipeline")

instance FromJSON Buildprovider where
  parseJSON = genericParseJSON (parseOptions "buildprovider")

instance FromJSON ID where
  parseJSON v = ID <$> parseJSON v

instance FromJSON ArtifactDownload where
  parseJSON = withObject "ArtifactDownload" $ \o ->
    ArtifactDownload <$> o .: "url"

instance FromJSON URI where
  parseJSON = withText "URI" $ \u -> case parseURI (T.unpack u) of
    Just uri -> return uri
    Nothing -> fail "URI could not be parsed"

instance FromJSON BuildNumber where
  parseJSON = withObject "Build" $ \o -> 
    BuildNumber <$> o .: "number"

parseOptions :: String -> Options
parseOptions prefix = defaultOptions { fieldLabelModifier = dropPrefix . camelTo2 '_' }
  where dropPrefix = drop (length prefix + 1)
