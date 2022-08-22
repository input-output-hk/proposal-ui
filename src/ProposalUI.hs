{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module ProposalUI (spawnProposalUI) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (isJust, fromJust)
import           Control.Monad.Managed (Managed, MonadIO)
import qualified System.Process             as P
import           Turtle (FilePath, Shell, system, empty,single,die,format,(%),w,printf,s,fp, (<.>), testfile, readTextFile, join, fromString, encodeString, with, liftIO, cp, (</>))
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (formatTime, defaultTimeLocale, iso8601DateFormat)
import qualified Data.ByteString.Lazy.Char8 as L8
import           Data.Aeson (Value, encode, toJSON, eitherDecodeFileStrict')
import qualified Filesystem.Path.CurrentOS        as FP
import           System.Directory (listDirectory, createDirectoryIfMissing)

import Brick (Widget, BrickEvent(VtyEvent), EventM, vBox, str, strWrap, txt, padLeftRight)
import qualified Graphics.Vty as V
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as L
import qualified Data.Vector as V


import Types (Dialog(Dialog, dRender, dHandleEvent), AppState, Name(Menu1), DialogReply(DialogReplyContinue, DialogReplyLiftIO), CustomEvent)
import PromptString (spawnPromptString)

import Iohk.Types (Environment(Testnet, Preview, Preprod, Development, Staging, Production, Nightly, ITNBC, ITNRW, MainnetFlight, ShelleyTestnet, MainnetCatalyst))
import Arch (Arch(Win64, Mac64, Linux64), ArchMap, archMapEach, idArchMap, archMapFromList, archMap, lookupArch)
import UpdateLogic (InstallersResults(globalResult, ciResults, InstallersResults), CIResult2(CIFetchedResult, cifBlakeCbor, cifLocal, cifSha256, cifResult)
                   , CIResult(CIResult, ciResultUrl, ciResultDownloadUrl, ciResultBuildNumber, ciResultArch, ciResultFilename, ciResultSystem, ciResultSHA1Sum)
                   , InstallerPredicate, BucketInfo(BucketInfo, biBucket), installerPredicates, selectBuildNumberPredicate, getInstallersResults
                   , CISystem(Buildkite)
                   , updateVersionJson, runAWS', uploadHashedInstaller, uploadSignature, hashInstallers)
import InstallerVersions (GlobalResults(GlobalResults, grCardanoCommit, grDaedalusCommit, grApplicationVersion, grNodeVersion, grCardanoVersion, grDaedalusVersion), installerNetwork, InstallerNetwork(InstallerTestnet, InstallerPreview, InstallerPreprod, InstallerStaging, InstallerMainnet, InstallerNightly, InstallerITNBC, InstallerITNRW, InstallerMainnetFlight, InstallerShelleyTestnet, InstallerMainnetCatalyst))
import Github (Rev)
import Utils (tt)

import ProposalUI.Types (ProposalUIState(psDownloadVersionInfo,psMenuState,psDaedalusRev,psInstallers,psEnvironment,psBucket,psGPGUser,ProposalUIState,psOutputDir,psCallback)
                        , MenuChoices(SetGPGUser, SelectCluster, SetDaedalusRev, FindInstallers, SignInstallers, S3Upload, UpdateVersionJSON, RehashInstallers, LocalInstallers)
                        , InstallerData(InstallerData, idResults), DownloadVersionInfo(DownloadVersionInfo, dviVersion, dviURL, dviHash, dviSignature, dviSHA256)
                        , DownloadVersionJson(DownloadVersionJson), ClusterConfig(ccBucket, ccBucketURL, ccEnvironment))

import FileChooser (spawnFileChooser)

mkProposalUI :: ProposalUIState -> Dialog
mkProposalUI state = Dialog { dRender = renderUI state, dHandleEvent = handleEvents state }

generateNewMenu :: ProposalUIState -> L.List Name MenuChoices
generateNewMenu ProposalUIState{psDaedalusRev,psInstallers,psDownloadVersionInfo} = L.list Menu1 (V.fromList thelist) 1
  where
    thelist = [ SetGPGUser, SelectCluster, SetDaedalusRev ] <> maybeFindInstallers <> maybeSign <> maybeUpload <> maybeSetVersion
    maybeFindInstallers = if (isJust psDaedalusRev) then [ FindInstallers, LocalInstallers ] else []
    maybeSign = if (isJust psInstallers) then [ RehashInstallers, SignInstallers ] else []
    maybeUpload = if (isJust psInstallers) then [ S3Upload ] else []
    maybeSetVersion = if (isJust psDownloadVersionInfo) then [ UpdateVersionJSON ] else []

renderUI :: ProposalUIState -> AppState -> [ Widget Name ]
renderUI ProposalUIState{psMenuState,psInstallers,psDaedalusRev,psDownloadVersionInfo,psEnvironment,psBucket,psGPGUser} _astate = [ root ]
  where
    root :: Widget Name
    root = vBox [ B.borderWithLabel (str "Current State") status, menu ]
    status :: Widget Name
    status = vBox $ [ daedalusRev, currentEnv, currentBucket, gpgUser psGPGUser ] <> (mkInstallers psInstallers)
    daedalusRev :: Widget Name
    daedalusRev = case psDaedalusRev of
      Nothing -> str "No Daedalus Revision set"
      Just rev -> str $ "Daeadalus Revision: " <> rev
    currentEnv = str $ "Environment: " <> show psEnvironment
    currentBucket = txt $ "Bucket: " <> (biBucket psBucket)
    gpgUser (Just user) = txt $ "GPG User: " <> user
    gpgUser Nothing = txt $ "using default gpg"
    mkInstallers :: Maybe InstallerData -> [ Widget Name ]
    mkInstallers (Just InstallerData{idResults = InstallersResults{ciResults,globalResult}}) =
         mkGlobalResult globalResult
      <> (map mkCiResult ciResults) <> [ strWrap $ show psDownloadVersionInfo ]
    mkInstallers Nothing = []
    mkCiResult :: CIResult2 -> Widget Name
    mkCiResult CIFetchedResult{cifLocal,cifResult=CIResult{ciResultUrl,ciResultDownloadUrl,ciResultBuildNumber,ciResultArch}} = B.border $ vBox
      [ str $ "Arch: " <> show ciResultArch
      , str $ "Local Path: " <> show cifLocal
      , txt $ "URL: " <> ciResultUrl
      , txt $ "Download URL: " <> ciResultDownloadUrl
      , str $ "Build#: " <> show ciResultBuildNumber
      --, str $ "SHA1: " <> show ciResultSHA1Sum
      ]
    mkGlobalResult GlobalResults{grCardanoCommit,grDaedalusCommit,grApplicationVersion,grCardanoVersion,grDaedalusVersion,grNodeVersion} =
      [ txt $ "Cardano rev: " <> grCardanoCommit
      , txt $ "Daedalus rev: " <> grDaedalusCommit
      , str $ "ApplicationVersion: " <> (show grApplicationVersion)
      , txt $ "Node Version: " <> grNodeVersion
      , txt $ "Cardano Version: " <> grCardanoVersion
      , txt $ "Daedalus Version: " <> grDaedalusVersion ]
    menu :: Widget Name
    menu = B.borderWithLabel (str "Menu") $ padLeftRight 1 $ L.renderList renderRow True psMenuState
    renderRow :: Bool -> MenuChoices -> Widget Name
    renderRow _ SetGPGUser = str "1: Set GPG User (optional)"
    renderRow _ SelectCluster = str "2: Select Cluster"
    renderRow _ SetDaedalusRev = str "3: Set Daedalus Revision"
    renderRow _ FindInstallers = str "4a: Find Installers"
    renderRow _ LocalInstallers = str "4b: use local installers in installers/"
    renderRow _ RehashInstallers = str "5: Rehash installer (optional)"
    renderRow _ SignInstallers = str "6: Sign installers with GPG (optional)"
    renderRow _ S3Upload = str "7: Upload Installers to S3"
    renderRow _ UpdateVersionJSON = str "8: Set daedalus-latest-version.json"

configurationKeys :: Environment -> Arch -> T.Text
configurationKeys Production Win64   = "mainnet_wallet_win64"
configurationKeys Production Mac64   = "mainnet_wallet_macos64"
configurationKeys Production Linux64 = "mainnet_wallet_linux64"
configurationKeys Staging    Win64   = "mainnet_dryrun_wallet_win64"
configurationKeys Staging    Mac64   = "mainnet_dryrun_wallet_macos64"
configurationKeys Staging    Linux64 = "mainnet_dryrun_wallet_linux64"
configurationKeys Testnet    Win64   = "testnet_wallet_win64"
configurationKeys Testnet    Mac64   = "testnet_wallet_macos64"
configurationKeys Testnet    Linux64 = "testnet_wallet_linux64"
configurationKeys env' _ = error $ "Application versions not used in '" <> show env' <> "' environment"

-- | Step 2a. (Optional) Sign installers with GPG. This will leave
-- .asc files next to the installers which will be picked up in the
-- upload S3 step.
updateProposalSignInstallers :: InstallersResults -> Maybe T.Text -> IO ()
updateProposalSignInstallers InstallersResults{ciResults} userId = do
  mapM_ signInstaller $ map cifLocal $ ciResults
  where
    -- using system instead of procs so that tty is available to gpg
    signInstaller f = system (P.proc "gpg2" $ map T.unpack $ gpgArgs f) empty
    gpgArgs f = userArg ++ ["--detach-sig", "--armor", "--sign", tt f]
    userArg = case userId of
                Just u  -> ["--local-user", u]
                Nothing -> []

-- | Checks if an installer from a CI result matches the environment
-- that iohk-ops is running under.
installerForEnv :: Environment -> CIResult -> Bool
installerForEnv env = matchNet . installerNetwork . ciResultFilename
  where matchNet n = case env of
          Production     -> n == Just InstallerMainnet
          Nightly        -> n == Just InstallerNightly
          ITNBC          -> n == Just InstallerITNBC
          ITNRW          -> n == Just InstallerITNRW
          Staging        -> n == Just InstallerStaging
          Testnet        -> n == Just InstallerTestnet
          Preview        -> n == Just InstallerPreview
          Preprod        -> n == Just InstallerPreprod
          MainnetFlight  -> n == Just InstallerMainnetFlight
          ShelleyTestnet -> n == Just InstallerShelleyTestnet
          MainnetCatalyst -> n == Just InstallerMainnetCatalyst
          Development    -> True
          _              -> False

findInstallers :: Turtle.FilePath -> Environment -> Rev -> IO InstallerData
findInstallers destDir env rev = do
  let
    bkNum = Nothing
    avNum = Nothing
    instP :: InstallerPredicate
    instP = installerPredicates (installerForEnv env) (selectBuildNumberPredicate bkNum avNum)
    thing2 :: Managed InstallersResults
    thing2 = getInstallersResults (configurationKeys env) instP rev destDir
  installerResults <- with thing2 pure
  print installerResults
  pure $ InstallerData installerResults

-- | Step 3. Hash installers and upload to S3
updateProposalUploadS3 :: BucketInfo -> InstallerData -> Shell (ArchMap DownloadVersionInfo)
updateProposalUploadS3 bucket InstallerData{idResults} = do
  printf ("*** Uploading installers to S3 bucket "%s%"\n") (biBucket bucket)
  urls <- uploadInstallers bucket idResults
  printf ("*** Uploading signatures to same S3 bucket.\n")
  signatures <- uploadSignatures bucket idResults
  resultMap <- needCIResult idResults
  let
    dvis = makeDownloadVersionInfo idResults resultMap urls signatures
  pure dvis

updateVersionJSON :: BucketInfo -> ArchMap DownloadVersionInfo -> IO T.Text
updateVersionJSON bucket dvis = do
  let
    blob = createVersionJSON dvis
  updateVersionJson bucket blob

createVersionJSON :: ArchMap DownloadVersionInfo -> L8.ByteString
createVersionJSON dvis = do
  let
    cfgReleaseNotes = Nothing
    v = downloadVersionInfoObject dvis cfgReleaseNotes
  encode v

uploadInstallers :: BucketInfo -> InstallersResults -> Shell (ArchMap T.Text)
uploadInstallers bucket res = runAWS' $ forResults res upload
  where
    upload _arch ci = do
      let
        hash = T.pack $ show $ cifBlakeCbor ci
      printf ("***   "%s%"  "%fp%"\n") hash (cifLocal ci)
      uploadHashedInstaller bucket (cifLocal ci) (globalResult res) (hash, T.pack $ encodeString $ ciResultFilename $ cifResult ci)

-- | Perform an action on the CI result of each arch.
forResults :: MonadIO io => InstallersResults -> (Arch -> CIResult2 -> io b) -> io (ArchMap b)
forResults rs action = needCIResult rs >>= archMapEach action

uploadResultSignature :: BucketInfo -> CIResult2 -> IO (Maybe T.Text)
uploadResultSignature bucket res = maybeReadFile sigFile >>= \case
  Just sig -> do
    runAWS' $ uploadSignature bucket sigFile
    pure $ Just sig
  Nothing -> do
    printf ("***   Signature file "%fp%" does not exist.\n") sigFile
    pure Nothing
  where
    sigFile = cifLocal res <.> "asc"
    maybeReadFile f = testfile f >>= \case
      True -> Just <$> readTextFile f
      False -> pure Nothing

-- | Partition CI results by arch.
groupResults :: InstallersResults -> ArchMap [CIResult2]
groupResults rs = filt <$> idArchMap
  where filt arch = filter ((== arch) . ciResultArch . cifResult) (ciResults rs)

-- | Get a single CI result for each arch and crash if not found.
needCIResult :: MonadIO io => InstallersResults -> io (ArchMap CIResult2)
needCIResult = archMapEach need . groupResults
  where
    need arch [] = die $ format ("The CI result for "%w%" is required but was not found.") arch
    need _arch (r:_) = pure r

-- | Slurp in previously created signatures.
uploadSignatures :: BucketInfo -> InstallersResults -> Shell (ArchMap (Maybe T.Text))
uploadSignatures bucket irs = fmap join . archMapFromList <$> mapM uploadSig (ciResults irs)
  where
    uploadSig res = do
      sig <- liftIO $ uploadResultSignature bucket res
      pure (ciResultArch $ cifResult res, sig)

-- | Adds two json objects together.
{-mergeObjects :: Value -> Value -> Value
mergeObjects (Object a) (Object b) = Object (a <> b)
mergeObjects _ b                   = b-}

-- | Splat version info to an aeson object.
downloadVersionInfoObject :: ArchMap DownloadVersionInfo  -> Maybe T.Text -> Value
downloadVersionInfoObject dvis releaseNotes = newFormat
  where
    newFormat :: Value
    newFormat = toJSON (DownloadVersionJson dvis releaseNotes)
    {-legacy :: Value
    legacy = (Object . HM.fromList . concat . map (uncurry toObject) . archMapToList) dvis
    toObject :: Arch -> DownloadVersionInfo -> [ (T.Text, Value) ]
    toObject arch DownloadVersionInfo{dviVersion,dviURL,dviHash,dviSignature,dviSHA256} = attrs
      where
        attrs :: [ (T.Text, Value) ]
        attrs = [ (keyPrefix arch <> k, String v) | (k, v) <-
                    [ (""         , dviVersion)
                    , ("URL"      , dviURL)
                    , ("Hash"     , dviHash)
                    , ("SHA256"   , dviSHA256)
                    , ("Signature", fromMaybe "" dviSignature)
                    ] ]
    keyPrefix Mac64   = "macos"
    keyPrefix Win64   = "win64"
    keyPrefix Linux64 = "linux"-}

makeDownloadVersionInfo :: InstallersResults
                        -> ArchMap CIResult2
                        -> ArchMap T.Text         -- ^ Download URLS
                        -> ArchMap (Maybe T.Text) -- ^ GPG Signatures
                        -> ArchMap DownloadVersionInfo
makeDownloadVersionInfo InstallersResults{globalResult} resultMap urls sigs = archMap dvi
  where
    dvi :: Arch -> DownloadVersionInfo
    dvi a = DownloadVersionInfo
      { dviVersion = grDaedalusVersion globalResult
      , dviURL = lookupArch a urls
      , dviHash = T.pack $ show $ cifBlakeCbor $ lookupArch a resultMap
      , dviSHA256 = T.pack $ show $ cifSha256 $ lookupArch a resultMap
      , dviSignature = lookupArch a sigs
      }

rehashInstallers :: InstallerData -> IO InstallerData
rehashInstallers InstallerData{idResults} = do
  let
    rehashInstaller :: CIResult2 -> IO CIResult2
    rehashInstaller input = do
      (blakecbor, sha256) <- hashInstallers $ cifLocal input
      pure $ input { cifBlakeCbor = blakecbor, cifSha256 = sha256 }
  rehashed <- mapM rehashInstaller (ciResults idResults)
  pure $ InstallerData $ InstallersResults rehashed (globalResult idResults)

handleEvents :: ProposalUIState -> AppState -> BrickEvent Name CustomEvent -> EventM Name DialogReply
handleEvents pstate@ProposalUIState{psMenuState,psDaedalusRev,psInstallers,psOutputDir,psDownloadVersionInfo,psBucket,psGPGUser,psEnvironment} _astate event = do
  let
    isValidRevision :: String -> Bool
    isValidRevision = all isValidChar
    isValidChar char = ( (char >= 'a') && (char <= 'f') ) || ( (char >= '0') && (char <= '9') )
    openThing :: EventM Name DialogReply
    openThing = do
      case L.listSelectedElement psMenuState of
        Just (_index, item) -> case item of
          SelectCluster -> do
            dlg <- spawnFileChooser "clusters" $ \mPath -> do
              case mPath of
                Just path -> do
                  eCfg <- liftIO $ eitherDecodeFileStrict' ("clusters/" <> path)
                  case eCfg of
                    Right cfg ->
                      pure $ DialogReplyContinue $ mkProposalUI $ pstate
                        { psBucket = BucketInfo (ccBucket cfg) (ccBucketURL cfg)
                        , psEnvironment = ccEnvironment cfg
                        }
            pure $ DialogReplyContinue dlg
          SetGPGUser -> spawnPromptString "GPG User?" (\_ -> True) $ \user -> do
            pure $ DialogReplyContinue $ mkProposalUI $ pstate { psGPGUser = Just $ T.pack user }
          SetDaedalusRev -> spawnPromptString "Daedalus Revision?" isValidRevision $ \rev -> do
            let
              state1 = pstate { psDaedalusRev = Just rev, psInstallers = Nothing }
              state2 = state1 { psMenuState = generateNewMenu state1 }
            pure $ DialogReplyContinue $ mkProposalUI state2
          FindInstallers -> do
            let
              act :: IO Dialog
              act = do
                res <- findInstallers psOutputDir psEnvironment (T.pack $ fromJust psDaedalusRev)
                let
                  state1 = pstate { psInstallers = Just res }
                  state2 = state1 { psMenuState = generateNewMenu state1 }
                pure $ mkProposalUI state2
            pure $ DialogReplyLiftIO act
          LocalInstallers -> do
            let
              nameToArch :: T.Text -> Arch
              nameToArch fn | T.isSuffixOf ".pkg" fn = Mac64
                            | T.isSuffixOf ".bin" fn = Linux64
                            | T.isSuffixOf ".exe" fn = Win64
              fileToResult :: T.Text -> IO CIResult2
              fileToResult name = do
                print ("fileToResult"::String)
                print name
                let
                  arch = nameToArch name
                  res1 = CIResult
                    { ciResultSystem = Buildkite
                    , ciResultUrl = "missing"
                    , ciResultDownloadUrl = "missing"
                    , ciResultBuildNumber = 0
                    , ciResultArch = arch
                    , ciResultSHA1Sum = Nothing
                    , ciResultFilename = FP.fromText name
                  }
                  localDest = (psOutputDir </> (FP.fromText name))
                cp ("installers" </> (FP.fromText name)) localDest
                (blakecbor, sha256) <- hashInstallers localDest
                let
                  res2 = CIFetchedResult
                    { cifResult = res1
                    , cifLocal = localDest
                    , cifBlakeCbor = blakecbor
                    , cifSha256 = sha256
                    }
                pure res2
              act :: IO Dialog
              act = do
                createDirectoryIfMissing True $ FP.encodeString psOutputDir
                rawFiles <- listDirectory "installers/"
                res2 <- mapM (fileToResult . T.pack) (filter (/= "version") rawFiles)
                daedalusVer <- T.readFile "installers/version"
                let
                  globalStatus = GlobalResults
                    { grCardanoCommit = "missing"
                    , grDaedalusCommit = T.pack $ fromJust $ psDaedalusRev
                    , grNodeVersion = "missing"
                    , grApplicationVersion = 0
                    , grCardanoVersion = "missing"
                    , grDaedalusVersion = T.dropAround (== '\n') daedalusVer
                    }
                  results = InstallersResults
                    { ciResults = res2
                    , globalResult = globalStatus
                    }
                  state1 = pstate { psInstallers = Just $ InstallerData results }
                  state2 = state1 { psMenuState = generateNewMenu state1 }
                pure $ mkProposalUI state2
            pure $ DialogReplyLiftIO act
          SignInstallers -> do
            pure $ DialogReplyLiftIO $ do
              updateProposalSignInstallers (idResults $ fromJust psInstallers) psGPGUser
              -- TODO, flag as signed, and give a popup saying success/fail
              pure $ mkProposalUI pstate
          S3Upload -> do
            pure $ DialogReplyLiftIO $ do
              dvis <- single $ updateProposalUploadS3 psBucket (fromJust psInstallers)
              let
                state1 = pstate { psDownloadVersionInfo = Just dvis }
                state2 = state1 { psMenuState = generateNewMenu state1 }
              pure $ mkProposalUI state2
          UpdateVersionJSON -> do
            pure $ DialogReplyLiftIO $ do
              _url <- updateVersionJSON psBucket (fromJust psDownloadVersionInfo)
              pure $ mkProposalUI pstate
          RehashInstallers -> do
            pure $ DialogReplyLiftIO $ do
              newInstallerData <- rehashInstallers (fromJust psInstallers)
              let
                state1 = pstate { psInstallers = Just newInstallerData, psDownloadVersionInfo = Nothing }
                state2 = state1 { psMenuState = generateNewMenu state1 }
              pure $ mkProposalUI state2
        Nothing -> pure $ DialogReplyContinue $ mkProposalUI pstate -- nothing selected, do nothing
  case event of
    VtyEvent (V.EvKey (V.KChar 'q') []) -> do
      psCallback pstate
    VtyEvent (V.EvKey V.KEnter []) -> do
      openThing
    VtyEvent evt -> do
      newlist <- L.handleListEventVi L.handleListEvent evt psMenuState
      pure $ DialogReplyContinue $ mkProposalUI pstate { psMenuState = newlist }
    _ -> do
      pure $ DialogReplyContinue $ mkProposalUI pstate

spawnProposalUI :: EventM Name DialogReply -> EventM Name Dialog
spawnProposalUI callback = do
  now <- liftIO $ getCurrentTime
  let
    yearMonthDay = formatTime defaultTimeLocale (iso8601DateFormat Nothing) now
    destDir = "proposal-cluster-" <> yearMonthDay
    bucket = BucketInfo "proposal-ui-test" "proposal-ui-test.s3.amazonaws.com"
    gpgUser :: Maybe T.Text
    gpgUser = Nothing
    state' :: ProposalUIState
    state' = ProposalUIState (Nothing) callback undefined Nothing (fromString destDir) Nothing bucket gpgUser ITNRW
    menu = generateNewMenu state'
    state = state' { psMenuState = menu }
  pure $ mkProposalUI state
