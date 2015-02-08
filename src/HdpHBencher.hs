{-# LANGUAGE OverloadedStrings #-}

-- Main benchmark runner.
--
-- Author: Blair Archibald
--

import HSBencher
import HSBencher.Backend.Dribble

import HdpHBencher.BenchmarkConfig

import HdpHBencher.HdpHMethod

import System.Process (callCommand)
import System.Directory

import Data.List (intercalate)

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf

import Data.Maybe
import qualified Data.Text as T

type ConfMap = HM.HashMap Conf.Name Conf.Value

type BenchmarkName = T.Text

-- Datatypes for configuration options
-- Allow setting the most common options.
--
-- TODO: See if using lenses can make this neater.
data BenchmarkConfig = BenchmarkConfig { runConf      :: BenchmarkRunConf
                                       , hdphConf     :: HdpHConf
                                       , rtsConf      :: RTSConf
                                       , mpiExecConf  :: MPIExecConf
                                       } deriving (Show)

-- An Induvidual Benchmark Run.
data BenchmarkRunConf = BenchmarkRunConf { binaryLoc     :: FilePath
                                         , binaryName    :: String
                                         , benchmarkArgs :: Maybe String
                                         } deriving (Show)

-- HdpH Options for a Run
data HdpHConf = HdpHConf { hdphNumProcs :: Int
                         , interface    :: Maybe String
                         , tcpStartup   :: Bool
                         , startupHost  :: Maybe String
                         , startupPort  :: Maybe String
                         , hdphAdditionalArgs :: Maybe String
                         } deriving (Show)

-- RTS Options for a Run
data RTSConf = RTSConf { numThreads :: Maybe Int
                       , heapSize   :: Maybe String
                       , rtsAdditonalArgs :: Maybe String
                       } deriving (Show)

-- Optons to pass to the mpiexec command
-- TODO: Allow the user to select which mpiexec they are using (args differ).
data MPIExecConf = MPIExecConf { mpiNumProcs :: Int
                               , hostFile :: Maybe FilePath
                               , mpiAdditonalArgs :: Maybe String
                               } deriving (Show)

-- Default configs
defaultBenchmarkRunConf :: BenchmarkRunConf
defaultBenchmarkRunConf = BenchmarkRunConf "" "" Nothing

defaultHpdHConf :: HdpHConf
defaultHpdHConf = HdpHConf { hdphNumProcs       = 0
                           , interface          = Nothing
                           , tcpStartup         = False
                           , startupHost        = Nothing
                           , startupPort        = Nothing
                           , hdphAdditionalArgs = Nothing
                           }

defaultRTSConf :: RTSConf
defaultRTSConf = RTSConf Nothing Nothing Nothing

defaultMPIExecConf :: MPIExecConf
defaultMPIExecConf = MPIExecConf 0 Nothing Nothing

-- Config Parsing

-- Benchmark space builders

-- TODO: Change to Text?
buildHdpHArgString :: HdpHConf -> String
buildHdpHArgString cfg = "+HdpH " ++ genArgs ++ " -HdpH"
  where genArgs   = collapseArgs printArgs
        printArgs = [nprocs, iface, startup, host, port, extra]
        nprocs    = "numProcs=" ++ show (hdphNumProcs cfg)
        iface     = maybe "" ("nic=" ++) (interface cfg)
        startup   = case (tcpStartup cfg) of
          True  -> "startupBackend=tcp"
          False -> ""
        host      = maybe "" ("startupHost=" ++) (startupHost cfg)
        port      = maybe "" ("startupPort=" ++) (startupPort cfg)
        extra     = fromMaybe "" (hdphAdditionalArgs cfg)

buildRTSArgString :: RTSConf -> String
buildRTSArgString cfg = "+RTS " ++ genArgs ++ " -RTS"
  where genArgs       = collapseArgs printArgs
        printArgs     = [threads, heap, extra]
        threads       = "-N" ++ show (fromMaybe 1 (numThreads cfg))
        heap          = maybe "" ("-A" ++) (heapSize cfg)
        extra         = fromMaybe "" (rtsAdditonalArgs cfg)

buildMPIExecArgString :: MPIExecConf -> String
buildMPIExecArgString cfg = collapseArgs printArgs
  where printArgs         = [hosts, nprocs, extra]
        nprocs            = "-n " ++ show (mpiNumProcs cfg)
        hosts             = maybe "" ("-hostfile " ++) (hostFile cfg)
        extra             = fromMaybe "" (mpiAdditonalArgs cfg)

-- Collapse a list of string with possible nulls into a single space seperated
-- string.
collapseArgs :: [String] -> String
collapseArgs = intercalate " " . filter (/= "")

-- Main benchmark runner
main :: IO ()
main = do
  putStrLn "Starting Benchmarking"

  -- Read in user config
  cfg <- Conf.load [Conf.Required "benchmark.conf"]

  -- Download the code under test.
  --getRepository cfg

  confMap <- Conf.getMap cfg

  -- Add global options to the default configurations
  let baseConf = constructBaseConfig confMap

  defaultMainModifyConfig $ configureBenchmarkSpace confMap baseConf

  where getRepository cfg = do  r <- Conf.require cfg "hdphRepo"
                                b <- Conf.lookup cfg  "hdphBranch"
                                l <- Conf.require cfg "hdphInstallLoc"
                                downloadRepository r b l

-- Add global options to a default config space.
constructBaseConfig :: ConfMap -> BenchmarkConfig
constructBaseConfig cfg = BenchmarkConfig { runConf  = defaultBenchmarkRunConf
                                          , hdphConf = globalHdpHConf
                                          , rtsConf  = globalRTSConf
                                          , mpiExecConf = globalMPIExecConf
                                          }
  where globalHdpHConf =
          defaultHpdHConf
          {
            hdphNumProcs       = fromMaybe 0 $ getConfValueOrNothing "numProcs" cfg
          , interface          = getConfValueOrNothing "interface" cfg
          , tcpStartup         = maybe False id $ getConfValueOrNothing "tcpStartup" cfg
          , startupHost        = getConfValueOrNothing "startupHost" cfg
          , startupPort        = getConfValueOrNothing "startupPort" cfg
          , hdphAdditionalArgs = getConfValueOrNothing "globalHdpHArgs" cfg
          }

        globalRTSConf =
          defaultRTSConf
          {
            numThreads       = getConfValueOrNothing "numThreads" cfg
          , rtsAdditonalArgs = getConfValueOrNothing "globalRTSArgs" cfg
          }

        globalMPIExecConf =
          defaultMPIExecConf
          {
            mpiNumProcs      = fromMaybe 0 $ getConfValueOrNothing "numProcs" cfg
          , hostFile         = getConfValueOrNothing "hostsFile" cfg
          , mpiAdditonalArgs = getConfValueOrNothing "MPIExecAdditionalArgs" cfg
  }

-- Possibly able to refactor this into the baseConf method
updateConf :: BenchmarkConfig -> ConfMap -> BenchmarkName -> BenchmarkConfig
updateConf baseConf cfg bname =
  let
      baseRunConf  = runConf  baseConf
      baseHdpHConf = hdphConf baseConf
      baseRTSConf  = rtsConf  baseConf
      baseMPIConf  = mpiExecConf baseConf
  in
      baseConf { runConf  = updateRunConf baseRunConf
               , hdphConf = updateHdpHConf baseHdpHConf
               , rtsConf  = updateRTSConf baseRTSConf
               , mpiExecConf = updateMPIExecConf baseMPIConf
               }
  where
    addPrefix = T.append (bname `T.append` ".")
    updateMaybeConf s cur = case cur of
                             Nothing -> getConfValueOrNothing (addPrefix s) cfg
                             Just x  -> Just x

    updateRunConf c =
      c { binaryLoc     = getConfValueOrFail (addPrefix "binLoc") cfg
        , binaryName    = getConfValueOrFail (addPrefix "binName") cfg
        , benchmarkArgs = updateMaybeConf "args" (benchmarkArgs c)
        }

    updateHdpHConf c =
      c {
          hdphNumProcs       = fromMaybe
                                (hdphNumProcs c)
                                (getConfValueOrNothing (addPrefix "numProcs") cfg)
        , interface          = updateMaybeConf "interface" (interface c)
        , tcpStartup         = fromMaybe
                                (tcpStartup c)
                                (getConfValueOrNothing (addPrefix "tcpStartup") cfg)
        , startupHost        = updateMaybeConf "startupHost" (startupHost c)
        , startupPort        = updateMaybeConf "startupPort" (startupPort c)
        , hdphAdditionalArgs = updateMaybeConf "globalHdpHArgs" (hdphAdditionalArgs c)
        }

    updateRTSConf c =
      c {
          numThreads       = updateMaybeConf "numThreads" (numThreads c)
        , rtsAdditonalArgs = updateMaybeConf "globalRTSArgs" (rtsAdditonalArgs c)
        }

    updateMPIExecConf c =
      c {
           mpiNumProcs = fromMaybe
                          (mpiNumProcs c)
                          (getConfValueOrNothing (addPrefix "numProcs") cfg)
        , hostFile         = updateMaybeConf "hostsFile" (hostFile c)
        , mpiAdditonalArgs = updateMaybeConf "MPIExecAdditionalArgs" (mpiAdditonalArgs c)
        }


-- Config Helper functions.
getConfValueOrNothing :: Conf.Configured a =>  Conf.Name -> ConfMap -> Maybe a
getConfValueOrNothing s m = case s `HM.lookup` m of
                          Just a  -> Conf.convert a
                          Nothing -> Nothing

getConfValueOrFail :: Conf.Configured a =>  Conf.Name -> ConfMap -> a
getConfValueOrFail s m = case s `HM.lookup` m of
                          Just a -> case Conf.convert a of
                            Just a' -> a'
                            Nothing -> error $ "Could not get: " ++ (show s) ++ " from config."
                          Nothing -> error   $ "Could not get: " ++ (show s) ++ " from config."

-- Benchmark Configuration Functions
configureBenchmarkSpace :: ConfMap -> BenchmarkConfig -> Config -> Config
configureBenchmarkSpace cfg baseConf = addPlugins
                                     . addHarvesters
                                     . addBuildMethods (buildPathReg cfg)
                                     . addBenchmarks (configureBenches cfg baseConf)
  where
    addPlugins = addPlugin defaultDribblePlugin (DribbleConf $ Just dribbleLoc)
    dribbleLoc = fromMaybe "" $ getConfValueOrNothing "resultsCsv" cfg
    --TODO: Harvesters from config file.
    addHarvesters c = c { harvesters = customTagHarvesterDouble "RUNTIME"
                          <> harvesters c }


configureBenches :: ConfMap -> BenchmarkConfig -> [Benchmark DefaultParamMeaning]
configureBenches cfg baseConf = map addBenchmark benchmarkList
  where benchmarkList = fromMaybe [""] $ Conf.convert $ cfg HM.! "benchmarks" :: [T.Text]
        addBenchmark  = configureBenchmark cfg baseConf

configureBenchmark :: ConfMap -> BenchmarkConfig -> BenchmarkName -> Benchmark DefaultParamMeaning
configureBenchmark cfg baseConf bname =
  let conf = updateConf baseConf cfg bname in
      mkBenchmark (binaryLoc (runConf conf)) [] $
        And [  Set NoMeaning (RuntimeParam ("HdpHArgs:"    ++ (buildHdpHArgString (hdphConf conf))))
             , Set NoMeaning (RuntimeParam ("MPIExecArgs:" ++ (buildMPIExecArgString (mpiExecConf conf))))
             , Set NoMeaning (RuntimeParam ("RTSArgs:"     ++ (buildRTSArgString (rtsConf conf))))
             , Set NoMeaning (RuntimeParam ("Bin:"         ++ (binaryName (runConf conf))))
             , Set NoMeaning (RuntimeParam ("ProgArgs:"    ++ (fromMaybe "" $ benchmarkArgs (runConf conf))))
             ]

addBuildMethods :: M.Map String String -> Config -> Config
addBuildMethods pathReg c = c {buildMethods = [hdphMethod]
                              ,doClean      = False
                              -- Slight hack to pass the local install information into the sandbox compile stage.
                              ,pathRegistry = pathRegistry c `M.union` pathReg
                              }

buildPathReg :: ConfMap -> M.Map String String
buildPathReg cfg = M.fromList
    [
      ("extra-packages", fromMaybe "" $ getConfValueOrNothing "extraSandboxPackages" cfg),
      ("ghc", fromMaybe "" $ getConfValueOrNothing "ghcLoc" cfg)
    ]

downloadRepository :: String -> Maybe String -> FilePath -> IO ()
downloadRepository rep branch loc = do
  e <- doesDirectoryExist loc
  case e of
    True  -> do
      putStrLn ("Removing existing repository at " ++ loc)
      removeDirectoryRecursive loc
    False -> return ()

  case branch of
    Just b  -> callCommand $ "git clone -b " ++ b ++ " " ++ rep ++ " " ++ loc
    Nothing -> callCommand $ "git clone " ++ rep ++ " " ++ loc
