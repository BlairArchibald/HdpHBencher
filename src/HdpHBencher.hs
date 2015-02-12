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

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf

import Data.Maybe
import qualified Data.Text as T

type ConfMap = HM.HashMap Conf.Name Conf.Value

type BenchmarkName = T.Text

-- Main benchmark runner
main :: IO ()
main = do
  putStrLn "Starting Benchmarking"

  -- Read in user config
  cfg <- Conf.load [Conf.Required "benchmark.conf"]

  -- Download the code under test.
  getRepository cfg

  confMap <- Conf.getMap cfg

  -- Add global options to the default configurations
  let baseConf = constructBaseConfig confMap

  defaultMainModifyConfig $ configureBenchmarkSpace confMap baseConf

  where getRepository cfg = do  r <- Conf.require cfg "hdphRepo"
                                b <- Conf.lookup cfg  "hdphBranch"
                                l <- Conf.require cfg "hdphInstallLoc"
                                downloadRepository r b l



-- Benchmark Configuration Functions
configureBenchmarkSpace :: ConfMap -> BenchmarkConfig -> Config -> Config
configureBenchmarkSpace cfg baseConf =
   addPlugins
 . updateHsBencherConf cfg
 . addBenchmarks (configureBenches cfg baseConf)
  where
    addPlugins = addPlugin defaultDribblePlugin (DribbleConf $ Just dribbleLoc)
    dribbleLoc = fromMaybe "" $ getConfValueOrNothing "resultsCsv" cfg


configureBenches :: ConfMap -> BenchmarkConfig -> [Benchmark DefaultParamMeaning]
configureBenches cfg baseConf = map addBenchmark benchmarkList
  where benchmarkList :: [BenchmarkName]
        benchmarkList = fromMaybe [""] $ Conf.convert $ cfg HM.! "benchmarks"
        addBenchmark  = configureBenchmark cfg baseConf

configureBenchmark :: ConfMap
                  -> BenchmarkConfig
                  -> BenchmarkName
                  -> Benchmark DefaultParamMeaning
configureBenchmark cfg baseConf bname =
  let conf = updateConf baseConf cfg bname in
      mkBenchmark (binaryLoc (runConf conf)) [] $
        And [  Set NoMeaning (RuntimeParam
                ("HdpHArgs:" ++ (buildHdpHArgString (hdphConf conf))))
             , Set NoMeaning (RuntimeParam
                ("MPIExecArgs:" ++ (buildMPIExecArgString (mpiExecConf conf))))
             , Set NoMeaning (RuntimeParam
                ("RTSArgs:"  ++ (buildRTSArgString (rtsConf conf))))
             , Set NoMeaning (RuntimeParam
                ("Bin:"      ++ (binaryName (runConf conf))))
             , Set (Variant (variant (runConf conf))) (RuntimeParam
                ("ProgArgs:" ++ (fromMaybe "" $ benchmarkArgs (runConf conf))))
             ]

updateHsBencherConf :: ConfMap -> Config -> Config
updateHsBencherConf cfm config =
  config
    { buildMethods = [hdphMethod]
    , doClean      = False
    , keepgoing    = True
    , trials       = fromMaybe 1 $ getConfValueOrNothing "samples" cfm

    --TODO: Harvesters from config?
    , harvesters   = customAccumHarvesterDouble "RUNTIME" <> harvesters config

    -- Hack to pass the local install information into the sandbox compile stage.
    , pathRegistry = pathRegistry config `M.union` buildPathReg cfm
    }

buildPathReg :: ConfMap -> M.Map String String
buildPathReg cfg = M.fromList
   [
     ("extra-packages", fromMaybe "" $
       getConfValueOrNothing "extraSandboxPackages" cfg)
   , ("ghc", fromMaybe "" $ getConfValueOrNothing "ghcLoc" cfg)
   ]

-- TODO: Only delete the repository if it's out of date.
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
