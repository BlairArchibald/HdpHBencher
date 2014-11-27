{-# LANGUAGE OverloadedStrings #-}

-- Main benchmark runner.
--
-- Author: Blair Archibald
--

import HSBencher
import HSBencher.Backend.Dribble

import HdpHMethod

import System.Process (callCommand)
import System.Directory

import Data.Monoid ((<>))
import qualified Data.Map as M
import qualified Data.HashMap.Strict as HM

import qualified Data.Configurator as Conf
import qualified Data.Configurator.Types as Conf

import Data.Maybe

import Data.List.Split (splitOn)

type ConfMap = HM.HashMap Conf.Name Conf.Value

main :: IO ()
main = do 
  putStrLn "Starting Benchmarking"

  -- Read in user config
  cfg <- Conf.load [Conf.Required "benchmark.conf"]

  Conf.display cfg


  -- Download the code under test.
  -- getRepository cfg

  confMap <- Conf.getMap cfg

  -- Run the benchmarks.
  defaultMainModifyConfig $ configureBenchmarkSpace confMap

    where getRepository cfg = do  r <- Conf.require cfg "hdphRepo"
                                  l <- Conf.require cfg "hdphInstallLoc"
                                  downloadRepository r l

configureBenchmarkSpace :: ConfMap -> Config -> Config
configureBenchmarkSpace cfg = addPlugins . addHarvesters . addBuildMethods  (buildPathReg cfg) . addBenchmarks (configureBenches cfg)
  where dribbleLoc = getStringOrNothing cfg "resultsCsv"
        addPlugins = addPlugin defaultDribblePlugin (DribbleConf $ Just dribbleLoc)
        addHarvesters c = c { harvesters = customTagHarvesterDouble "RUNPARIOTIME" <> harvesters c }

configureBenches :: ConfMap -> [Benchmark DefaultParamMeaning]
configureBenches cfg = foldl createBenchmarkSpaces [] benchmarkList
  where hdphLocal    = getStringOrNothing cfg "hdphInstallLoc" 
        benchmarkList= fromMaybe [""] $ Conf.convert $ cfg HM.! "benchmarks" :: [String]
        createBenchmarkSpaces acc n = mkBenchmark (hdphLocal ++ "/hdph/") [] (baseSpace $ And []) : acc -- $ binarySpace cfg n $ argSpace cfg n $ processSpace cfg n) : acc

baseSpace :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
baseSpace spc = And [
                Set NoMeaning (CompileParam "--flags=WithMPI")
               ,Set NoMeaning (RuntimeParam "hostFile:hosts")
               ,spc
               ]

{- argSpace :: ConfigParser -> String -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
argSpace cfg n spc = Or [spc]

binarySpace :: ConfigParser -> String -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
binarySpace cfg n spc = Or [ And [Set NoMeaning (RuntimeParam $ "bin:" ++ bin), spc] ]
  where bin = forceConfigGet cfg n "name"

--Doesn't currently scale threads. How do I handle this?
processSpace :: ConfigParser -> String -> BenchSpace DefaultParamMeaning
processSpace cfg n = Or [ Set NoMeaning (RuntimeParam $ "numProcs" ++ ":" ++ show (proc)) | proc <- [1..maxProcs]]
  where maxProcs = forceConfigGet cfg n "maxProcs" :: Int -}

addBuildMethods :: M.Map String String -> Config -> Config
addBuildMethods pathReg c = c {buildMethods = [hdphMethod]
                      ,doClean      = True
                      -- Slight hack to pass the local install information into the sandbox compile stage.
                      ,pathRegistry = pathRegistry c `M.union` pathReg
                      }

buildPathReg :: ConfMap -> M.Map String String
buildPathReg cfg = M.fromList [("extra-packages", getStringOrNothing cfg "installArgs")]

downloadRepository :: String -> FilePath -> IO ()
downloadRepository rep loc = do
  e <- doesDirectoryExist loc
  case e of 
    True  -> putStrLn ("Removing existing repository at " ++ loc) >> removeDirectoryRecursive loc
    False -> return ()

  callCommand $ "git clone " ++ rep ++ " " ++ loc

-- Config Helper functions.
getStringOrNothing :: ConfMap -> Conf.Name -> String
getStringOrNothing cfg s = fromMaybe "" $ Conf.convert $ cfg HM.! s

