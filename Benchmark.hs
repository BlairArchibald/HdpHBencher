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

import qualified Data.ConfigFile as Conf
import Data.ConfigFile

import Data.Either.Utils

import Data.List.Split (splitOn)

main :: IO ()
main = do 
  putStrLn "Starting Benchmarking"

  -- Read in user config
  r <- Conf.readfile emptyCP{optionxform = id
                            ,accessfunc  = interpolatingAccess 10
                            } "benchmark.conf"
  let cfg = forceEither r
  putStrLn "Using config: " >> putStrLn (Conf.to_string cfg)

  -- Download the code under test.
  getRepository cfg

  -- Run the benchmarks.
  defaultMainModifyConfig $ configureBenchmarkSpace cfg

    where getRepository cfg = let r = forceConfigGet cfg "DEFAULT" "hdphRepo"
                                  l = forceConfigGet cfg "DEFAULT" "hdphInstallLoc"
                              in downloadRepository r l

configureBenchmarkSpace :: ConfigParser -> Config -> Config
configureBenchmarkSpace cfg = addPlugins . addHarvesters . addBuildMethods  (buildPathReg cfg) . addBenchmarks (configureBenches cfg)
  where dribbleLoc = forceConfigGet cfg "DEFAULT" "resultsCsv"
        addPlugins = addPlugin defaultDribblePlugin (DribbleConf $ Just dribbleLoc)
        addHarvesters c = c { harvesters = customTagHarvesterDouble "RUNPARIOTIME" <> harvesters c }

configureBenches :: ConfigParser -> [Benchmark DefaultParamMeaning]
configureBenches cfg = foldl createBenchmarkSpaces [] benchmarkList
  where hdphLocal    = forceConfigGet cfg "DEFAULT" "hdphInstallLoc"
        benchmarkList= splitOn "," $ forceConfigGet cfg "DEFAULT" "benchmarks"
        -- Might actually be easier to do strong scaling here. We can use args
        -- if we have lots of small spaces rather than one big one?
        createBenchmarkSpaces acc n = mkBenchmark (hdphLocal ++ "/hdph/") [] (baseSpace $ binarySpace cfg n $ argSpace cfg n $ processSpace cfg n) : acc

baseSpace :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
baseSpace spc = And [
                Set NoMeaning (CompileParam "--flags=WithMPI")
               ,Set NoMeaning (RuntimeParam "hostFile:hosts")
               ,spc
               ]

argSpace :: ConfigParser -> String -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
argSpace cfg n spc = Or [spc]

binarySpace :: ConfigParser -> String -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
binarySpace cfg n spc = Or [ And [Set NoMeaning (RuntimeParam $ "bin:" ++ bin), spc] ]
  where bin = forceConfigGet cfg n "name"

--Doesn't currently scale threads. How do I handle this?
processSpace :: ConfigParser -> String -> BenchSpace DefaultParamMeaning
processSpace cfg n = Or [ Set NoMeaning (RuntimeParam $ "numProcs" ++ ":" ++ show (proc)) | proc <- [1..maxProcs]]
  where maxProcs = forceConfigGet cfg n "maxProcs" :: Int

addBuildMethods :: M.Map String String -> Config -> Config
addBuildMethods pathReg c = c {buildMethods = [hdphMethod]
                      ,doClean      = True
                      -- Slight hack to pass the local install information into the sandbox compile stage.
                      ,pathRegistry = pathRegistry c `M.union` pathReg
                      }

buildPathReg :: ConfigParser -> M.Map String String
buildPathReg cfg = let a = forceConfigGet cfg "DEFAULT" "installArgs"
                   in  M.fromList [("extra-packages",a)]

downloadRepository :: String -> FilePath -> IO ()
downloadRepository rep loc = do
  e <- doesDirectoryExist loc
  case e of 
    True  -> putStrLn ("Removing existing repository at " ++ loc) >> removeDirectoryRecursive loc
    False -> return ()

  callCommand $ "git clone " ++ rep ++ " " ++ loc

forceConfigGet :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
forceConfigGet cfg sec opt = forceEither $ get cfg sec opt 
