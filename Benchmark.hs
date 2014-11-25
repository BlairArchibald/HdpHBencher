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
import Data.Map as M

import qualified Data.ConfigFile as Conf
import Data.ConfigFile

import Data.Either.Utils

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
configureBenchmarkSpace cfg c = addPlugin defaultDribblePlugin (DribbleConf $ Just "./dribble.csv") . addHarvesters . (addBuildMethods $ buildPathReg cfg) . addBenchmarks (benches hdphLocal 0 [])
  where hdphLocal  = forceConfigGet cfg "DEFAULT" "hdphInstallLoc"
        dribbleLoc = forceConfigGet cfg "DEFAULT" "resultsCsv"

benches :: String -> Int -> [String] -> [Benchmark DefaultParamMeaning]
benches hdphLocal maxProcs binList = [mkBenchmark (hdphLocal ++ "/hdph/") [] (baseSpace $ binarySpace binList $ processSpace maxProcs)]

baseSpace :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
baseSpace spc = And [
                Set NoMeaning (CompileParam "--flags=WithMPI")
               ,Set NoMeaning (RuntimeParam "hostFile:hosts")
               --quick hack for now
               ,Set NoMeaning (RuntimeParam "args:v2") 
               ,spc
               ]

binarySpace :: [String] -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
binarySpace bins spc = Or [ And [Set NoMeaning (RuntimeParam $ "bin" ++ ":" ++ bin), spc] | bin <- bins ]

processSpace :: Int -> BenchSpace DefaultParamMeaning
processSpace n = Or [ Set NoMeaning (RuntimeParam $ "numProcs" ++ ":" ++ show (proc)) | proc <- [1..n]]

addBuildMethods :: Map String String -> Config -> Config
addBuildMethods pathReg c = c {buildMethods = [hdphMethod]
                      ,doClean      = True
                      -- Slight hack to pass the local install information into the sandbox compile stage.
                      ,pathRegistry = pathRegistry c `M.union` pathReg
                      }

buildPathReg :: ConfigParser -> Map String String
buildPathReg cfg = let a = forceConfigGet cfg "DEFAULT" "installArgs"
                   in  M.fromList [("extra-packages",a)]

addHarvesters :: Config -> Config
addHarvesters conf = conf { harvesters = customTagHarvesterDouble "RUNPARIOTIME" <> harvesters conf }

downloadRepository :: String -> FilePath -> IO ()
downloadRepository rep loc = do
  e <- doesDirectoryExist loc
  case e of 
    True  -> putStrLn ("Removing existing repository at " ++ loc) >> removeDirectoryRecursive loc
    False -> return ()

  callCommand $ "git clone " ++ rep ++ " " ++ loc

forceConfigGet :: Get_C a => ConfigParser -> SectionSpec -> OptionSpec -> a
forceConfigGet cfg sec opt = forceEither $ get cfg sec opt 
