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

import qualified Data.Configurator as Conf
import Data.Configurator (Worth(..))

maxProcs  = 4
binList   = ["hello"]

main :: IO ()
main = do 
  putStrLn "Starting Benchmarking"

  cfg <- Conf.load [Required "benchmark.conf"]
  putStrLn "Using config:"
  Conf.display cfg

  getRepository cfg

  defaultMainModifyConfig $ (addPlugin defaultDribblePlugin (DribbleConf $ Just "./dribble.csv")) . addHarvesters . addBuildMethods . addBenchmarks benches
    where getRepository cfg = do r <- Conf.require cfg "hdphRepo"
                                 l <- Conf.require cfg "hdphInstallLoc"
                                 downloadRepository r l

benches :: [Benchmark DefaultParamMeaning]
benches = [mkBenchmark (hdphLocal ++ "/hdph/") [] (baseSpace $ binarySpace binList $ processSpace maxProcs)]

baseSpace :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
baseSpace spc = And [
                Set NoMeaning (CompileParam "--flags=WithMPI")
               ,Set NoMeaning (RuntimeParam "hostFile:hosts")
               ,spc
               ]

binarySpace :: [String] -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
binarySpace bins spc = Or [ And [Set NoMeaning (RuntimeParam $ "bin" ++ ":" ++ bin), spc] | bin <- bins ]

processSpace :: Int -> BenchSpace DefaultParamMeaning
processSpace n = Or [ Set NoMeaning (RuntimeParam $ "numProcs" ++ ":" ++ show (proc)) | proc <- [1..n]]

addBuildMethods :: Config -> Config
addBuildMethods c = c {buildMethods = [hdphMethod]
                      ,doClean      = True
                      -- Slight hack to pass the local install information into the sandbox compile stage.
                      ,pathRegistry = pathRegistry c `M.union` pathReg
                      }
  where pathReg = M.fromList [
                             -- Don't like how this is hardcoded - Either read
                             -- from a file or try and do it relative?
                             -- Probably do some form of intersperse function to make this neat.
                              ("extra-packages",(hdphLocal ++ "/hdph-mpi-allgather/ --flags=libMPICH2 --extra-include-dirs=" 
                              ++ (home ++ "/mpich/include") ++ " --extra-lib-dirs=" ++ (home ++ "/mpich/lib") 
                              ++ ":" ++ (hdphLocal ++ "/hdph-closure/")))
                             ]

addHarvesters :: Config -> Config
addHarvesters conf = conf { harvesters = customTagHarvesterDouble "RUNPARIOTIME" <> harvesters conf }

-- TODO: Possibly check the gitref that way we can avoid destroying all sandbox
-- dirs and starting fresh.
downloadRepository :: String -> FilePath -> IO ()
downloadRepository rep loc = do
  e <- doesDirectoryExist loc
  case e of 
    True  -> putStrLn ("Removing existing repository at " ++ loc) >> removeDirectoryRecursive loc
    False -> return ()

  callCommand $ "git clone " ++ rep ++ " " ++ loc
