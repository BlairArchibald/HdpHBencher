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

maxProcs  = 3
binList   = ["sumeuler"]

main :: IO ()
main = do 
  putStrLn "Starting Benchmarking"

  cfg <- Conf.load [Required "benchmark.conf"]
  putStrLn "Using config:"
  Conf.display cfg

  getRepository cfg

  pathReg <- buildPathReg cfg
  l <- Conf.require cfg "hdphInstallLoc"

  defaultMainModifyConfig $ (addPlugin defaultDribblePlugin (DribbleConf $ Just "./dribble.csv")) . addHarvesters . (addBuildMethods pathReg) . addBenchmarks (benches l)
    where getRepository cfg = do r <- Conf.require cfg "hdphRepo"
                                 l <- Conf.require cfg "hdphInstallLoc"
                                 downloadRepository r l

benches :: String -> [Benchmark DefaultParamMeaning]
benches hdphLocal = [mkBenchmark (hdphLocal ++ "/hdph/") [] (baseSpace $ binarySpace binList $ processSpace maxProcs)]

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

--buildPathReg :: Conf.Config -> IO Map String String
buildPathReg cfg = do a <- Conf.require cfg "installArgs"
		      return $ M.fromList [("extra-packages",a)]

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
