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
import qualified Data.Text as T

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
        benchmarkList= fromMaybe [""] $ Conf.convert $ cfg HM.! "benchmarks" :: [T.Text]
        createBenchmarkSpaces acc n = mkBenchmark (hdphLocal ++ "/hdph/") [] (baseSpace $ generateBenchmarkSpaces cfg n) : acc

baseSpace :: BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
baseSpace spc = And [
                Set NoMeaning (CompileParam "--flags=WithMPI")
               ,Set NoMeaning (RuntimeParam "hostFile:hosts")
               ,spc
               ]

generateBenchmarkSpaces :: ConfMap -> T.Text -> BenchSpace DefaultParamMeaning
generateBenchmarkSpaces cfg bname = And [Set NoMeaning (RuntimeParam $ "bin:" ++ binName)
                                        ,Or genArgs
                                        ]
  where binName = getStringOrNothing cfg $ bname `T.append` ".name"
        genArgs = foldl createSpaces [] $ getArgs
        getArgs = fromMaybe [[]] $ Conf.convert $ cfg HM.! (bname `T.append` ".args")
        createSpaces acc (a:p:[]) = (And [Set NoMeaning (RuntimeParam $ "args:" ++ (fromMaybe "" $ Conf.convert a))
                                      ,Set NoMeaning (RuntimeParam $ "numProcs: " ++ (show $ (fromMaybe 0 $ Conf.convert p :: Int)))
                                      ]
                                 ): acc
        createSpaces acc _ = (And []) : acc

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
