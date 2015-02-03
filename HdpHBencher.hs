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

type ConfMap = HM.HashMap Conf.Name Conf.Value

main :: IO ()
main = do
  putStrLn "Starting Benchmarking"

  -- Read in user config
  cfg <- Conf.load [Conf.Required "benchmark.conf"]

  -- Download the code under test.
  getRepository cfg

  confMap <- Conf.getMap cfg

  -- Run the benchmarks.
  defaultMainModifyConfig $ configureBenchmarkSpace confMap

  --TODO: Move this into HsBencher - Process the results file to get the numCores
  --callCommand $ "./processResults.sh " ++ (getStringOrNothing confMap "resultsCsv")

    where getRepository cfg = do  r <- Conf.require cfg "hdphRepo"
                                  l <- Conf.require cfg "hdphInstallLoc"
                                  b <- Conf.lookup cfg  "hdphBranch"
                                  downloadRepository r b l

-- TODO: Use a reader monad to thread through the configuration.
configureBenchmarkSpace :: ConfMap -> Config -> Config
configureBenchmarkSpace cfg = addPlugins . addHarvesters . addBuildMethods  (buildPathReg cfg) . addBenchmarks (configureBenches cfg)
  where dribbleLoc = getStringOrNothing cfg "resultsCsv"
        addPlugins = addPlugin defaultDribblePlugin (DribbleConf $ Just dribbleLoc)
        addHarvesters c = c { harvesters =  customTagHarvesterDouble "RUNTIME" <> harvesters c }

--TODO: Need to specify the benchmark dir in config line so we can support
-- multiple directories.
configureBenches :: ConfMap -> [Benchmark DefaultParamMeaning]
configureBenches cfg = foldl createBenchmarkSpaces [] benchmarkList
  where benchmarkList= fromMaybe [""] $ Conf.convert $ cfg HM.! "benchmarks" :: [T.Text]
        createBenchmarkSpaces acc n = mkBenchmark ("./programs/Liouville/") [] (baseSpace cfg $ generateBenchmarkSpaces cfg n) : acc

baseSpace :: ConfMap -> BenchSpace DefaultParamMeaning -> BenchSpace DefaultParamMeaning
baseSpace cfg spc = And [ Set NoMeaning (RuntimeParam ("hostFile:" ++ hfile))
                        , Set NoMeaning (RuntimeParam ("interface:" ++ interface))
                        ,spc
                        ]
  where hfile      = fromMaybe "hosts" $ Conf.convert $ HM.lookupDefault (Conf.String "hosts") "hostFile"  cfg
        interface  = fromMaybe "eth0"  $ Conf.convert $ HM.lookupDefault (Conf.String "eth0")  "interface" cfg

generateBenchmarkSpaces :: ConfMap -> T.Text -> BenchSpace DefaultParamMeaning
generateBenchmarkSpaces cfg bname = And [Set NoMeaning (RuntimeParam $ "bin:" ++ binName)
                                        ,Or genArgs
                                        ]
  where binName = getStringOrNothing cfg $ bname `T.append` ".name"
        genArgs = foldl createSpaces [] $ getArgs
        getArgs = fromMaybe [[]] $ Conf.convert $ cfg HM.! (bname `T.append` ".args")
        createSpaces acc (a:p:t:[]) = let threads = fromMaybe 0 $ Conf.convert t :: Int in
                                      (And [Set NoMeaning (RuntimeParam $ "args:" ++ (fromMaybe "" $ Conf.convert a))
                                           ,Set NoMeaning (RuntimeParam $ "numProcs: " ++ (show $ (fromMaybe 0 $ Conf.convert p :: Int)))
                                           ,Set (Threads threads) (RuntimeParam $ "numThreads: " ++ (show $ threads))
                                           ]
                                      ): acc
        createSpaces acc _ = (And []) : acc

addBuildMethods :: M.Map String String -> Config -> Config
addBuildMethods pathReg c = c {buildMethods = [hdphMethod]
                      ,doClean      = False
                      -- Slight hack to pass the local install information into the sandbox compile stage.
                      ,pathRegistry = pathRegistry c `M.union` pathReg
                      }

buildPathReg :: ConfMap -> M.Map String String
buildPathReg cfg = M.fromList [
  ("extra-packages", getStringOrNothing cfg "extraSandboxPackages"),
  ("ghc", getStringOrNothing cfg "ghcLoc")
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

-- Config Helper functions.
getStringOrNothing :: ConfMap -> Conf.Name -> String
getStringOrNothing cfg s = fromMaybe ""  $ Conf.convert $ HM.lookupDefault (Conf.String "") s cfg
