-- Main benchmark runner.
--
-- Author: Blair Archibald
--

import HSBencher
import HdpHMethod

import System.Process (callCommand)
import System.Directory

import Data.Map as M

home      = "/users/grad/blair"
hdphRepo  = "git@github.com:BlairArchibald/HdpH.git"
hdphLocal = "/users/grad/blair/HdpHBencher/HdpH"

maxProcs  = 4
binList   = ["hello"]

main :: IO ()
main = do 
  putStrLn "Starting Benchmarking"
  grabRepository hdphRepo hdphLocal
  defaultMainModifyConfig $ addBuildMethods . addBenchmarks benches 

benches :: [Benchmark DefaultParamMeaning]
benches = [mkBenchmark (hdphLocal ++ "/hdph-0.2.2/") [] (baseSpace $ binarySpace binList $ processSpace maxProcs)]

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
                      ,doClean      = False
                      -- Slight hack to pass the local install information into the sandbox compile stage.
                      ,pathRegistry = pathRegistry c `M.union` pathReg
                      }
  where pathReg = M.fromList [
                             -- Don't like how this is hardcoded - Either read
                             -- from a file or try and do it relative?
                             -- Probably do some form of intersperse function to make this neat.
                              ("extra-packages",(hdphLocal ++ "/hdph-mpi-allgather-0.0.2/ --flags=libMPICH2 --extra-include-dirs=" 
                              ++ (home ++ "/mpich/include") ++ " --extra-lib-dirs=" ++ (home ++ "/mpich/lib") 
                              ++ ":" ++ (hdphLocal ++ "/hdph-closure-0.2.0/")))
                             ]

-- TODO: Possibly check the gitref that way we can avoid destroying all sandbox
-- dirs and starting fresh.
grabRepository :: String -> FilePath -> IO ()
grabRepository rep loc = do
  e <- doesDirectoryExist loc
  case e of 
    True  -> putStrLn ("Removing existing repository at " ++ loc) >> removeDirectoryRecursive loc
    False -> return ()

  callCommand $ "git clone " ++ rep ++ " " ++ loc
