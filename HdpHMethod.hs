{-# LANGUAGE NamedFieldPuns #-}
module HdpHMethod (hdphMethod) where

import HSBencher.Types
import HSBencher.Internal.Logging (log)
import HSBencher.Internal.Utils (runLogged)

import Control.Monad
import Control.Monad.Reader

import Text.Regex.Posix
import Data.List.Split (splitOn)

import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

import System.Process
import System.Directory
import System.FilePath
import Text.Printf
import Prelude hiding (log)

-- HdpH method is a modified cabal method to support sandboxes and mpiexec runs.
hdphMethod :: BuildMethod
hdphMethod = BuildMethod
  { methodName = "HdpH"
  , canBuild = dotcab `PredOr` InDirectoryWithExactlyOne dotcab
  , concurrentBuild = True

  --TODO: Use this to set the number of processes rather than threads.
  , setThreads = Just $ \n -> [ CompileParam "--ghc-option='-threaded' --ghc-option='-rtsopts'"
                              , RuntimeParam ("+RTS -N"++ show n++" -RTS")]

  , clean = \pathReg buildID _target -> do
      let sandBoxDir = _target ++ ".cabal-sandbox/"
      log $ tag++ " Removing sandbox at " ++ sandBoxDir
      liftIO $ do b <- doesDirectoryExist sandBoxDir
                  when b $ removeDirectoryRecursive sandBoxDir

  , compile = \ Config{pathRegistry, runTimeOut} bldid flags target -> do

     benchroot <- liftIO$ getCurrentDirectory
     let suffix = "_" ++ bldid
         cabalPath = M.findWithDefault "cabal" "cabal" pathRegistry
         _ghcPath  = M.findWithDefault "ghc"   "ghc"   pathRegistry
         binD      = benchroot </> "bin"
     liftIO $ createDirectoryIfMissing True binD

     dir <- liftIO $ getDir target -- Where the indiv benchmark lives.
     inDirectory dir $ do
       let tmpdir = benchroot </> dir </> "temp" ++ suffix
       _ <- runSuccessful tag $ "rm -rf " ++ tmpdir
       _ <- runSuccessful tag $ "mkdir "  ++ tmpdir
       log $ tag ++ " Switched to " ++ dir ++ ", and cleared temporary directory."

       let cmdSetup = cabalPath ++ " sandbox init"
           cmd0 = cabalPath ++ " install " ++ unwords flags
           cmd1 = cmd0 ++ " --only-dependencies"
           cmd2 = cmd0 ++ " --bindir="++tmpdir ++ " ./ --program-suffix=" ++suffix

       -- Setup a sandbox
       log$ tag++ " Setting up cabal sandbox " ++cmd1
       _ <- runSuccessful tag cmdSetup

       -- Install any local dependencies first
       let additionalPackages = M.lookup "extra-packages" pathRegistry
       case additionalPackages of
         Just p  -> let pkgs = splitOn ":" p in
                        forM_ pkgs $ \p -> do
                          log $ tag ++ "Installing " ++ p ++ " into the cabal sandbox"
                          let cmd = cabalPath ++ " install " ++ p ++ " " ++ unwords flags
                          runSuccessful tag cmd
         Nothing -> return ()

       -- Install the main package that we are testing.
       log $ tag ++ "Running cabal command for deps only: " ++ cmd1
       _ <- runSuccessful tag cmd1
       log $ tag ++ "Running cabal command to build benchmark: " ++ cmd2
       _ <- runSuccessful tag cmd2

       -- We filter the information from runtime flags. Would be nicer if we
       -- could pass these in as a map but HsBencher isn't currently set up to
       -- do this.
       let runit args envVars =
             let bin   = tmpdir </> lookupArg "bin" args ++ suffix
		 prog_args  = lookupArg "args" args
                 hosts = benchroot </> lookupArg "hostFile" args
                 numProcs = lookupArg "numProcs" args
              in CommandDescr
               {
                --TODO: Remove the hardcoded nic and possibly get num procs from the thread settings.
                command = ShellCommand 
                            ("mpiexec -launcher ssh -f " ++ hosts ++ " -n " ++ numProcs ++ " " 
                            ++ bin ++ " " ++ prog_args ++ " +HdpH numProcs=" ++ numProcs ++ " nic=p1p1  debug=9 -HdpH")
               ,envVars = envVars
               ,timeout = runTimeOut
               ,workingDir = Just tmpdir
               ,tolerateError = False
               }

       return (RunInPlace runit)
  }
 where
   dotcab = WithExtension ".cabal"
   tag = " [HdpHMethod] "
   lookupArg a args = stripColon . head $ filter (\f -> f =~ (a ++ ":.*") :: Bool) args
   stripColon s = let (b,w,a) = s =~ ":" :: (String,String,String) in a

--------------------------------------------------------------------------------
-- Helper routines: From the HsBencher built ins. (Not exported but I needed them)
--------------------------------------------------------------------------------

-- | Checks whether a `BuildMethod` works for a given file
-- matchesMethod :: BuildMethod -> FilePath -> IO Bool
-- matchesMethod BuildMethod{canBuild} path =
--   return $ filePredCheck canBuild path

-- | Our compilation targets might be either directories or file names.
getDir :: FilePath -> IO FilePath
getDir path = do
  b  <- doesDirectoryExist path
  b2 <- doesFileExist path
  if b
    then return path
    else if b2
         then return (takeDirectory path)
         else error$ "getDir: benchmark target path does not exist at all: "++path

inDirectory :: (MonadIO m) => FilePath -> m a -> m a
inDirectory dir act = do
  orig <- liftIO$ getCurrentDirectory
  liftIO$ setCurrentDirectory dir
  x <- act
  liftIO$ setCurrentDirectory orig
  return x
-- TODO: Use bracket, but it's only IO, not generalized:
  -- bracket (do o <- liftIO getCurrentDirectory
  --             setCurrentDirectory dir
  --             return o)
  --         (\orig -> liftIO$ setCurrentDirectory orig)
  --         (\_ -> act)

-- Returns actual files only
filesInDir :: FilePath -> IO [FilePath]
filesInDir d = do
  inDirectory d $ do
    ls <- getDirectoryContents "."
    filterM doesFileExist ls


-- | A simple wrapper for a command that is expected to succeed (and whose output we
-- don't care about).  Throws an exception if the command fails.
-- Returns lines of output if successful.
runSuccessful :: String -> String -> BenchM [B.ByteString]
runSuccessful tag cmd = do
  (res,lns) <- runLogged tag cmd
  case res of
    ExitError code  -> error$ "expected this command to succeed! But it exited with code "++show code++ ":\n  "++ cmd
    RunTimeOut {}   -> error$ "Methods.hs/runSuccessful - error! The following command timed out:\n  "++show cmd
    RunCompleted {} -> return lns
