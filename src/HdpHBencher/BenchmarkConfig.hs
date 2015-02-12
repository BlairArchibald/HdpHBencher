{-# LANGUAGE OverloadedStrings #-}

-- Module to deal with configuration options for the bechmarks.
-- Uses configurator to parse the input file (and do type conversions). We
-- provide a layer ontop with specific HdpH/Mpiexec and RTS options.
--
-- Author: Blair Archibald
-- Email: mrblairarchibald@gmail.com

module HdpHBencher.BenchmarkConfig
(
    BenchmarkConfig (..)
  , BenchmarkRunConf (..)
  , HdpHConf (..)
  , RTSConf (..)

  , buildHdpHArgString
  , buildRTSArgString
  , buildMPIExecArgString

  , constructBaseConfig
  , updateConf

  , getConfValueOrNothing
  , getConfValueOrFail
) where

import Data.Maybe (fromMaybe)

import qualified Data.Text as T (append, Text)
import qualified Data.HashMap.Strict as HM (lookup, HashMap)

import Data.Configurator ()
import Data.Configurator.Types (convert, Configured, Name, Value)

type ConfMap = HM.HashMap Name Value
type BenchmarkName = T.Text

data BenchmarkConfig =
  BenchmarkConfig
    { runConf     :: BenchmarkRunConf
    , hdphConf    :: HdpHConf
    , rtsConf     :: RTSConf
    , mpiExecConf :: MPIExecConf
    }
    deriving (Show)

data BenchmarkRunConf =
  BenchmarkRunConf
    { binaryLoc     :: FilePath
    , binaryName    :: String
    , benchmarkArgs :: Maybe String
    , variant       :: String
    }
    deriving (Show)

data HdpHConf =
  HdpHConf
    { hdphNumProcs       :: Int
    , interface          :: Maybe String
    , tcpStartup         :: Bool
    , startupHost        :: Maybe String
    , startupPort        :: Maybe String
    , hdphAdditionalArgs :: Maybe String
    }
    deriving (Show)

data RTSConf =
  RTSConf
    { numThreads       :: Maybe Int
    , heapSize         :: Maybe String
    , rtsAdditonalArgs :: Maybe String
    }
    deriving (Show)

-- TODO: Allow the user to select which mpiexec version they are using
-- (args differ based on version).
data MPIExecConf =
  MPIExecConf
    { mpiNumProcs      :: Int
    , hostFile         :: Maybe FilePath
    , mpiAdditonalArgs :: Maybe String
    }
    deriving (Show)

-- Default configs
defaultBenchmarkRunConf :: BenchmarkRunConf
defaultBenchmarkRunConf =
  BenchmarkRunConf
    { binaryLoc     = ""
    , binaryName    = ""
    , benchmarkArgs = Nothing
    , variant       = ""
    }

defaultHpdHConf :: HdpHConf
defaultHpdHConf =
  HdpHConf
   { hdphNumProcs       = 0
   , interface          = Nothing
   , tcpStartup         = False
   , startupHost        = Nothing
   , startupPort        = Nothing
   , hdphAdditionalArgs = Nothing
   }

defaultRTSConf :: RTSConf
defaultRTSConf =
  RTSConf
    { numThreads       = Nothing
    , heapSize         = Nothing
    , rtsAdditonalArgs = Nothing
    }

defaultMPIExecConf :: MPIExecConf
defaultMPIExecConf =
  MPIExecConf
    { mpiNumProcs      = 0
    , hostFile         = Nothing
    , mpiAdditonalArgs = Nothing
    }

-- Add global options to a default config space.
constructBaseConfig :: ConfMap -> BenchmarkConfig
constructBaseConfig cfg =
  BenchmarkConfig
    {
      runConf     = defaultBenchmarkRunConf
    , hdphConf    = globalHdpHConf
    , rtsConf     = globalRTSConf
    , mpiExecConf = globalMPIExecConf
    }
  where globalHdpHConf =
          defaultHpdHConf
          {
            hdphNumProcs       = fromMaybe 0 $ getConfValueOrNothing "numProcs" cfg
          , interface          = getConfValueOrNothing "interface" cfg
          , tcpStartup         = fromMaybe False $ getConfValueOrNothing "tcpStartup" cfg
          , startupHost        = getConfValueOrNothing "startupHost" cfg
          , startupPort        = getConfValueOrNothing "startupPort" cfg
          , hdphAdditionalArgs = getConfValueOrNothing "globalHdpHArgs" cfg
          }

        globalRTSConf =
          defaultRTSConf
          {
            numThreads       = getConfValueOrNothing "numThreads" cfg
          , rtsAdditonalArgs = getConfValueOrNothing "globalRTSArgs" cfg
          }

        globalMPIExecConf =
          defaultMPIExecConf
          {
            mpiNumProcs      = fromMaybe 0 $ getConfValueOrNothing "numProcs" cfg
          , hostFile         = getConfValueOrNothing "hostsFile" cfg
          , mpiAdditonalArgs = getConfValueOrNothing "MPIExecAdditionalArgs" cfg
  }

-- Possibly able to refactor this into the baseConf method
updateConf :: BenchmarkConfig -> ConfMap -> BenchmarkName -> BenchmarkConfig
updateConf baseConf cfg bname =
  let
      baseRunConf  = runConf  baseConf
      baseHdpHConf = hdphConf baseConf
      baseRTSConf  = rtsConf  baseConf
      baseMPIConf  = mpiExecConf baseConf
  in
      baseConf
        { runConf  = updateRunConf baseRunConf
        , hdphConf = updateHdpHConf baseHdpHConf
        , rtsConf  = updateRTSConf baseRTSConf
        , mpiExecConf = updateMPIExecConf baseMPIConf
        }
  where
    addPrefix = T.append (bname `T.append` ".")
    updateMaybeConf s cur =
      case cur of
       Nothing -> getConfValueOrNothing (addPrefix s) cfg
       Just x  -> Just x

    appendMaybeConf s cur =
      case cur of
       Nothing -> getConfValueOrNothing (addPrefix s) cfg
       Just x  -> let val = getConfValueOrNothing (addPrefix s) cfg in
                      case val of
                        Nothing -> Just x
                        Just v  -> Just (x ++ " " ++ v)

    updateRunConf c =
      c { binaryLoc     = getConfValueOrFail (addPrefix "binLoc") cfg
        , binaryName    = getConfValueOrFail (addPrefix "binName") cfg
        , benchmarkArgs = updateMaybeConf "args" (benchmarkArgs c)
        , variant       = fromMaybe "" $
                            getConfValueOrNothing (addPrefix "variant") cfg
        }

    updateHdpHConf c =
      c {
          hdphNumProcs       = fromMaybe
                                (hdphNumProcs c)
                                (getConfValueOrNothing (addPrefix "numProcs") cfg)
        , interface          = updateMaybeConf "interface" (interface c)
        , tcpStartup         = fromMaybe
                                (tcpStartup c)
                                (getConfValueOrNothing (addPrefix "tcpStartup") cfg)
        , startupHost        = updateMaybeConf "startupHost" (startupHost c)
        , startupPort        = updateMaybeConf "startupPort" (startupPort c)
        , hdphAdditionalArgs = appendMaybeConf "hdphArgs" (hdphAdditionalArgs c)
        }

    updateRTSConf c =
      c {
          numThreads       = updateMaybeConf "numThreads" (numThreads c)
        , rtsAdditonalArgs = appendMaybeConf "rtsArgs" (rtsAdditonalArgs c)
        }

    updateMPIExecConf c =
      c {
           mpiNumProcs = fromMaybe
                          (mpiNumProcs c)
                          (getConfValueOrNothing (addPrefix "numProcs") cfg)
        , hostFile         = updateMaybeConf "hostsFile" (hostFile c)
        , mpiAdditonalArgs = appendMaybeConf "mpiExecAdditionalArgs" (mpiAdditonalArgs c)
        }


-- TODO: Change to Text?
buildHdpHArgString :: HdpHConf -> String
buildHdpHArgString cfg = "+HdpH " ++ genArgs ++ " -HdpH"
  where genArgs   = collapseArgs printArgs
        printArgs = [nprocs, iface, startup, host, port, extra]
        nprocs    = "numProcs=" ++ show (hdphNumProcs cfg)
        iface     = maybe "" ("nic=" ++) (interface cfg)
        startup   = case (tcpStartup cfg) of
          True  -> "startupBackend=tcp"
          False -> ""
        host      = maybe "" ("startupHost=" ++) (startupHost cfg)
        port      = maybe "" ("startupPort=" ++) (startupPort cfg)
        extra     = fromMaybe "" (hdphAdditionalArgs cfg)

buildRTSArgString :: RTSConf -> String
buildRTSArgString cfg = "+RTS " ++ genArgs ++ " -RTS"
  where genArgs       = collapseArgs printArgs
        printArgs     = [threads, heap, extra]
        threads       = "-N" ++ show (fromMaybe 1 (numThreads cfg))
        heap          = maybe "" ("-A" ++) (heapSize cfg)
        extra         = fromMaybe "" (rtsAdditonalArgs cfg)

buildMPIExecArgString :: MPIExecConf -> String
buildMPIExecArgString cfg = collapseArgs printArgs
  where printArgs         = [hosts, nprocs, extra]
        nprocs            = "-n " ++ show (mpiNumProcs cfg)
        hosts             = maybe "" ("-hostfile " ++) (hostFile cfg)
        extra             = fromMaybe "" (mpiAdditonalArgs cfg)

-- Utility Functions
collapseArgs :: [String] -> String
collapseArgs = unwords . filter (/= "")

getConfValueOrNothing :: Configured a =>  Name -> ConfMap -> Maybe a
getConfValueOrNothing s m = case s `HM.lookup` m of
                          Just a  -> convert a
                          Nothing -> Nothing

getConfValueOrFail :: Configured a =>  Name -> ConfMap -> a
getConfValueOrFail s m = case s `HM.lookup` m of
                          Just a -> case convert a of
                            Just a' -> a'
                            Nothing -> error $ "Could not get: " ++ (show s) ++ " from config."
                          Nothing -> error   $ "Could not get: " ++ (show s) ++ " from config."
