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
) where

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
