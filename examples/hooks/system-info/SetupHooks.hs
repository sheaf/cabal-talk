{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}

{-# OPTIONS_GHC -Wall #-}

module SetupHooks where


-- basic-cpuid
import qualified System.Cpuid.Basic

-- Cabal
import Distribution.Compiler
import Distribution.Simple.Flag
import Distribution.Simple.LocalBuildInfo
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Utils.Path

-- Cabal-hooks
import Distribution.Simple.SetupHooks
import qualified Distribution.Simple.SetupHooks as PreConf
  ( PreConfComponentOutputs(..) )

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks = myConfigureHooks }

myConfigureHooks :: ConfigureHooks
myConfigureHooks =
  noConfigureHooks
    { postConfPackageHook  = Just writeSystemInfo
    , preConfComponentHook = Just confComps
    }

data SystemInfo = SystemInfo { supportsAVX2 :: !Bool }
  deriving stock ( Show, Read )

systemInfoFlags :: SystemInfo -> [ String ]
systemInfoFlags ( SystemInfo { supportsAVX2 } ) =
  [ "-mavx2" | supportsAVX2 ]

writeSystemInfo :: PostConfPackageInputs -> IO ()
writeSystemInfo ( PostConfPackageInputs { packageBuildDescr = pbd } ) = do
  let cfg = LBC.configFlags pbd
      distPref = fromFlag $ configDistPref cfg
      mbWorkDir = flagToMaybe $ configWorkingDir cfg
  supportsAVX2 <- System.Cpuid.Basic.supportsAVX2
  -- + more system-wide checks, if desired
  writeFile ( interpretSymbolicPath mbWorkDir $ systemInfoFile distPref )
    ( show $ SystemInfo { supportsAVX2 } )

systemInfoFile :: SymbolicPath Pkg ( Dir Dist ) -> SymbolicPath Pkg File
systemInfoFile distPref = distPref </> makeRelativePathEx "system-info"

confComps :: PreConfComponentInputs -> IO PreConfComponentOutputs
confComps pcci@( PreConfComponentInputs { packageBuildDescr = pbd, component = comp } ) = do
  let cfg = LBC.configFlags pbd
      distPref = fromFlag $ configDistPref cfg
      mbWorkDir = flagToMaybe $ configWorkingDir cfg
  sysInfo <- read <$> readFile ( interpretSymbolicPath mbWorkDir $ systemInfoFile distPref )
  let opts = systemInfoFlags sysInfo
      bi' = emptyBuildInfo
              { ccOptions = opts
              , cxxOptions = opts
              , options = PerCompilerFlavor opts []
              }
  return $
    ( noPreConfComponentOutputs pcci )
      { PreConf.componentDiff =
         buildInfoComponentDiff ( componentName comp ) bi'
      }
