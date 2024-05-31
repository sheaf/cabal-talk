{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE OverloadedStrings #-}

module SetupHooks ( setupHooks ) where

import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.SetupHooks
import Distribution.Simple.Utils
import Distribution.Utils.Path (getSymbolicPath, makeRelativePathEx)
import Distribution.Text

import System.Directory
import System.FilePath

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks =
        noBuildHooks
          { preBuildComponentRules =
              Just $ rules ( static () ) preBuildRules
          }
    }

preBuildRules :: PreBuildComponentInputs -> RulesM ()
preBuildRules
  ( PreBuildComponentInputs
    { buildingWhat   = what
    , localBuildInfo = lbi
    , targetInfo     = tgt }
  ) =
  case targetComponent tgt of
    CTest suite
      | unUnqualComponentName ( testName suite ) == testSuiteName
      -> do
        let
          autogenDir = autogenComponentModulesDir lbi ( targetCLBI tgt )
          cmd = mkCommand ( static Dict ) ( static generateBuildModule ) ( what, lbi, getSymbolicPath autogenDir )
        registerRule_ "build-module" $
          staticRule cmd [] ( NE.singleton $ Location autogenDir ( makeRelativePathEx $ buildSingletonsBaseModule <.> "hs" ) )
    _ -> return ()

generateBuildModule
  :: ( BuildingWhat, LocalBuildInfo, FilePath {- Autogen dir -} )
  -> IO ()
generateBuildModule (what, lbi, testAutogenDir) = do
  let pkg = localPkgDescr lbi
  rootDir <- getCurrentDirectory
  let verbosity = buildingWhatVerbosity what
      distPref  = getSymbolicPath $ buildingWhatDistPref what
      distPref' | isRelative distPref = rootDir </> distPref
                | otherwise           = distPref
      -- Package DBs
      dbStack = withPackageDB lbi ++ [ SpecificPackageDB $ distPref' </> "package.conf.inplace" ]
      dbFlags = "-hide-all-packages" : "-package-env=-" : packageDbArgsDb dbStack

      ghc = case lookupProgram ghcProgram (withPrograms lbi) of
              Just fp -> locationPath $ programLocation fp
              Nothing -> error "Can't find GHC path"
  createDirectoryIfMissingVerbose verbosity True testAutogenDir
  let buildSingletonsBaseFile = testAutogenDir </> buildSingletonsBaseModule <.> "hs"
  withLibLBI pkg lbi $ \_ libCLBI -> do
    let libDeps = map fst $ componentPackageDeps libCLBI
        pidx = case dependencyClosure (installedPkgs lbi) libDeps of
                 Left p  -> p
                 Right _ -> error "Broken dependency closure"
        libTransDeps = map installedUnitId $ allPackages pidx
        singletonsBaseUnitId = componentUnitId libCLBI
        deps = formatDeps (singletonsBaseUnitId:libTransDeps)
        allFlags = dbFlags ++ deps
    writeFile buildSingletonsBaseFile $ unlines
      [ "module Build_singletons_base where"
      , ""
      , "ghcPath :: FilePath"
      , "ghcPath = " ++ show ghc
      , ""
      , "ghcFlags :: [String]"
      , "ghcFlags = " ++ show allFlags
      , ""
      , "rootDir :: FilePath"
      , "rootDir = " ++ show rootDir
      ]
  where
    formatDeps = map formatOne
    formatOne installedPkgId = "-package-id=" ++ display installedPkgId

    -- GHC >= 7.6 uses the '-package-db' flag. See
    -- https://ghc.haskell.org/trac/ghc/ticket/5977.
    packageDbArgsDb :: [PackageDB] -> [String]
    -- special cases to make arguments prettier in common scenarios
    packageDbArgsDb dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs)
        | all isSpecific dbs              -> concatMap single dbs
      (GlobalPackageDB:dbs)
        | all isSpecific dbs              -> "-no-user-package-db"
                                           : concatMap single dbs
      dbs                                 -> "-clear-package-db"
                                           : concatMap single dbs
     where
       single (SpecificPackageDB db) = [ "-package-db=" ++ db ]
       single GlobalPackageDB        = [ "-global-package-db" ]
       single UserPackageDB          = [ "-user-package-db" ]
       isSpecific (SpecificPackageDB _) = True
       isSpecific _                     = False

buildSingletonsBaseModule :: FilePath
buildSingletonsBaseModule = "Build_singletons_base"

testSuiteName :: String
testSuiteName = "singletons-base-test-suite"
