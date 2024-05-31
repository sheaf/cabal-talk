{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StaticPointers #-}

{-# OPTIONS_GHC -Wall #-}

module SetupHooks where

-- base
import Control.Monad.IO.Class
  ( liftIO )
import Data.Foldable
  ( for_ )
import qualified Data.List.NonEmpty as NE
  ( NonEmpty(..) )
import Data.Traversable
  ( for )

-- containers
import qualified Data.Map as Map
  ( fromList )

-- Cabal
import Distribution.Parsec
import Distribution.Simple.Flag
import Distribution.Simple.Glob.Internal -- TODO: shouldn't need internals
import Distribution.Simple.Glob
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Utils
import qualified Distribution.Types.LocalBuildConfig as LBC
import Distribution.Utils.Path
import Distribution.Utils.ShortText
import Distribution.Version

-- Cabal-hooks
import Distribution.Simple.SetupHooks

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { configureHooks = myConfigureHooks
    , buildHooks = myBuildHooks }

myConfigureHooks :: ConfigureHooks
myConfigureHooks =
  noConfigureHooks
    { preConfPackageHook = Just confMyPreProc }

myBuildHooks :: BuildHooks
myBuildHooks =
  noBuildHooks
    { preBuildComponentRules =
      Just $ rules ( static () ) myPreBuildRules
    }

--------------------------------------------------------------------------------
-- The custom preprocessor (as a program in the Cabal ProgramDb)

myPreProcName :: String
myPreProcName = "myPreProc"

myPreProcProg :: Program
myPreProcProg =
  ( simpleProgram myPreProcName )
    { programFindLocation =
        -- custom logic to find the installed location of myPreProc
        -- on the system used to build the package
        myPreProcProgFindLocation
    , programFindVersion =
        -- custom logic to find the program version,
        -- e.g. by parsing the output of "myPreProc --version"
        myPreProcProgFindVersion
    }

myPreProcProgFindLocation :: Verbosity -> ProgramSearchPath -> IO ( Maybe ( FilePath, [ FilePath ] ) )
myPreProcProgFindLocation verbosity searchPath =
  findProgramOnSearchPath verbosity searchPath myPreProcName
myPreProcProgFindVersion :: Verbosity -> FilePath -> IO ( Maybe Version )
myPreProcProgFindVersion verbosity progPath = do
  verStr <- rawSystemStdout verbosity progPath [ "--numeric-version" ]
  case simpleParsec verStr of
    Nothing -> error $ "Could not parse output of " ++ progPath ++ " --numeric-version"
    v@( Just {} ) -> return v

--------------------------------------------------------------------------------
-- Configure hook.

confMyPreProc :: PreConfPackageInputs -> IO PreConfPackageOutputs
confMyPreProc pcpi@( PreConfPackageInputs { configFlags = cfg, localBuildConfig = lbc } ) = do
  let verbosity = fromFlag $ configVerbosity cfg
      progDb = LBC.withPrograms lbc
  mbMyConfPreProcProg <-
    configureUnconfiguredProgram verbosity myPreProcProg progDb
  return $
    case mbMyConfPreProcProg of
      Nothing ->
        noPreConfPackageOutputs pcpi
      Just myConfPreProcProg ->
        ( noPreConfPackageOutputs pcpi )
          { extraConfiguredProgs = Map.fromList [ ( myPreProcName, myConfPreProcProg ) ] }

--------------------------------------------------------------------------------
-- Pre-build rules.

myPreBuildRules :: PreBuildComponentInputs -> RulesM ()
myPreBuildRules
  PreBuildComponentInputs
    { buildingWhat   = what
    , localBuildInfo = lbi
    , targetInfo     = TargetInfo { targetComponent = comp, targetCLBI = clbi }
    } = do
  let verbosity = buildingWhatVerbosity what
      progDb = withPrograms lbi
      bi = componentBuildInfo comp
      mbWorkDir = mbWorkDirLBI lbi
  -- Look up our custom pre-processor in the Cabal program database.
  for_ ( lookupProgramByName myPreProcName progDb ) $ \ myPreProc -> do
    -- 1. Define how to invoke our custom preprocessor.
    let myPpCmd :: Location -> Location -> Command MyPpArgs ( IO () )
        myPpCmd inputLoc outputLoc =
          mkCommand ( static Dict ) ( static ppModule )
            ( verbosity, mbWorkDir, myPreProc, inputLoc, outputLoc )

    -- 2. Search for "*.hs-mypp" files to pre-process in the source directories of the package.
    let glob = GlobDirRecursive [ WildCard, Literal "hs-mypp" ]
    myPpFiles <- liftIO $ for ( hsSourceDirs bi ) $ \ srcDir -> do
      let root = interpretSymbolicPath mbWorkDir srcDir
      matches <- runDirFileGlob verbosity Nothing root glob
      return
        [ Location srcDir ( makeRelativePathEx match )
        | match <- globMatches matches
        ]
    -- Monitor existence of file glob to handle new input files getting added.
    --   NB: we don't have to monitor the contents of the files, because the files
    --       are declared as inputs to rules, which means that their contents are
    --       automatically tracked.
    addRuleMonitors [ monitorFileGlobExistence $ RootedGlob FilePathRelative glob ]
      -- NB: monitoring a directory recursive glob isn't currently supported;
      -- but implementing support would be a nice newcomer-friendly task for cabal-install.
      -- See https://github.com/haskell/cabal/issues/10064.

    -- 3. Declare rules, one for each module to be preprocessed, with the
    --    corresponding preprocessor invocation.
    for_ ( concat myPpFiles ) $ \ inputLoc@( Location _ inputRelPath ) -> do
      let outputBaseLoc = autogenComponentModulesDir lbi clbi
          outputLoc =
            Location
              outputBaseLoc
              ( unsafeCoerceSymbolicPath $ replaceExtensionSymbolicPath inputRelPath "hs" )
      registerRule_ ( toShortText $ getSymbolicPath inputRelPath ) $
        staticRule ( myPpCmd inputLoc outputLoc ) [] ( outputLoc NE.:| [] )

type MyPpArgs = ( Verbosity, Maybe ( SymbolicPath CWD ( Dir Pkg ) ), ConfiguredProgram, Location, Location )
  -- NB: this could be a datatype instead, but it would need a 'Binary' instance.

ppModule :: MyPpArgs -> IO ()
ppModule ( verbosity, mbWorkDir, myPreProc, inputLoc, outputLoc ) = do
  let inputPath  = location inputLoc
      outputPath = location outputLoc
  createDirectoryIfMissingVerbose verbosity True $
    interpretSymbolicPath mbWorkDir $ takeDirectorySymbolicPath outputPath
  runProgramCwd verbosity mbWorkDir myPreProc
    [ getSymbolicPath inputPath, getSymbolicPath outputPath ]
