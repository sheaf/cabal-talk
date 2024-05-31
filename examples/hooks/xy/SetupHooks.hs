{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StaticPointers #-}

{-# OPTIONS_GHC -Wall #-}

module SetupHooks where

-- base
import Data.Maybe
  ( fromMaybe )
import qualified Data.List.NonEmpty as NE
  ( singleton )

-- directory
import System.Directory
  ( getTemporaryDirectory )

-- Cabal
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.Db
import Distribution.Simple.Utils
import Distribution.Utils.Path

-- Cabal-hooks
import Distribution.Simple.SetupHooks

--------------------------------------------------------------------------------

setupHooks :: SetupHooks
setupHooks =
  noSetupHooks
    { buildHooks = myBuildHooks }

myBuildHooks :: BuildHooks
myBuildHooks =
  noBuildHooks
    { preBuildComponentRules =
      Just $ rules ( static () ) myPreBuildRules
    }

--------------------------------------------------------------------------------
-- Pre-build rules.

myPreBuildRules :: PreBuildComponentInputs -> RulesM ()
myPreBuildRules
  PreBuildComponentInputs
    { buildingWhat   = what
    , localBuildInfo = lbi
    , targetInfo     = TargetInfo { targetCLBI = clbi }
    } = do
  let verbosity = buildingWhatVerbosity what
      progDb = withPrograms lbi
      mbWorkDir = mbWorkDirLBI lbi
      srcDir = makeSymbolicPath "src" -- for illustration purposes only (don't do this at home)
      autogenDir = autogenComponentModulesDir lbi clbi
  -- Define the alex/happy commands.
  let alex  = fromMaybe ( error "alex not found"  ) $ lookupProgramByName "alex"  progDb
      happy = fromMaybe ( error "happy not found" ) $ lookupProgramByName "happy" progDb
      alexCmd  = mkCommand ( static Dict ) ( static runAlex )
      happyCmd = mkCommand ( static Dict ) ( static runHappy )
  -- Register a rule: run alex on Lexer.alex, producing Lexer.hs.
  let lexerInFile  = Location srcDir     ( makeRelativePathEx "Lexer.alex" )
      lexerOutFile = Location autogenDir ( makeRelativePathEx "Lexer.hs" )
  registerRule_ "alex:Lexer" $
    staticRule ( alexCmd ( verbosity, mbWorkDir, alex, lexerInFile, lexerOutFile ) )
      {- inputs  -} [ FileDependency lexerInFile ]
      {- outputs -} ( NE.singleton lexerOutFile )
  -- Register a rule: run happy on Parser.happy, producing Parser.hs.
  let parserInFile  = Location srcDir     (  makeRelativePathEx "Parser.happy" )
      parserOutFile = Location autogenDir (  makeRelativePathEx "Parser.hs" )
  registerRule_ "happy:Parser" $
    staticRule ( happyCmd ( verbosity, mbWorkDir, happy, parserInFile, parserOutFile ) )
      {- inputs  -} [ FileDependency parserInFile ]
      {- outputs -} ( NE.singleton parserOutFile )

runAlex, runHappy :: ( Verbosity, Maybe ( SymbolicPath CWD ( Dir Pkg ) ), ConfiguredProgram, Location, Location ) -> IO ()
runAlex  = runPp ( Suffix "x" )
runHappy = runPp ( Suffix "y" )

runPp :: Suffix
      -> ( Verbosity, Maybe ( SymbolicPath CWD ( Dir Pkg ) ), ConfiguredProgram, Location, Location )
      -> IO ()
runPp ( Suffix ppExt ) ( verbosity, mbWorkDir, ppProg, inLoc, outLoc ) = do
  -- Alex/Happy expect files with a specific extension,
  -- so we make a new temporary file and copy its contents,
  -- giving the file the expected file extension.
  tempDir <- makeSymbolicPath <$> getTemporaryDirectory
  withTempFileCwd mbWorkDir tempDir ( "." <> ppExt ) $ \ inputPpFile _ -> do
    copyFileVerbose verbosity
      ( interpretSymbolicPath mbWorkDir $ location inLoc )
      ( interpretSymbolicPath mbWorkDir inputPpFile )
    runProgramCwd verbosity mbWorkDir ppProg
      [ getSymbolicPath inputPpFile
      , "-o"
      , getSymbolicPath ( location outLoc )
      ]
