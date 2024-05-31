{-# OPTIONS_GHC -Wall #-}
module Main (main) where

import Distribution.Simple
import SetupHooks

main = defaultMainWithSetupHooks setupHooks
