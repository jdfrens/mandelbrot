module Main (main) where

import System.Exit

import Test.HUnit
import OptionsTest

main :: IO ()
main = do
    counts2 <- runTestTT optionsModuleTests
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure
