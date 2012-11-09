module Main (main) where

import System.Exit

import Test.HUnit
import Options2Test

main :: IO ()
main = do
    counts2 <- runTestTT tests
    if (errors counts2 + failures counts2 == 0)
        then exitSuccess
        else exitFailure