{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Core
import Test.Rewrite
import Test.RoundTrip
import Test.LatticeSurgery
import Test.STIM

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "HZX Tests"
  [ coreTests
  , rewriteTests
  , roundTripTests
  , latticeSurgeryTests
  , stimTests
  ]
