{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Test.Tasty
import Test.Tasty.HUnit

import Test.Core
import Test.Rewrite
import Test.RoundTrip
import Test.LatticeSurgery
import Test.STIM
import Test.Parametric
import Test.Doubled
import Test.Verification

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "HZX Tests"
  [ coreTests
  , rewriteTests
  , roundTripTests
  , latticeSurgeryTests
  , stimTests
  , parametricTests
  , doubledTests
  , verificationTests
  ]
