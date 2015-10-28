{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :  Copyright JP Moresmau 2015
-- License     :  AllRightsReserved
--
-- Maintainer  : jp@moresmau.fr
-- Stability   : Experimental
-- Portability : Portable
--
-- |
--
-----------------------------------------------------------------------------

module Main (
    main
) where

import Language.Haskell.ErrorAnalyze

import Test.Tasty
import Test.Tasty.HUnit

import Data.List
import qualified Data.Text as T


-- | entry point
main :: IO()
main = defaultMain tests

-- | test definitions
tests :: TestTree
tests = testGroup "error-resolver Tests"
    [ testCase "Unknown Package" $ do
        checkCauses "cabal: At least the following dependencies are missing:\nunknown1 -any" [UnknownPackage "unknown1" "-any"]
        checkCauses "cabal: At least the following dependencies are missing:\nunknown1 -any, unknown2 -any" [UnknownPackage "unknown1" "-any",UnknownPackage "unknown2" "-any"]
        checkCauses "cabal: At least the following dependencies are missing:\nunknown1 >0.2, unknown2 -any" [UnknownPackage "unknown1" ">0.2",UnknownPackage "unknown2" "-any"]
        checkCauses "At least the following dependencies are missing:\nunknown1 -any" [UnknownPackage "unknown1" "-any"]
        checkCauses "At least the following dependencies are missing:\nunknown1 -any, unknown2 -any" [UnknownPackage "unknown1" "-any",UnknownPackage "unknown2" "-any"]
        checkCauses "At least the following dependencies are missing:\nunknown1 >0.2, unknown2 -any" [UnknownPackage "unknown1" ">0.2",UnknownPackage "unknown2" "-any"]
    , testCase "Unreferenced Package" $ do
        checkCauses "test/Main.hs:27:8-17:\n    Could not find module ‘Data.Aeson’\n    It is a member of the hidden package ‘aeson-0.8.0.2@aeson_13eQZfuKTVFKCCyISpvgPw’.\n    Perhaps you need to add ‘aeson’ to the build-depends in your .cabal file.\n    Use -v to see a list of the files searched for."
            [UnreferencedPackage "aeson"]
        checkCauses "Could not find module ‘Data.Aeson’\n    It is a member of the hidden package ‘aeson-0.8.0.2@aeson_13eQZfuKTVFKCCyISpvgPw’.\n    Perhaps you need to add ‘aeson’ to the build-depends in your .cabal file.\n    Use -v to see a list of the files searched for."
            [UnreferencedPackage "aeson"]
    , testCase "OverloadedStrings" $ do
        checkCauses "test/Main.hs:46:34-40:\n    Couldn't match expected type ‘T.Text’ with actual type ‘[Char]’\n    In the first argument of ‘UnreferencedPackage’, namely ‘\"aeson\"’\n    In the expression: UnreferencedPackage \"aeson\"\n    In the second argument of ‘checkCauses’, namely\n      ‘[UnreferencedPackage \"aeson\"]’"
            [MissingExtension "OverloadedStrings"]
        checkCauses "test/Main.hs:46:34-40:\n    Couldn't match expected type ‘T.Text’ with actual type `[Char]'\n    In the first argument of ‘UnreferencedPackage’, namely ‘\"aeson\"’\n    In the expression: UnreferencedPackage \"aeson\"\n    In the second argument of ‘checkCauses’, namely\n      ‘[UnreferencedPackage \"aeson\"]’"
            [MissingExtension "OverloadedStrings"]
        checkCauses "test/Main.hs:46:34-40:\n    Couldn't match expected type ‘T.Text’ with ‘[Char]’\n    In the first argument of ‘UnreferencedPackage’, namely ‘\"aeson\"’\n    In the expression: UnreferencedPackage \"aeson\"\n    In the second argument of ‘checkCauses’, namely\n      ‘[UnreferencedPackage \"aeson\"]’"
            [MissingExtension "OverloadedStrings"]
        checkCauses "test/Main.hs:46:34-40:\n    Couldn't match expected type ‘T.Text’ with `[Char]'\n    In the first argument of ‘UnreferencedPackage’, namely ‘\"aeson\"’\n    In the expression: UnreferencedPackage \"aeson\"\n    In the second argument of ‘checkCauses’, namely\n      ‘[UnreferencedPackage \"aeson\"]’"
            [MissingExtension "OverloadedStrings"]
    , testCase "Missing type" $ do
        checkCauses "Top-level binding with no type signature:\n               commonHome :: forall s a a1.\n                             (ToWidget s App a1, blaze-markup-0.5.1.0:Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml"
            [MissingType "commonHome :: forall s a a1.\n                             (ToWidget s App a1, Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml"]
        checkCauses "Top-level binding with no type signature:\n               commonHome :: forall s a a1.\n                             (blaze-markup-0.5.1.0:Text.Blaze.ToMarkup a1, blaze-markup-0.5.1.0:Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml"
            [MissingType "commonHome :: forall s a a1.\n                             (Text.Blaze.ToMarkup a1, Text.Blaze.ToMarkup a) =>\n                             a1 -> a -> Maybe Search -> GHandler s App RepHtml"]
        checkCauses "Top-level binding with no type signature:\n               f :: forall (x :: * -> *) y t. (t -> x y) -> t -> D x y"
            [MissingType "f :: forall (x :: * -> *) y t. (t -> x y) -> t -> D x y"]
        checkCauses "Top-level binding with no type signature:\n           fun :: forall t. t -> [Char] -> [Char]"
            [MissingType "fun :: forall t. t -> [Char] -> [Char]"]
        checkCauses "test/Main.hs:(35,1)-(69,5): Warning:\n    Top-level binding with no type signature: tests :: TestTree\n"
            [MissingType "tests :: TestTree"]
        checkCauses "test/Main.hs:(35,1)-(69,5): Warning:\n    Top-level binding with no type signature: Main.tests :: TestTree\n"
            [MissingType "tests :: TestTree"]

    ]


-- | check that given messages gives given causes, in any order
checkCauses :: T.Text -> [ErrorCause] -> IO()
checkCauses msg expected = do
    let actual = sort $ errorCauses msg
    (sort expected) @=? actual

