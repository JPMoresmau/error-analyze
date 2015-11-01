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

import Data.List (sort)
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
    , testCase "Useless import" $ do
        checkCauses "Warning: Module `List' is imported, but nothing from it is used\n              (except perhaps instances visible in `List')"
            [UselessImport "List"]
        checkCauses "src/Language/Haskell/ErrorAnalyze.hs:23:1-18: Warning:\n    The import of ‘Debug.Trace’ is redundant\n      except perhaps to import instances from ‘Debug.Trace’\n    To import instances alone, use: import Debug.Trace()"
            [UselessImport "Debug.Trace"]
    , testCase "Useless import element" $ do
        checkCauses "test/Main.hs:25:1-31: Warning:\n    The import of ‘sortBy’ from module ‘Data.List’ is redundant"
            [UselessImportElement "Data.List" "sortBy"]
    , testCase "Mispellt Module" $ do
        checkCauses "src/Language/Haskell/ErrorAnalyze.hs:25:8-18:\n    Could not find module ‘Data.Maybe1’\n    Perhaps you meant\n      Data.Maybe (from base-4.8.0.0)\n      Data.Label (needs flag -package-key fclabels-2.0.2.3@fclab_G5tEoXqujdV8Q79iHWslf8)\n    Use -v to see a list of the files searched for."
            [MispelltModule "Data.Maybe1" [ModuleSuggestion "base" "4.8.0.0" Referenced "Data.Maybe",ModuleSuggestion "fclabels" "2.0.2.3" Unreferenced "Data.Label"]]
    , testCase "Discarded Do" $ do
        checkCauses "test/Main.hs:82:9-47: Warning:\n    A do-notation statement discarded a result of type ‘[Int]’\n    Suppress this warning by saying\n      ‘_ <- mapM ((.) return length) [\"toto\" :: String]’\n    or by using the flag -fno-warn-unused-do-bind"
            [MissingOption "-fno-warn-unused-do-bind"]
    ]



-- | check that given messages gives given causes, in any order
checkCauses :: T.Text -> [ErrorCause] -> IO()
checkCauses msg expected = do
    let actual = sort $ errorCauses msg
    (sort expected) @=? actual


