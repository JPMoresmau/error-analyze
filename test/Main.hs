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
    , testCase "Mispelled Module" $ do
        checkCauses "src/Language/Haskell/ErrorAnalyze.hs:25:8-18:\n    Could not find module ‘Data.Maybe1’\n    Perhaps you meant\n      Data.Maybe (from base-4.8.0.0)\n      Data.Label (needs flag -package-key fclabels-2.0.2.3@fclab_G5tEoXqujdV8Q79iHWslf8)\n    Use -v to see a list of the files searched for."
            [MispelledModule "Data.Maybe1" [ModuleSuggestion "base" "4.8.0.0" Referenced "Data.Maybe",ModuleSuggestion "fclabels" "2.0.2.3" Unreferenced "Data.Label"]]
    , testCase "Discarded Do" $ do
        checkCauses "test/Main.hs:82:9-47: Warning:\n    A do-notation statement discarded a result of type ‘[Int]’\n    Suppress this warning by saying\n      ‘_ <- mapM ((.) return length) [\"toto\" :: String]’\n    or by using the flag -fno-warn-unused-do-bind"
            [MissingOption "-fno-warn-unused-do-bind"]
    , testCase "Mispelled Identifier" $ do
        checkCauses "Not in scope: foldl'\nPerhaps you meant one of these:\n  DM.foldl' (imported from Data.Map),\n  `foldl' (imported from Prelude),\n  `DM.foldl' (imported from Data.Map)\n"
            [MispelledIdentifier "foldl'" [IdentifierSuggestion "Data.Map" "DM.foldl'",IdentifierSuggestion "Prelude" "foldl",IdentifierSuggestion "Data.Map" "DM.foldl"]]
        checkCauses "Not in scope: foldl'"
            [MispelledIdentifier "foldl'" []]
        checkCauses "Not in scope: `foldM'\nPerhaps you meant one of these:\n  `foldr' (imported from Prelude),\n  `DM.foldr' (imported from Data.Map),\n  foldl' (imported from Prelude)\n"
            [MispelledIdentifier "foldM" [IdentifierSuggestion "Prelude" "foldr",IdentifierSuggestion "Data.Map" "DM.foldr",IdentifierSuggestion "Prelude" "foldl'"]]
        checkCauses "Not in scope: type constructor or class ‘ByteString’\nPerhaps you meant ‘BS.ByteString’ (imported from Data.ByteString.Lazy)\n"
            [MispelledIdentifier "ByteString" [IdentifierSuggestion "Data.ByteString.Lazy" "BS.ByteString"]]
        checkCauses "Not in scope: `assertEqual'"
            [MispelledIdentifier "assertEqual" []]
        checkCauses "Not in scope: type constructor or class `Array'"
            [MispelledIdentifier "Array" []]
        checkCauses " Not in scope: ‘<$>’"
            [MispelledIdentifier "<$>" []]
        checkCauses "src/Language/Haskell/ErrorAnalyze.hs:173:20-23:  Not in scope: data constructor ‘Data’"
            [MispelledIdentifier "Data" []]
    , testCase "Data Constructor imported" $ do
        checkCauses "test/Main.hs:28:20-23:\n    In module ‘Data.Maybe’:\n      ‘Just’ is a data constructor of ‘Maybe’\n    To import it use\n      ‘import’ Data.Maybe( Maybe( Just ) )\n    or\n      ‘import’ Data.Maybe( Maybe(..) )"
            [ConstructorImported "Data.Maybe" "Maybe" "Just"]
    , testCase "Missing extension" $ do
        checkCauses "test/Main.hs:109:1-9:\n    Parse error: naked expression at top level\n    Perhaps you intended to use TemplateHaskell"
            [MissingExtension "TemplateHaskell"]
        checkCauses "test/Main.hs:113:16-57:\n    Illegal constraint: forall a (Use ConstraintKinds to permit this)\n    In the type signature for ‘checkCauses’:\n      checkCauses :: forall a => T.Text -> [ErrorCause] -> IO ()"
            [MissingExtension "ConstraintKinds"]
        checkCauses "test/Main.hs:114:25:\n    Illegal symbol '.' in type\n    Perhaps you intended to use RankNTypes or a similar language\n    extension to enable explicit-forall syntax: forall <tvs>. <type>"
            [MissingExtension "RankNTypes",MissingExtension "ScopedTypeVariables",MissingExtension "ExistentialQuantification"]
        checkCauses "test/Main.hs:117:5: parse error: naked lambda expression ''"
            [MissingExtension "LambdaCase"]
        checkCauses "test/Main.hs:117:5: parse error on input `case'"
            [MissingExtension "LambdaCase"]
        checkCauses "test/Main.hs:124:21-28:\n    Couldn't match expected type ‘b1’ with actual type ‘b’\n      ‘b’ is a rigid type variable bound by\n          the type signature for\n            foob :: (b -> b) -> b -> (a -> b) -> Maybe a -> b\n          at test/Main.hs:119:18\n      ‘b1’ is a rigid type variable bound by\n           the type signature for val :: b1 at test/Main.hs:123:16"
            [MissingExtension "ScopedTypeVariables"]
    , testCase "cabal version" $ do
        checkCauses "Warning: Examples.cabal: A package using section syntax must \nspecify at least 'cabal-version: >= 1.2'."
            [IncorrectCabalVersion ">= 1.2"]
    ]


-- | check that given messages gives given causes, in any order
checkCauses :: T.Text -> [ErrorCause] -> IO()
checkCauses msg expected = do
    let actual = sort $ errorCauses msg
    (sort expected) @=? actual


