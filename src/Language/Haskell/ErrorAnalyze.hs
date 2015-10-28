{-# LANGUAGE OverloadedStrings, PatternGuards #-}
-----------------------------------------------------------------------------
--
-- Module      :  ErrorAnalyze
-- Copyright   :  Copyright JP Moresmau 2015
-- License     :  BSD3
--
-- Maintainer  : jp@moresmau.fr
-- Stability   : Experimental
-- Portability : Portable
--
-- | The library itself
--
-----------------------------------------------------------------------------

module Language.Haskell.ErrorAnalyze
 ( ErrorCause(..)
 , errorCauses
) where

import qualified Data.Text as T

import Debug.Trace

-- | The possible error causese
data ErrorCause
  -- | Package referenced in cabal file is unknown, needs to be installed, with the given version (may be -any)
  = UnknownPackage T.Text T.Text
  -- | A module from the package is referenced but the package is not in the build depends section of the cabal file
  | UnreferencedPackage T.Text
  -- | The type signature is missing
  | MissingType T.Text
  | WrongIdentifier T.Text T.Text
  | UselessImport T.Text
  | UselessImportElement T.Text T.Text
  | MissingOption T.Text
  -- | An extension is missing (to add in current source file or in Cabal file)
  | MissingExtension T.Text
  | IncorrectCabalVersion T.Text
    deriving (Show,Read,Eq,Ord)

-- | Get the possible causes for a given error message
errorCauses :: T.Text -> [ErrorCause]
errorCauses msg = let
    low = T.toLower msg
    in concatMap ($ (msg,low)) analyzers
    where analyzers =
            [ unknownPackageAnalyzer
            , unreferencedPackageAnalyzer
            , overloadedStringAnalyzer
            , missingTypeAnalyzer]

-- | Shortcut for analyzer: takes the message in original case and lower case, return the causes
type Analyzer = (T.Text,T.Text) -> [ErrorCause]

-- | An unknown package
unknownPackageAnalyzer :: Analyzer
unknownPackageAnalyzer (msg,_)
    | (_,aft) <- T.breakOn "the following dependencies are missing" msg
    , not $ T.null aft
    , (_,ln) <- T.breakOn "\n" aft
    , not $ T.null ln
    , ls <- T.strip ln
    , not $ T.null ls
    , pkgs <- map T.strip $ T.splitOn "," ls
    , nameVersions <- map (T.splitOn " ") pkgs
     = map toUP nameVersions
    | otherwise  = []
    where
        toUP (n:[]) = UnknownPackage n "-any"
        toUP (n:v:_) = UnknownPackage n v
        toUP [] = error "should not happen: empty list in toUP"

-- | An unreferenced package
unreferencedPackageAnalyzer :: Analyzer
unreferencedPackageAnalyzer (msg,low)
    | T.isInfixOf "could not find module" low
    , (bef,aft) <- T.breakOnEnd "you need to add" msg
    , not $ T.null bef
    , (bsp,_) <- T.breakOn " " $ T.stripStart aft
       = [UnreferencedPackage $ unquote bsp]
    | otherwise = []

-- | Need OverloadedStrings
overloadedStringAnalyzer :: Analyzer
overloadedStringAnalyzer (_,low)
    | uq <- unquote low
    ,      T.isInfixOf "with actual type [char]" uq
        || T.isInfixOf "with [char]" uq
      = [MissingExtension "OverloadedStrings"]
    | otherwise = []

-- | Missing type signature
missingTypeAnalyzer :: Analyzer
missingTypeAnalyzer (msg,_)
    | (bef,aft) <- T.breakOnEnd "Top-level binding with no type signature:" msg
    , not $ T.null bef
    , typ <- T.strip aft
    , (nam,rtyp) <- T.breakOn "::" typ
    , (_,sname) <- T.breakOnEnd "." nam
    , cleanTypes <- T.intercalate " " $ map cleanType $ T.splitOn " " rtyp
       = [MissingType (T.concat [sname,cleanTypes])]
    | otherwise = []
    where cleanType typ =
            let typs = T.splitOn "::" typ
                noPkg = map removePackage typs
            in T.intercalate "::" noPkg
          removePackage t =
            let (b,a) = T.breakOnEnd ":" t
            in T.append (T.takeWhile (\c->c `elem` [' ','(']) b) a

-- | Remove all quotes from given text (inside the text as well)
unquote :: T.Text -> T.Text
unquote = T.concatMap addNonQuote
    where addNonQuote c
            | isQuote c = T.empty
            | otherwise = T.singleton c
    --T.dropAround isQuote

-- | Is a character a quote?
isQuote :: Char -> Bool
isQuote c= c `elem` ['\'', '`','‘','’' ]
