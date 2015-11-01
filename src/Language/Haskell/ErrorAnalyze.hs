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
 ( ErrorPackage, ErrorVersion, ErrorModule
 , ErrorCause(..)
 , ModuleSuggestion(..)
 , errorCauses
) where

import Data.Maybe
import qualified Data.Text as T

import Debug.Trace

-- | Simple synonym to indicate package names
type ErrorPackage = T.Text
-- | Simple synonym to indicate package versions
type ErrorVersion = T.Text
-- | Simple synonym to indicate module names
type ErrorModule = T.Text

data ModuleSuggestion = ModuleSuggestion ErrorPackage ErrorVersion ErrorModule
    deriving (Show,Read,Eq,Ord)

-- | The possible error causes
data ErrorCause
  -- | Package referenced in cabal file is unknown, needs to be installed, with the given version (may be -any)
  = UnknownPackage ErrorPackage ErrorVersion
  -- | A module from the package is referenced but the package is not in the build depends section of the cabal file
  | UnreferencedPackage ErrorPackage
  -- | The type signature is missing
  | MissingType T.Text
  -- | A module has been mispellt, give suggestions
  | MispelltModule T.Text [ModuleSuggestion]
  | WrongIdentifier T.Text T.Text
  -- | a full import statement is not needed (or only for instances)
  | UselessImport ErrorModule
  | UselessImportElement ErrorModule T.Text
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
            , moduleErrorAnalyzer
            , overloadedStringAnalyzer
            , missingTypeAnalyzer
            , uselessImportAnalyzer]

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

-- | An unreferenced package or a mispellt module
moduleErrorAnalyzer :: Analyzer
moduleErrorAnalyzer (msg,_)
    | T.isInfixOf couldNot msg
    , (bef,aft) <- T.breakOnEnd "you need to add" msg
    , not $ T.null bef
    , (bsp,_) <- T.breakOn " " $ T.stripStart aft
       = [UnreferencedPackage $ unquote bsp]
    | (_,aft) <- T.breakOn couldNot msg
    , not $ T.null aft
    , (ln1,aftln) <- T.breakOn "\n" $ T.drop (T.length couldNot) aft
    , not $ T.null ln1
    , modl <- unquote $ T.strip ln1
       = [MispelltModule modl (suggs $ T.strip aftln)]
    | otherwise = []
    where
        couldNot ="Could not find module"
        suggs aftln
            | T.isPrefixOf "Perhaps you meant" aftln
            , lns <- filter (not . T.isInfixOf "Use -v") $ map (unquote . T.strip) $ tail $ T.lines aftln
                = catMaybes $ map sugg lns
            | otherwise = []
        sugg ln
            | (bef,aft) <- T.breakOn " " ln
            , not $ T.null bef
            , Just brkts <- brackets aft
            , Just (pkg,vers) <- suggPkg brkts
                = Just $ ModuleSuggestion pkg vers(T.strip bef)
            | otherwise = Nothing
        suggPkg brk
            | (bef,aft) <- T.breakOnEnd "from " brk
            , not $ T.null bef
             = pkgCut aft
            | (bef,aft) <- T.breakOnEnd "needs flag -package-key " brk
            , not $ T.null bef
             = pkgCut aft
            | otherwise = Nothing
        pkgCut pv
            | (bef,aft) <- T.breakOnEnd "-" pv
            , not $ T.null bef
            , (befAt,_) <- T.breakOn "@" aft
                = Just (T.init bef,befAt)
            | otherwise = Nothing

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
       = beautifyTypes aft
    | (bef,aft) <- T.breakOnEnd "definition but no type signature" msg
    , not $ T.null bef
    , (bef1, aft1) <- T.breakOnEnd "inferred type:" aft
    , not $ T.null bef1
       = beautifyTypes aft1
    | otherwise = []
    where cleanType typ =
            let typs = T.splitOn "::" typ
                noPkg = map removePackage typs
            in T.intercalate "::" noPkg
          removePackage t =
            let (b,a) = T.breakOnEnd ":" t
            in T.append (T.takeWhile (\c->c `elem` [' ','(']) b) a
          beautifyTypes aft =
            let typ = T.strip aft
                (nam,rtyp) = T.breakOn "::" typ
                (_,sname) = T.breakOnEnd "." nam
                cleanTypes = T.intercalate " " $ map cleanType $ T.splitOn " " rtyp
            in [MissingType (T.concat [sname,cleanTypes])]

-- | Useless import
uselessImportAnalyzer :: Analyzer
uselessImportAnalyzer (msg,low)
    | T.isInfixOf "imported, but nothing from it is used" low
    , (bef,aft) <- T.breakOnEnd "Module" msg
    , not $ T.null bef
    , (modl,_) <- T.breakOn " " $ T.stripStart aft
        = [UselessImport (unquote modl)]
    | (bef,aft) <- T.breakOn "is redundant" msg
    , not $ T.null aft
    , (bef1,modl) <- T.breakOnEnd "import of" bef
    , not $ T.null bef1
    , (befM,aftM) <- T.breakOn "from module" modl
       = if T.null aftM
            then [UselessImport (unquote $ T.strip modl)]
            else [UselessImportElement (unquote $ T.strip $ T.drop 11 aftM) (unquote $ T.strip befM)]
    | otherwise = []

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

-- | Read next text into brackets if any
brackets :: T.Text -> Maybe T.Text
brackets t
    | (bef,aft) <- T.breakOn "(" t
    , not $ T.null bef
    , (bef1,_) <- T.breakOn ")" aft
    , not $ T.null bef1
      = Just $ T.strip bef1
    |otherwise = Nothing
