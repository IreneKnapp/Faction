-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.ParseUtils
-- Copyright   :  (c) The University of Glasgow 2004
-- 
-- Maintainer  :  libraries@haskell.org
-- Stability   :  alpha
-- Portability :  portable
--
-- Utilities for parsing PackageDescription and InstalledPackageInfo.


{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of the University nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

-- This module is meant to be local-only to Distribution...

-- #hide
module Distribution.ParseUtils (
	LineNo, PError(..), showError, myError, runP,
	StanzaField(..), splitStanzas, Stanza, singleStanza,
	parseFilePathQ, parseLibNameQ,
	parseModuleNameQ, parseDependency, parseOptVersion,
	parsePackageNameQ, parseVersionRangeQ,
	parseTestedWithQ, parseLicenseQ, parseExtensionQ, parseCommaList,
	showFilePath, showTestedWith, showDependency,
	simpleField, listField, licenseField, optsField, 
	parseReadS, parseQuoted,
  ) where

import Text.PrettyPrint.HughesPJ
import Distribution.License
import Distribution.Version
import Distribution.Extension
import Distribution.Package	( parsePackageName )
import Distribution.Compat.ReadP as ReadP hiding (get)
import Distribution.Compat.Error
import Distribution.Setup(CompilerFlavor(..))

import Data.Char

-- -----------------------------------------------------------------------------

type LineNo = Int

data PError = AmbigousParse String LineNo
            | NoParse String LineNo
            | FromString String (Maybe LineNo)
        deriving Show

instance Error PError where
        strMsg s = FromString s Nothing

runP :: LineNo -> String -> ReadP a a -> String -> Either PError a
runP lineNo field p s =
  case [ x | (x,"") <- results ] of
    [a] -> Right a
    []  -> case [ x | (x,ys) <- results, all isSpace ys ] of
             [a] -> Right a
             []  -> Left (NoParse field lineNo)
             _   -> Left (AmbigousParse field lineNo)
    _   -> Left (AmbigousParse field lineNo)
  where results = readP_to_S p s

showError :: PError -> String
showError (AmbigousParse f n)     = "Line "++show n++": Ambigous parse in field '"++f++"'"
showError (NoParse f n)           = "Line "++show n++": Parse of field '"++f++"' failed"
showError (FromString s (Just n)) = "Line "++show n++": " ++ s
showError (FromString s Nothing)  = s

myError :: LineNo -> String -> Either PError a
myError n s = Left $ FromString s (Just n)

data StanzaField a 
  = StanzaField 
      { fieldName     :: String
      , fieldShow     :: a -> Doc
      , fieldGet      :: a -> Doc
      , fieldSet      :: LineNo -> String -> a -> Either PError a
      }

simpleField :: String -> (a -> Doc) -> (ReadP a a) -> (b -> a) -> (a -> b -> b) -> StanzaField b
simpleField name showF readF get set = StanzaField name
   (\st -> text name <> colon <+> showF (get st))
   (showF . get)
   (\lineNo val st -> do
       x <- runP lineNo name readF val
       return (set x st))

listField :: String -> (a -> Doc) -> (ReadP [a] a) -> (b -> [a]) -> ([a] -> b -> b) -> StanzaField b
listField name showF readF get set = StanzaField name
   (\st -> case get st of
        [] -> empty
        lst ->
           text name <> colon <+> sep (punctuate comma (map showF lst)))
   (\st -> case get st of
        [] -> empty
        lst ->
           vcat (map (\value -> comma <+> showF value) lst))
   (\lineNo val st -> do
       xs <- runP lineNo name (parseCommaList readF) val
       return (set xs st))

licenseField :: String -> Bool -> (b -> License) -> (License -> b -> b) -> StanzaField b
licenseField name flag get set = StanzaField name
   (\st -> case get st of
             OtherLicense path | flag      -> text name <> colon <+> showFilePath path
                               | otherwise -> empty
             license'          | not flag  -> text name <> colon <+> text (show license')
                               | otherwise -> empty)
   (\st -> case get st of
             OtherLicense path | flag      -> showFilePath path
                               | otherwise -> empty
             license'          | not flag  -> text (show license')
                               | otherwise -> empty)
   (\lineNo val st ->
       if flag 
         then do 
            path <- runP lineNo name parseFilePathQ val
            return (set (OtherLicense path) st)
         else do
            x <- runP lineNo name parseLicenseQ val
            return (set x st))

optsField :: String -> CompilerFlavor -> (b -> [(CompilerFlavor,[String])]) -> ([(CompilerFlavor,[String])] -> b -> b) -> StanzaField b
optsField name flavor get set = StanzaField name
   (\st -> case lookup flavor (get st) of
        Just args -> text name <> colon <+> hsep (map text args)
        Nothing   -> empty)
   (\st -> case lookup flavor (get st) of
        Just args -> sep (map text args)
        Nothing   -> empty)
   (\_ val st -> 
       let
         old_val  = get st
         old_args = case lookup flavor old_val of
                       Just args -> args
                       Nothing   -> []
         val'     = filter (\(f,_) -> f/=flavor) old_val
       in return (set ((flavor,words val++old_args) : val') st))

type Stanza = [(LineNo,String,String)]

-- |Split a string into blank line-separated stanzas of
-- "Field: value" groups
splitStanzas :: String -> Either PError [Stanza]
splitStanzas = mapM (mapM brk) . map merge . groupStanzas . filter validLine . zip [1..] . lines
  where validLine (_,s) = case dropWhile isSpace s of
                            '-':'-':_ -> False      -- Comment
                            _         -> True
        groupStanzas :: [(Int,String)] -> [[(Int,String)]]
        groupStanzas [] = []
        groupStanzas xs = let (ys,zs) = break allSpaces xs
                           in ys : groupStanzas (dropWhile allSpaces zs)

allSpaces (_,xs) = all isSpace xs

-- |Split a file into "Field: value" groups, but blank lines have no
-- significance, unlike 'splitStanzas'.  A field value may span over blank
-- lines.
singleStanza :: String -> Either PError Stanza
singleStanza = mapM brk . merge . filter (not.allSpaces) . zip [1..] . lines

merge ((n,x):(_,c:s):ys) 
  | c == ' ' || c == '\t' = case dropWhile isSpace s of
                               ('.':s') -> merge ((n,x++"\n"++s'):ys)
                               s'       -> merge ((n,x++"\n"++s'):ys)
merge ((n,x):ys) = (n,x) : merge ys
merge []         = []

brk :: (Int,String) -> Either PError (Int,String,String)
brk (n,xs) = case break (==':') xs of
             (fld, ':':val) -> return (n, map toLower fld, dropWhile isSpace val)
             (_, _)       -> fail $ "Line "++show n++": Invalid syntax (no colon after field name)"

-- |parse a module name
parseModuleNameQ :: ReadP r String
parseModuleNameQ = parseQuoted mod <++ mod
 where mod = do 
	  c <- satisfy isUpper
	  cs <- munch (\x -> isAlphaNum x || x `elem` "_'.")
	  return (c:cs)

parseFilePathQ :: ReadP r FilePath
parseFilePathQ = parseReadS <++ (munch1 (\x -> isAlphaNum x || x `elem` "-+/_."))

parseReadS :: Read a => ReadP r a
parseReadS = readS_to_P reads

parseDependency :: ReadP r Dependency
parseDependency = do name <- parsePackageNameQ
                     skipSpaces
                     ver <- parseVersionRangeQ <++ return AnyVersion
                     skipSpaces
                     return $ Dependency name ver

parsePackageNameQ = parseQuoted parsePackageName <++ parsePackageName 
parseVersionRangeQ = parseQuoted parseVersionRange <++ parseVersionRange

parseOptVersion :: ReadP r Version
parseOptVersion = parseQuoted ver <++ ver
  where ver = parseVersion <++ return noVersion
	noVersion = Version{ versionBranch=[], versionTags=[] }

parseTestedWithQ :: ReadP r (CompilerFlavor,VersionRange)
parseTestedWithQ = parseQuoted tw <++ tw
  where tw = do compiler <- parseReadS
		skipSpaces
		version <- parseVersionRange <++ return AnyVersion
		skipSpaces
		return (compiler,version)

parseLicenseQ :: ReadP r License
parseLicenseQ = parseQuoted parseReadS <++ parseReadS

-- urgh, we can't define optQuotes :: ReadP r a -> ReadP r a
-- because the "compat" version of ReadP isn't quite powerful enough.  In
-- particular, the type of <++ is ReadP r r -> ReadP r a -> ReadP r a
-- Hence the trick above to make 'lic' polymorphic.

parseExtensionQ :: ReadP r Extension
parseExtensionQ = parseQuoted parseReadS <++ parseReadS

parseLibNameQ :: ReadP r String
parseLibNameQ = parseReadS <++ munch1 (\x -> not (isSpace x) && x /= ',')

parseCommaList :: ReadP r a -- ^The parser for the stuff between commas
               -> ReadP r [a]
parseCommaList p = sepBy p separator
    where separator = skipSpaces >> ReadP.char ',' >> skipSpaces

parseQuoted :: ReadP r a -> ReadP r a
parseQuoted p = between (ReadP.char '"') (ReadP.char '"') p

-- --------------------------------------------
-- ** Pretty printing

showFilePath :: FilePath -> Doc
showFilePath fpath
	| all (\x -> isAlphaNum x || x `elem` "-+/_.") fpath = text fpath
	| otherwise = doubleQuotes (text fpath)


showTestedWith :: (CompilerFlavor,VersionRange) -> Doc
showTestedWith (compiler,version) = text (show compiler ++ " " ++ showVersionRange version)

showDependency :: Dependency -> Doc
showDependency (Dependency name ver) = text name <+> text (showVersionRange ver)
