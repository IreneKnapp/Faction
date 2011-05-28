-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.CCompiler
-- Copyright   :  2011, Dan Knapp
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This simple package provides types and functions for interacting with
-- C compilers.  Currently it's just a type enumerating extant C-like
-- languages, which we call dialects.

{-
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
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

module Distribution.Simple.CCompiler
  (CDialect(..),
   cDialectFilenameExtension,
   filenameCDialect)
  where

import Data.Monoid
     ( Monoid(..) )
import System.FilePath
     ( takeExtension )


-- | Represents a dialect of C.  The Monoid instance expresses backward
--   compatibility, in the sense that 'mappend a b' is the least inclusive
--   dialect which both 'a' and 'b' can be correctly interpreted as.
data CDialect = C
              | ObjectiveC
              | CPlusPlus
              | ObjectiveCPlusPlus
              deriving (Show)


instance Monoid CDialect where
  mempty = C
  mappend C anything = anything
  mappend ObjectiveC CPlusPlus = ObjectiveCPlusPlus
  mappend CPlusPlus ObjectiveC = ObjectiveCPlusPlus
  mappend _ ObjectiveCPlusPlus = ObjectiveCPlusPlus
  mappend ObjectiveC _ = ObjectiveC
  mappend CPlusPlus _ = CPlusPlus
  mappend ObjectiveCPlusPlus _ = ObjectiveCPlusPlus


-- | Takes a dialect of C and whether code is intended to be passed through
--   the preprocessor, and returns a filename extension for containing that
--   code.
cDialectFilenameExtension :: CDialect -> Bool -> String
cDialectFilenameExtension C True = "c"
cDialectFilenameExtension C False = "i"
cDialectFilenameExtension ObjectiveC True = "m"
cDialectFilenameExtension ObjectiveC False = "mi"
cDialectFilenameExtension CPlusPlus True = "cpp"
cDialectFilenameExtension CPlusPlus False = "ii"
cDialectFilenameExtension ObjectiveCPlusPlus True = "mm"
cDialectFilenameExtension ObjectiveCPlusPlus False = "mii"


-- | Infers from a filename's extension the dialect of C which it contains,
--   and whether it is intended to be passed through the preprocessor.
filenameCDialect :: String -> Maybe (CDialect, Bool)
filenameCDialect filename = do
  extension <- case takeExtension filename of
                 "" -> Nothing
                 extension -> Just $ drop 1 extension
  case extension of
    "c" -> return (C, True)
    "i" -> return (C, False)
    "ii" -> return (CPlusPlus, False)
    "m" -> return (ObjectiveC, True)
    "mi" -> return (ObjectiveC, False)
    "mm" -> return (ObjectiveCPlusPlus, True)
    "M" -> return (ObjectiveCPlusPlus, True)
    "mii" -> return (ObjectiveCPlusPlus, False)
    "cc" -> return (CPlusPlus, True)
    "cp" -> return (CPlusPlus, True)
    "cxx" -> return (CPlusPlus, True)
    "cpp" -> return (CPlusPlus, True)
    "CPP" -> return (CPlusPlus, True)
    "c++" -> return (CPlusPlus, True)
    "C" -> return (CPlusPlus, True)
    _ -> Nothing
