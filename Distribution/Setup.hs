-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Setup
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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

module Distribution.Setup (parseArgs, Action(..), hunitTests) where

 -- Local
import Distribution.Misc(LocalBuildInfo, CompilerFlavor(..),
                         Compiler(..), LocalBuildInfo(..))

-- Misc:
import HUnit (Test, (~:), (~=?))

-- |Parse the standard command-line arguments
parseArgs :: [String] -> CommandLineOpts
parseArgs _ = (NoCmd, [])

-- ------------------------------------------------------------
-- * command line
-- ------------------------------------------------------------


type CommandLineOpts = (Action,
                        [String]) -- The un-parsed remainder

data Action = ConfigCmd LocalBuildInfo
            | BuildCmd
            | InstallCmd
            | SDistCmd
            | PackageInfoCmd
            | RegisterCmd
            | NoCmd -- error case?
--             | TestCmd 1.0?
--             | BDist -- 1.0
--            | CleanCmd                 -- clean
    deriving (Show, Eq)


-- options :: [ OptDescr (Options -> IO Options) ]
-- options = 
--     [Option "p" ["prefix"]
--        (OptArg
--           (\arg opt -> return opt{command=useinfo...

-- Locate the compiler based on the flavor
exeLoc :: CompilerFlavor -> IO FilePath
exeLoc _ = return "error, not yet implemented" -- FIX

pkgLoc :: CompilerFlavor -> IO FilePath
pkgLoc _ = return "error, not yet implemented" -- FIX

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

hunitTests :: IO [Test]
hunitTests =
    do let basicGhcConfig = (ConfigCmd (LocalBuildInfo "/lib"
                                     (Compiler GHC "/bin/ghc"
                                                   "/bin/ghc-pkg")), [])
       let realGhcConfig = (ConfigCmd (LocalBuildInfo "" (Compiler Hugs "" "")), [])

       m <- sequence [do loc <- exeLoc comp
                         pkg <- pkgLoc comp
                         return (name, comp, loc, pkg)
                      | (name, comp) <- [("ghc", GHC), ("nhc", NHC), ("hugs", Hugs)]]
       return $ [-- Config:
                "config prefix ghc given package tool" ~: "failed" ~:
                        basicGhcConfig ~=? (parseArgs ["--prefix=/lib", "--ghc",
                                                        "--with-compiler=/bin/ghc",
                                                        "--with-pkg=/bin/ghc-pkg",
                                                        "configure"]),
               "find package tool" ~: "failed" ~:
                        basicGhcConfig ~=? (parseArgs ["--prefix=/lib", "--ghc",
                                                       "--with-compiler=/bin/ghc",
                                                       "configure"]),
               "locate compiler and package tool" ~: "failed" ~: 
                        realGhcConfig ~=? (parseArgs ["configure", "--ghc"]),
               "should we default to the current compiler?" ~: "failed" ~:
                        realGhcConfig ~=? (parseArgs ["configure"])]
               -- test location of various compilers
               ++ ["locate " ++ name ++ " and pkg tool" ~: "failed" ~:
                    (ConfigCmd (LocalBuildInfo "/usr/local"
                                (Compiler comp comploc pkgloc)), [])
                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name, "configure"])
                   | (name, comp, comploc, pkgloc) <- m]
               -- find the package tool
               ++ ["locate pkg tool given " ++ name ~: "failed" ~:
                    (ConfigCmd (LocalBuildInfo "/usr/local"
                                (Compiler comp comploc pkgloc)), [])
                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name,
                                   "--with-compiler="++name, "configure"])
                   | (name, comp, comploc, pkgloc) <- m]

               -- simpler commands:
               ++ [flag ~: "failed" ~: (flagCmd, []) ~=? (parseArgs [flag])
                   | (flag, flagCmd) <- [("build", BuildCmd),
                                         ("install", InstallCmd),
                                         ("sdist", SDistCmd),
                                         ("packageinfo", PackageInfoCmd),
                                         ("register", RegisterCmd)]
                  ]

{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}
