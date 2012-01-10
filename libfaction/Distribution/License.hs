{-
The License datatype.  For more information about these and other
open-source licenses, you may visit <http://www.opensource.org/>.

The @.faction@ file allows you to specify a license file. Of course you can
use any license you like but people often pick common open source licenses
and it's useful if we can automatically recognise that (eg so we can display
it on the hackage web pages). So you can also specify the license itself in
the @.faction@ file from a short enumeration defined in this module. It
includes 'GPL', 'LGPL' and 'BSD3' licenses.
-}

module Distribution.License (
    License(..),
    knownLicenses,
  ) where

import Distribution.Version (Version(Version))

import Distribution.Text (Text(..), display)
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlphaNum)

-- |This datatype indicates the license under which your package is
-- released.  It is also wise to add your license to each source file
-- using the license-file field.  The 'AllRightsReserved' constructor
-- is not actually a license, but states that you are not giving
-- anyone else a license to use or distribute your work.  The comments
-- below are general guidelines.  Please read the licenses themselves
-- and consult a lawyer if you are unsure of your rights to release
-- the software.
--
data License =

--TODO: * remove BSD4

    -- | GNU Public License. Source code must accompany alterations.
    GPL (Maybe Version)

    -- | Lesser GPL, Less restrictive than GPL, useful for libraries.
  | LGPL (Maybe Version)

    -- | 3-clause BSD license, newer, no advertising clause. Very free license.
  | BSD3

    -- | 4-clause BSD license, older, with advertising clause. You almost
    -- certainly want to use the BSD3 license instead.
  | BSD4

    -- | The MIT license, similar to the BSD3. Very free license.
  | MIT

    -- | Holder makes no claim to ownership, least restrictive license.
  | PublicDomain

    -- | No rights are granted to others. Undistributable. Most restrictive.
  | AllRightsReserved

    -- | Some other license.
  | OtherLicense

    -- | Not a recognised license.
    -- Allows us to deal with future extensions more gracefully.
  | UnknownLicense String
  deriving (Read, Show, Eq)

knownLicenses :: [License]
knownLicenses = [ GPL  unversioned, GPL  (version [2]),   GPL  (version [3])
                , LGPL unversioned, LGPL (version [2,1]), LGPL (version [3])
                , BSD3, MIT
                , PublicDomain, AllRightsReserved, OtherLicense]
 where
   unversioned = Nothing
   version   v = Just (Version v [])

instance Text License where
  disp (GPL  version)         = Disp.text "GPL"  <> dispOptVersion version
  disp (LGPL version)         = Disp.text "LGPL" <> dispOptVersion version
  disp (UnknownLicense other) = Disp.text other
  disp other                  = Disp.text (show other)

  parse = do
    name    <- Parse.munch1 (\c -> Char.isAlphaNum c && c /= '-')
    version <- Parse.option Nothing (Parse.char '-' >> fmap Just parse)
    return $! case (name, version :: Maybe Version) of
      ("GPL",               _      ) -> GPL  version
      ("LGPL",              _      ) -> LGPL version
      ("BSD3",              Nothing) -> BSD3
      ("BSD4",              Nothing) -> BSD4
      ("MIT",               Nothing) -> MIT
      ("PublicDomain",      Nothing) -> PublicDomain
      ("AllRightsReserved", Nothing) -> AllRightsReserved
      ("OtherLicense",      Nothing) -> OtherLicense
      _                              -> UnknownLicense $ name
                                     ++ maybe "" (('-':) . display) version

dispOptVersion :: Maybe Version -> Disp.Doc
dispOptVersion Nothing  = Disp.empty
dispOptVersion (Just v) = Disp.char '-' <> disp v
