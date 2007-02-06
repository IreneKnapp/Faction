-- This is a quick hack for uploading packages to Hackage.
-- See http://hackage.haskell.org/trac/hackage/wiki/CabalUpload

import Network.Browser
import Network.HTTP

import Data.Maybe
import Network.URI
import Numeric
import System.Environment
import System.Exit
import System.IO
import System.Random

type Username = String
type Password = String

main :: IO ()
main = do args <- getArgs
          (user, pwd, paths) <- parseOptions args
          mapM_ (uploadPackage user pwd) paths

uploadPackage :: Username -> Password -> FilePath -> IO ()
uploadPackage user pwd path =
  do
  let uri = uploadURI
      auth = AuthBasic { auRealm    = "Hackage",
                         auUsername = user,
                         auPassword = pwd,
                         auSite     = uri }
  req <- mkRequest uri path
  (_,resp) <- browse (addAuthority auth >> request req)
  print resp
  putStrLn $ rspBody resp

uploadURI :: URI
uploadURI = fromJust $ parseURI "http://hackage.haskell.org/cgi-bin/hackage-scripts/protected/upload-pkg"

mkRequest :: URI -> FilePath -> IO Request
mkRequest uri path = 
    do pkg <- readFile path
       boundary <- genBoundary pkg
       let body = printMultiPart boundary (mkFormData path pkg)
       return $ Request {
                         rqURI = uri,
                         rqMethod = POST,
                         rqHeaders = [Header HdrContentType ("multipart/form-data; boundary="++boundary),
                                      Header HdrContentLength (show (length body))],
                         rqBody = body
                        }

genBoundary :: String -> IO String
genBoundary pkg = do i <- randomRIO (0x10000000000000,0xFFFFFFFFFFFFFF) :: IO Integer
                     return $ showHex i ""

mkFormData :: FilePath -> String -> [BodyPart]
mkFormData path pkg = 
    -- yes, web browsers are that stupid (re quoting)
    [BodyPart [Header hdrContentDisposition ("form-data; name=package; filename=\""++path++"\""),
               Header HdrContentType "application/x-gzip"] 
     pkg]

hdrContentDisposition :: HeaderName
hdrContentDisposition = HdrCustom "Content-disposition"

-- * Multipart, partly stolen from the cgi package.

data BodyPart = BodyPart [Header] String

printMultiPart :: String -> [BodyPart] -> String
printMultiPart boundary xs = 
    concatMap (printBodyPart boundary) xs ++ crlf ++ "--" ++ boundary ++ "--" ++ crlf

printBodyPart :: String -> BodyPart -> String
printBodyPart boundary (BodyPart hs c) = crlf ++ "--" ++ boundary ++ crlf ++ concatMap show hs ++ crlf ++ c

crlf :: String
crlf = "\r\n"

-- * Command-line options

parseOptions :: [String] -> IO (Username, Password, [FilePath])
parseOptions (user:pwd:tarballs) = return (user,pwd,tarballs)
parseOptions _ = do hPutStrLn stderr "Usage: cabal-upload <username> <password> [<tarball> ...]"
                    exitFailure
