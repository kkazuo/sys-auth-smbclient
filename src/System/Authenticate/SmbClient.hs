module System.Authenticate.SmbClient (
    loginSmbClient
    ) where

import Data.Text
import Prelude hiding (unlines)
import System.Exit
import System.Process
import System.Timeout

commandName = "smbclient"

loginSmbClient :: Text -- ^ Server
               -> Text -- ^ Domain
               -> Text -- ^ Username
               -> Text -- ^ Password
               -> IO Bool
loginSmbClient server domain username password = do
    timedResult <- timeout (10 * 1000000) process
    case timedResult of
        Just result -> return result
        Nothing     -> return False
  where
    authInfo = [
        "username = ",
        "password = ",
        "domain = " `append` domain]
    process = do
        (exitCode, out, err) <-
            readProcessWithExitCode commandName [
                "-L", unpack server,
                "-A", "/dev/stdin"]
            $ unpack $ unlines authInfo
        case exitCode of
            ExitSuccess -> return True
            _ -> return False
