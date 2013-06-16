{-# LANGUAGE OverloadedStrings #-}

module System.Authenticate.SmbClient (
    loginSmbClient,
    SmbClientResult
    ) where

import Control.Applicative
import Data.Char
import Data.Maybe
import Data.Text hiding (head)
import Prelude hiding (unlines, null, takeWhile)
import System.Exit
import System.Process
import System.Timeout

commandName = "smbclient"

-- |The login result type.
--
-- If success, return Right with actual username.
type SmbClientResult = Either () Text

-- |Authenticate with @smbclient@ command.
--
-- Return Right if login success, Left otherwise.
--
-- This does not accept empty username or password,
-- because those has always been success as Guest login.
loginSmbClient :: Text -- ^ Server
               -> Text -- ^ Domain
               -> Text -- ^ Username
               -> Text -- ^ Password
               -> IO SmbClientResult
loginSmbClient server domain username password = do
    timedResult <- timeout (10 * 1000000) process
    case timedResult of
        Just result -> return result
        Nothing     -> return $ Left ()
  where
    domain' = checkedText domain
    username' = checkedText username
    password' = checkedText password
    authInfo dom usr pas = [
        "username = " `append` usr,
        "password = " `append` pas,
        "domain = " `append` dom]
    authInfo' =
        unpack <$> unlines
        <$> (authInfo <$> domain' <*> username' <*> password')
    process =
        case authInfo' of
            Just a -> validate commandName server a
                      $ Right (fromJust username')
            _      -> return $ Left ()

validate command server authInfo successInfo = do
    (exitCode, _, _) <-
        readProcessWithExitCode command [
            "-L", unpack server,
            "-A", "/dev/stdin"]
        authInfo
    case exitCode of
        ExitSuccess -> return successInfo
        _           -> return $ Left ()

-- |Returns valid words or Nothing.
--
-- >>> checkedText $ pack "hello"
-- Just "hello"
-- >>> checkedText $ pack " hello"
-- Just "hello"
-- >>> checkedText $ pack "hell o "
-- Just "hell o"
-- >>> checkedText $ pack "\npassword="
-- Nothing
-- >>> checkedText $ pack "hello\npassword="
-- Just "hello"
-- >>> checkedText $ pack " "
-- Nothing
-- >>> checkedText $ pack "\n"
-- Nothing
-- >>> checkedText $ pack " \n"
-- Nothing
-- >>> checkedText $ pack "\t"
-- Nothing
checkedText = noneEmptyText . trimWhite . takeUntilNewline
  where
    noneEmptyText text =
        if null text
        then Nothing
        else Just text
    takeUntilNewline =
        takeWhile (not . isControl)
    trimWhite =
        dropAround isSpace
