{-# LANGUAGE OverloadedStrings #-}

module System.Authenticate.SmbClient (
    loginSmbClient
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

-- |Authenticate with @smbclient@ command.
--
-- Return True if login success, False otherwise.
--
-- This does not accept empty username or password,
-- because those has always been success as Guest login.
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
            _      -> return False

validate command server authInfo = do
    (exitCode, _, _) <-
        readProcessWithExitCode command [
            "-L", unpack server,
            "-A", "/dev/stdin"]
        authInfo
    case exitCode of
        ExitSuccess -> return True
        _           -> return False

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
