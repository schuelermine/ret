{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ret where

import Data.Char (toUpper)
import Data.Function (on, (&))
import Data.List (genericIndex, genericLength)
import qualified Data.CaseInsensitive as CI
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink, getHomeDirectory,
  )
import System.FilePath
  ( takeDirectory,
    takeExtension,
    takeFileName,
    (</>), takeDrive,
  )
import System.Posix (getFileStatus)
import System.Posix.Files (deviceID, getFileStatus)

data CheckSpec = forall t.
  CheckSpec
  { prepare :: FilePath -> IO (Maybe t),
    check :: t -> Check
  }

newtype Check = Check (FilePath -> IO Bool)

checkCheck :: Check -> FilePath -> IO Bool
checkCheck (Check check) = check

prepareCheck :: FilePath -> CheckSpec -> IO (Maybe Check)
prepareCheck dir CheckSpec {prepare, check} = do
  r <- prepare dir
  case r of
    Nothing -> return Nothing
    Just t -> return . Just $ check t

prepareChecks :: FilePath -> [CheckSpec] -> IO [Check]
prepareChecks dir checkspecs = catMaybes <$> mapM (prepareCheck dir) checkspecs

ignoreNothing :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
ignoreNothing f (Just a) (Just b) = Just $ f a b
ignoreNothing _ x Nothing = x
ignoreNothing _ Nothing y = y

runChecks :: FilePath -> [Check] -> IO Bool
runChecks dir checks = do
  rs <- mapM (`checkCheck` dir) checks
  return $ or rs

isRoot :: FilePath -> Bool
isRoot dir = takeDrive dir == dir

mainChecks :: FilePath -> [Check] -> IO (Maybe FilePath)
mainChecks dir checks
  | isRoot dir = return $ Just dir
  | otherwise = do
    r <- runChecks dir checks
    if r
      then return $ Just dir
      else mainChecks (takeDirectory dir) checks

mainCheckSpecs :: FilePath -> [CheckSpec] -> IO (Maybe FilePath)
mainCheckSpecs dir0 checkspecs = do
  checks <- prepareChecks dir0 checkspecs
  mainChecks (takeDirectory dir0) checks

simpleCheckSpec :: (FilePath -> IO Bool) -> CheckSpec
simpleCheckSpec f =
  CheckSpec
    { prepare = \_ -> return (Just ()),
      check = \() -> Check f
    }

anyFileCheckSpec :: (String -> String -> Bool) -> CheckSpec
anyFileCheckSpec f = simpleCheckSpec \dir -> do
  files <- listDirectory dir
  if any (f dir) files
    then return True
    else return False

dirExistsCheckSpec :: String -> CheckSpec
dirExistsCheckSpec name = simpleCheckSpec \dir -> doesDirectoryExist (dir </> name)

fileExistsCheckSpec :: String -> CheckSpec
fileExistsCheckSpec name = simpleCheckSpec \dir -> doesFileExist (dir </> name)

checkSpecsMap :: Map.Map String CheckSpec
checkSpecsMap =
  Map.fromList (
    map (\name -> (name, fileExistsCheckSpec name))
      [ "CMakeLists.txt",
        "Makefile",
        "meson.build",
        "build.xml",
        "stack.yaml",
        "manifest.json",
        "tsconfig.json",
        ".gitignore",
        ".sublime-project",
        ".sublime-workspace",
        ".envrc",
        "flake.nix",
        "shell.nix",
        "default.nix"
      ]
    <> map (\name -> (name, dirExistsCheckSpec name))
      [ ".git",
        ".hg",
        ".svn",
        ".pijul",
        ".bzr",
        ".darcs",
        ".vscode",
        ".direnv"
      ]
    <> [
      ("changelog", changelogFileExists),
      ("license", licenseFileExists),
      (".cabal", cabalFileExists),
      ("device", changedDeviceId),
      ("home", isHomeDir),
      ("symlink", isSymlink)
    ]
  )

changelogFileExists :: CheckSpec
changelogFileExists = anyFileCheckSpec \_ file -> ((==) `on` CI.mk) (takeFileName file) "CHANGELOG"

licenseFileExists :: CheckSpec
licenseFileExists = anyFileCheckSpec \_ file -> ((==) `on` CI.mk) (takeFileName file) "LICENSE"

cabalFileExists :: CheckSpec
cabalFileExists = anyFileCheckSpec \_ file -> takeExtension file == ".cabal"

indexHtmlFileExists :: CheckSpec
indexHtmlFileExists =
  anyFileCheckSpec \_ file ->
    takeFileName file == "index.html" &&
    takeExtension file `elem` [".html", ".xhtml", ".htm"]

changedDeviceId :: CheckSpec
changedDeviceId =
  CheckSpec
    { prepare = \dir -> do
        stat <- getFileStatus dir
        return . Just $ deviceID stat,
      check = \id -> Check $ \dir -> do
        stat <- getFileStatus dir
        return $ deviceID stat /= id
    }

isSymlink :: CheckSpec
isSymlink = simpleCheckSpec pathIsSymbolicLink

isHomeDir :: CheckSpec
isHomeDir =
  CheckSpec
    { prepare = \_ -> do
        home <- getHomeDirectory
        return . Just $ home,
      check = \home -> Check $ \dir -> return $ home == dir
    }
