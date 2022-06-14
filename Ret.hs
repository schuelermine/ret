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

data Landmark = forall t.
  Landmark
  { prepare :: FilePath -> IO (Maybe t),
    check :: t -> Check
  }

newtype Check = Check (FilePath -> IO Bool)

checkCheck :: Check -> FilePath -> IO Bool
checkCheck (Check check) = check

prepareCheck :: FilePath -> Landmark -> IO (Maybe Check)
prepareCheck dir Landmark {prepare, check} = do
  r <- prepare dir
  case r of
    Nothing -> return Nothing
    Just t -> return . Just $ check t

prepareChecks :: FilePath -> [Landmark] -> IO [Check]
prepareChecks dir landmarks = catMaybes <$> mapM (prepareCheck dir) landmarks

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

mainLandmarks :: FilePath -> [Landmark] -> IO (Maybe FilePath)
mainLandmarks dir0 landmarks = do
  checks <- prepareChecks dir0 landmarks
  mainChecks (takeDirectory dir0) checks

simpleLandmark :: (FilePath -> IO Bool) -> Landmark
simpleLandmark f =
  Landmark
    { prepare = \_ -> return (Just ()),
      check = \() -> Check f
    }

anyFileLandmark :: (String -> String -> Bool) -> Landmark
anyFileLandmark f = simpleLandmark \dir -> do
  files <- listDirectory dir
  if any (f dir) files
    then return True
    else return False

dirExistsLandmark :: String -> Landmark
dirExistsLandmark name = simpleLandmark \dir -> doesDirectoryExist (dir </> name)

fileExistsLandmark :: String -> Landmark
fileExistsLandmark name = simpleLandmark \dir -> doesFileExist (dir </> name)

landmarksMap :: Map.Map String Landmark
landmarksMap =
  Map.fromList (
    map (\name -> (name, fileExistsLandmark name))
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
    <> map (\name -> (name, dirExistsLandmark name))
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

changelogFileExists :: Landmark
changelogFileExists = anyFileLandmark \_ file -> ((==) `on` CI.mk) (takeFileName file) "CHANGELOG"

licenseFileExists :: Landmark
licenseFileExists = anyFileLandmark \_ file -> ((==) `on` CI.mk) (takeFileName file) "LICENSE"

cabalFileExists :: Landmark
cabalFileExists = anyFileLandmark \_ file -> takeExtension file == ".cabal"

indexHtmlFileExists :: Landmark
indexHtmlFileExists =
  anyFileLandmark \_ file ->
    takeFileName file == "index.html" &&
    takeExtension file `elem` [".html", ".xhtml", ".htm"]

changedDeviceId :: Landmark
changedDeviceId =
  Landmark
    { prepare = \dir -> do
        stat <- getFileStatus dir
        return . Just $ deviceID stat,
      check = \id -> Check $ \dir -> do
        stat <- getFileStatus dir
        return $ deviceID stat /= id
    }

isSymlink :: Landmark
isSymlink = simpleLandmark pathIsSymbolicLink

isHomeDir :: Landmark
isHomeDir =
  Landmark
    { prepare = \_ -> do
        home <- getHomeDirectory
        return . Just $ home,
      check = \home -> Check $ \dir -> return $ home == dir
    }
