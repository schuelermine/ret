{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Char (toUpper)
import Data.Function (on, (&))
import Data.List (genericIndex, genericLength)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import System.Directory
  ( doesDirectoryExist,
    doesFileExist,
    listDirectory,
    pathIsSymbolicLink, getHomeDirectory, getCurrentDirectory, getXdgDirectory, XdgDirectory (XdgConfig),
  )
import System.FilePath
  ( takeDirectory,
    takeExtension,
    takeFileName,
    (</>), isDrive,
  )
import System.Posix (getFileStatus)
import System.Posix.Files (deviceID, getFileStatus)
import System.Directory.Internal.Prelude (exitFailure, getArgs)
import System.Exit (exitSuccess)
import qualified Data.CaseInsensitive as CI

data Landmark = forall t.
  Landmark
  { prepare :: FilePath -> IO (Maybe t),
    check :: t -> Check
  }

newtype Check = Check (FilePath -> [String] -> IO Bool)

checkCheck :: Check -> FilePath -> [String] -> IO Bool
checkCheck (Check check) = check

prepareCheck :: FilePath -> Landmark -> IO (Maybe Check)
prepareCheck dir Landmark {prepare, check} = do
  result <- prepare dir
  case result of
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
  ls <- listDirectory dir
  results <- mapM (\check -> checkCheck check dir ls) checks
  return $ or results

mainChecks :: FilePath -> [Check] -> IO (Maybe FilePath)
mainChecks dir checks
  | isDrive dir = return Nothing
  | otherwise = do
    result <- runChecks dir checks
    if result
      then return $ Just dir
      else mainChecks (takeDirectory dir) checks

mainLandmarks :: FilePath -> [Landmark] -> IO (Maybe FilePath)
mainLandmarks dir0 landmarks = do
  checks <- prepareChecks dir0 landmarks
  mainChecks (takeDirectory dir0) checks

select :: Ord k => Map.Map k v -> [k] -> [v]
select m = mapMaybe (`Map.lookup` m)

mainLandmarkNames :: FilePath -> [String] -> IO (Maybe FilePath)
mainLandmarkNames dir0 names = mainLandmarks dir0 $ select landmarksMap names

main :: IO ()
main = do
  names <- getLandmarkNames
  dir0 <- getCurrentDirectory
  result <- mainLandmarkNames dir0 names
  case result of
    Nothing -> exitFailure
    Just dir -> do
      putStrLn dir
      exitSuccess

getLandmarkNames :: IO [String]
getLandmarkNames = do
  args <- getArgs
  if null args then do
    configDir <- getXdgDirectory XdgConfig "ret"
    let configFile = configDir </> "landmarks.txt"
    exists <- doesFileExist configFile
    if exists then do
      names <- readFile configFile
      return $ lines names
    else return defaultLandmarkNames
  else return args

defaultLandmarkNames :: [String]
defaultLandmarkNames = Map.keys landmarksMap

simpleLandmark :: (FilePath -> [String] -> IO Bool) -> Landmark
simpleLandmark f =
  Landmark
    { prepare = \_ -> return (Just ()),
      check = \() -> Check f
    }

anyFileLandmark :: (String -> Bool) -> Landmark
anyFileLandmark f = simpleLandmark \dir ls -> do
  if any f ls
    then return True
    else return False

dirExistsLandmark :: String -> Landmark
dirExistsLandmark name = simpleLandmark \dir ls -> doesDirectoryExist (dir </> name)

fileExistsLandmark :: String -> Landmark
fileExistsLandmark name = simpleLandmark \dir ls -> doesFileExist (dir </> name)

landmarksMap :: Map.Map String Landmark
landmarksMap =
  Map.fromList (
    map (\name -> (name, anyFileLandmark (== name)))
      [
        "CMakeLists.txt",
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
        "default.nix",
        "Makefile.am",
        "pom.xml",
        ".hgignore",
        "jsconfig.json",
        "tslint.json",
        "package.json",
        "package-lock.json",
        "yarn.lock",
        "cabal.project",
        "cabal.sandbox.config",
        "pnpm-lock.yaml",
        ".pnpmfile.cjs",
        "package-lock.json",
        ".npmrc",
        ".yarnrc",
        ".npmignore",
        "flake.lock",
        ".git",
        ".hg",
        ".svn",
        ".pijul",
        ".bzr",
        ".darcs",
        ".vscode",
        ".direnv",
        "bower_components",
        "node_modules",
        "jspm_packages",
        "Cargo.toml",
        ".clang-format",
        "_clang-format",
        "compile_commands.json",
        "pom.xml",
        "build.gradle",
        "build.sbt"
      ]
    <> map (\name -> (name, anyFileLandmark \file -> ((==) `on` CI.mk) (takeFileName file) name))
      [
        "changelog",
        "license",
        "readme"
      ]
    <> map (\name -> (name, anyFileLandmark \file -> takeExtension file == name))
      [
        ".cabal",
        ".sln"
      ]
    <> [
      ("device", changedDeviceId),
      ("home", isHomeDir),
      ("symlink", isSymlink),
      ("index.html", indexHtmlFileExists)
    ]
  )

slnFileExists :: Landmark
slnFileExists = anyFileLandmark \file -> takeExtension file == ".sln"

cabalFileExists :: Landmark
cabalFileExists = anyFileLandmark \file -> takeExtension file == ".cabal"

indexHtmlFileExists :: Landmark
indexHtmlFileExists =
  anyFileLandmark \file ->
    takeFileName file == "index.html" &&
    takeExtension file `elem` [".html", ".xhtml", ".htm"]

changedDeviceId :: Landmark
changedDeviceId =
  Landmark
    { prepare = \dir -> do
        stat <- getFileStatus dir
        return . Just $ deviceID stat,
      check = \id -> Check $ \dir ls -> do
        stat <- getFileStatus dir
        return $ deviceID stat /= id
    }

isSymlink :: Landmark
isSymlink = simpleLandmark \dir ls -> pathIsSymbolicLink dir

isHomeDir :: Landmark
isHomeDir =
  Landmark
    { prepare = \_ -> do
        home <- getHomeDirectory
        return . Just $ home,
      check = \home -> Check $ \dir ls -> return $ home == dir
    }
