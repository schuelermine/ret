{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import qualified Data.CaseInsensitive as CI
import Data.Char (toUpper)
import Data.Function (on, (&))
import Data.List (genericIndex, genericLength)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, mapMaybe)
import System.Directory
  ( XdgDirectory (XdgConfig),
    doesDirectoryExist,
    doesFileExist,
    getCurrentDirectory,
    getHomeDirectory,
    getXdgDirectory,
    listDirectory,
    pathIsSymbolicLink,
  )
import System.Directory.Internal.Prelude (exitFailure, getArgs)
import System.Exit (exitSuccess)
import System.FilePath
  ( isDrive,
    takeBaseName,
    takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import System.Posix (getFileStatus)
import System.Posix.Files (deviceID, getFileStatus)

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
  if null args
    then do
      configDir <- getXdgDirectory XdgConfig "ret"
      let configFile = configDir </> "landmarks.txt"
      exists <- doesFileExist configFile
      if exists
        then do
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

anyFile :: (String -> Bool) -> Landmark
anyFile f = simpleLandmark \dir ls -> do
  if any f ls
    then return True
    else return False

anyFileWithName :: String -> Landmark
anyFileWithName name = anyFile (== name)

anyFileWithExtension :: String -> Landmark
anyFileWithExtension name = anyFile \file -> takeExtension file == name

anyFileWithBaseNameCI :: String -> Landmark
anyFileWithBaseNameCI name = anyFile \file -> ((==) `on` CI.mk) (takeBaseName file) name

getNamedLandmarksUsing :: (String -> Landmark) -> [String] -> [(String, Landmark)]
getNamedLandmarksUsing f names = map (\name -> (name, f name)) $ names

landmarksMap :: Map.Map String Landmark
landmarksMap =
  Map.fromList
    ( getNamedLandmarksUsing
        anyFileWithName
        [ "_clang-format",
          ".bzr",
          ".clang-format",
          ".darcs",
          ".direnv",
          ".envrc",
          ".git",
          ".gitignore",
          ".hg",
          ".hgignore",
          ".npmignore",
          ".npmrc",
          ".pijul",
          ".pnpmfile.cjs",
          ".sublime-project",
          ".sublime-workspace",
          ".svn",
          ".vscode",
          ".yarnrc",
          "bower_components",
          "build.gradle",
          "build.sbt",
          "build.xml",
          "cabal.project",
          "cabal.sandbox.config",
          "Cargo.toml",
          "CMakeLists.txt",
          "compile_commands.json",
          "default.nix",
          "flake.lock",
          "flake.nix",
          "jsconfig.json",
          "jspm_packages",
          "Makefile.am",
          "Makefile",
          "manifest.json",
          "meson.build",
          "MODULE",
          "node_modules",
          "package-lock.json",
          "package-lock.json",
          "package.json",
          "pnpm-lock.yaml",
          "pom.xml",
          "shell.nix",
          "stack.yaml",
          "tsconfig.json",
          "tslint.json",
          "WORKSPACE",
          "yarn.lock"
        ]
        ++ getNamedLandmarksUsing
          anyFileWithBaseNameCI
          [ "changelog",
            "license",
            "readme"
          ]
        ++ getNamedLandmarksUsing
          anyFileWithExtension
          [ ".cabal",
            ".sln"
          ]
        ++ [ ("device", changedDeviceId),
             ("home", isHomeDir),
             ("index.html", indexHtmlFileExists),
             ("symlink", isSymlink)
           ]
    )

indexHtmlFileExists :: Landmark
indexHtmlFileExists =
  anyFile \file ->
    takeFileName file == "index.html"
      && takeExtension file `elem` [".html", ".xhtml", ".htm"]

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
