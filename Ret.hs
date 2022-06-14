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
    pathIsSymbolicLink,
  )
import System.FilePath
  ( takeDirectory,
    takeExtension,
    takeFileName,
    (</>),
  )
import System.Posix (getFileStatus)
import System.Posix.Files
import Control.Monad

data CheckSpec = forall t.
  CheckSpec
  { prepare :: FilePath -> IO (Maybe t),
    check :: t -> Check
  }

newtype Check = Check (FilePath -> IO (Maybe Integer))

checkCheck :: Check -> FilePath -> IO (Maybe Integer)
checkCheck (Check check) = check

prepareCheck :: FilePath -> CheckSpec -> IO (Maybe Check)
prepareCheck dir1 CheckSpec {prepare, check} = do
  r <- prepare dir1
  case r of
    Nothing -> return Nothing
    Just t -> return . Just $ check t

prepareChecks :: FilePath -> [CheckSpec] -> IO [Check]
prepareChecks dir1 checkspecs = catMaybes <$> mapM (prepareCheck dir1) checkspecs

ignoreNothing :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a
ignoreNothing f (Just a) (Just b) = Just $ f a b
ignoreNothing _ x Nothing = x
ignoreNothing _ Nothing y = y

_prepareAndRunChecks :: FilePath -> FilePath -> [CheckSpec] -> IO (Maybe Integer)
_prepareAndRunChecks dir1 dir2 checkspecs = do
  checks <- prepareChecks dir1 checkspecs
  runChecks dir2 checks

_whichChecks :: FilePath -> FilePath -> Map.Map String CheckSpec -> IO (Map.Map String (Maybe Integer))
_whichChecks dir1 dir2 checkspecs = do
  checks <- mapM (prepareCheck dir1) checkspecs
  x <- mapM (mapM (`checkCheck` dir2)) checks
  return $ fmap join x

runChecks :: FilePath -> [Check] -> IO (Maybe Integer)
runChecks dir checks = do
  rs <- mapM (`checkCheck` dir) checks
  return $ foldl (ignoreNothing min) Nothing rs

mainChecks :: [FilePath] -> Integer -> [Check] -> IO (Maybe Integer)
mainChecks dirs ix checks
  | ix >= genericLength dirs = return Nothing
  | otherwise = do
    let dir = genericIndex dirs ix
    r <- runChecks dir checks
    case r of
      Just o -> return . Just $ o + ix
      Nothing -> mainChecks dirs (ix + 1) checks

iterateToFix :: Eq a => (a -> a) -> a -> [a]
iterateToFix f x
  | x == x' = [x]
  | otherwise = x : iterateToFix f x'
  where
    x' = f x

mainCheckSpecs :: FilePath -> [CheckSpec] -> IO (Maybe FilePath)
mainCheckSpecs dir1 checkspecs = do
  checks <- prepareChecks dir1 checkspecs
  let dirs = iterateToFix takeDirectory dir1
  r <- mainChecks dirs 1 checks
  case r of
    Just ix
      | ix < genericLength dirs && ix > 0 ->
        return . Just $ genericIndex dirs ix
    _ -> return $ Just "/"

simpleCheckSpec :: (FilePath -> IO (Maybe Integer)) -> CheckSpec
simpleCheckSpec f =
  CheckSpec
    { prepare = \_ -> return (Just ()),
      check = \() -> Check f
    }

anyFileCheckSpec :: (String -> String -> Bool) -> CheckSpec
anyFileCheckSpec f = simpleCheckSpec \dir -> do
  files <- listDirectory dir
  if any (f dir) files
    then return (Just 0)
    else return Nothing

dirExistsCheckSpec :: String -> CheckSpec
dirExistsCheckSpec name = simpleCheckSpec \dir -> do
  exists <- doesDirectoryExist (dir </> name)
  if exists
    then return (Just 0)
    else return Nothing

fileExistsCheckSpec :: String -> CheckSpec
fileExistsCheckSpec name = simpleCheckSpec \dir -> do
  exists <- doesFileExist (dir </> name)
  if exists
    then return (Just 0)
    else return Nothing

checkspecNames :: Map.Map String CheckSpec
checkspecNames =
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
      ("chdisk", changedDeviceId)
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
        if deviceID stat /= id
          then return $ Just 0
          else return Nothing
    }

isSymlink :: CheckSpec
isSymlink = simpleCheckSpec \dir -> do
  isSymlink <- pathIsSymbolicLink dir
  if isSymlink
    then return (Just 0)
    else return Nothing

--main = do
--  args <- getArgs
