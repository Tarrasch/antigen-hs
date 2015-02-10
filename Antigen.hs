{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Antigen where

import Prelude hiding (FilePath)
import Shelly hiding (path)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
import Data.Maybe (fromMaybe)
import Control.Monad (forM, (<=<))
default (T.Text)

-- | Configuration that contains what plugins you use
data AntigenConfiguration = AntigenConfiguration
  { plugins :: ![ZshPlugin]
  }

-- | Data type representing a zsh plugin
data ZshPlugin =
  ZshPlugin {
    storage :: RepoStorage,
    sourcingStrategy :: SourcingStrategy,
    sourcingLocations :: [FilePath]
  }


-- | Run this from repo directory, it should yield the files that must be
-- sourced in order, left to right.
type SourcingStrategy = Sh [FilePath]

-- | Source reposi
data RepoStorage =
    GitRepository { url :: !Text -- ^ Example: https://github.com/Tarrasch/zsh-functional
                  }
  | Development { filePath :: !FilePath -- ^ See 'developFromFileSystem'
                }
  deriving (Show, Eq)



(<$/>) :: Sh FilePath -> FilePath -> Sh FilePath
shPath <$/> path = (</> path) <$> shPath

-- | Root directory of where all generated files will be created
outputDirectory :: Sh FilePath
outputDirectory = fromText <$> fromMaybe ".antigen-hs" <$> get_env "ANTIGEN_HS_OUT"


-- | Directory the repositories will be stored
reposDirectory :: Sh FilePath
reposDirectory = outputDirectory <$/> "repos"


-- | The final output file which the user should source
--   (Note: init.zsh sources it automatically)
outputFileToSource :: Sh FilePath
outputFileToSource = outputDirectory <$/> "antigen-hs.zsh"


-- | default ZshPlugin
defZshPlugin :: ZshPlugin
defZshPlugin = ZshPlugin {
    storage = error "set storage please!",
    sourcingStrategy = strictSourcingStrategy,
    sourcingLocations = ["."]
  }


-- | Like `antigen bundle` from antigen. It assumes you want a github
-- repository.
bundle :: Text -> ZshPlugin
bundle githubRepoIdentifier = defZshPlugin {
    storage = GitRepository $ "https://github.com/" <> githubRepoIdentifier
  }


-- | A local repository, useful when testing plugins
--
-- Example: developFromFileSystem "/home/arash/repos/zsh-snakebite-completion"
developFromFileSystem :: FilePath -> ZshPlugin
developFromFileSystem filePath = defZshPlugin {
    storage = Development filePath
  }


-- | Get the folder in which the storage will be stored on disk
storagePath :: RepoStorage -> Sh FilePath
storagePath storage = case storage of
    GitRepository url -> reposDirectory <$/> fromText (santitize url)
    Development path  -> reposDirectory <$/> fromText (santitize $ toTextIgnore path)
  where
    santitize = T.concatMap aux
    aux ':' = "-COLON-"
    aux '/' = "-SLASH-"
    aux c   = T.singleton c


-- | Clone the repository if it already doesn't exist
ensurePlugin :: RepoStorage -> Sh ()
ensurePlugin storage = do
  needToClone <- pluginNeedsCloning storage
  when needToClone $ clonePlugin storage


-- | Yes if we need to clone the repository again
pluginNeedsCloning :: RepoStorage -> Sh Bool
pluginNeedsCloning storage@(GitRepository _) = fmap not $ test_d =<< (storagePath storage)
pluginNeedsCloning (Development _) = return True -- Since it's development


-- | Clone the repository
clonePlugin :: RepoStorage -> Sh ()
clonePlugin storage@(GitRepository url) =
  cmd "git" "clone" "--recursive" "--" url =<< (storagePath storage)
clonePlugin storage@(Development path) = do
  rm_rf =<< (storagePath storage)
  cp_r path =<< (storagePath storage)


-- | (Used in my REPL development)
samplePlugin :: ZshPlugin
samplePlugin = bundle "Tarrasch/zsh-functional"


-- | (Used in my REPL development)
sampleConfig :: AntigenConfiguration
sampleConfig = AntigenConfiguration [samplePlugin]


-- | Get files to source for a plugin
findPluginZshs :: ZshPlugin -> Sh [FilePath]
findPluginZshs plugin = do
  dir <- storagePath $ storage plugin
  chdir dir $ do
    fmap concat $ forM (sourcingLocations plugin) $ \loc ->
      chdir loc $ (sourcingStrategy plugin)

-- | Match for one single *.plugin.zsh file
strictSourcingStrategy :: SourcingStrategy
strictSourcingStrategy = do
    files <- pwd >>= ls
    case filter (endsWith ".plugin.zsh" . toTextIgnore) files of
      [file] -> return [file]
      [] -> terror $ "No *.plugin.zsh file! \
                     \See antigenSourcingStrategy example in README \
                     \on how to configure this"
      _  -> terror $ "Too many *.plugin.zsh files!"


-- | Find what to source, using the strategy described here:
--
-- As the author of antigen-hs doesn't like this method, it's not the default
--
-- https://github.com/zsh-users/antigen#notes-on-writing-plugins
antigenSourcingStrategy :: SourcingStrategy
antigenSourcingStrategy = do
  files <- pwd >>= ls
  let candidatePatterns = [".plugin.zsh", "init.zsh", ".zsh", ".sh"]
  let sfilt pat = filter (endsWith pat . toTextIgnore) files
  let filteredResults =  map sfilt candidatePatterns
  case [res | res <- filteredResults, not (null res)]  of
    (matchedFiles:_) -> return matchedFiles
    [] -> terror $ T.pack $ "No files to source among " ++ show files

-- | Source all files in the given order. Currently does no file existence
-- check or anything.
filePathsSourcingStrategy :: [FilePath] -> SourcingStrategy
filePathsSourcingStrategy paths = do
  currDir <- pwd
  return $ map (currDir </>) paths


-- | endsWith ".txt" "hello.txt" ==> True
endsWith :: Text -> Text -> Bool
endsWith needle haystack =
  T.null $ snd $ T.breakOnEnd needle haystack


isDevelopment :: AntigenConfiguration -> Bool
isDevelopment (AntigenConfiguration {..}) =
    any (isDev . storage) plugins
  where
    isDev (Development _) = True
    isDev _                  = False

-- | Get the content that will be put in the file to source.
--
-- Since we need to aboslutify the FilePaths, this function is not pure.
fileToSourceContent ::
        AntigenConfiguration
     -> [FilePath] -- ^ List of all the *.plugin.zsh files
     -> Sh Text -- ^ What the file should contain
fileToSourceContent config@(AntigenConfiguration {..}) pluginZshs = do
    pluginZshsAbs <- mapM absPath pluginZshs
    repoDirectories <- mapM (absPath <=< storagePath) (map storage plugins)
    return $ T.unlines $
      [ "# THIS FILE IS GENERATED BY antigen-hs!!!!\n"
      , T.unlines $ map (("source " <>) . toTextIgnore) pluginZshsAbs
      , T.unlines $ map (("fpath+=" <>) . toTextIgnore) repoDirectories
      ] ++ if isDevelopment config then
      [ "# Since your using antigen-hs in development mode"
      , "echo 'You run antigen-hs in development mode, startups are slower.'"
      , "antigen-hs-compile"
      ] else []


-- | Do an Sh action inside the home directory
inHomeDir :: Sh a -> Sh a
inHomeDir sh = do
    mHomeDir <- get_env "HOME"
    case mHomeDir of
      Nothing      -> terror "$HOME is not set. Aborting."
      Just homeDir -> chdir (fromText homeDir) sh


-- | The main function that will clone all the repositories and create the file
-- to be sourced by the user
antigen :: AntigenConfiguration -> Sh ()
antigen config@(AntigenConfiguration {..}) = inHomeDir $ do
    mkdir_p =<< reposDirectory
    mapM_ ensurePlugin (map storage plugins)
    pluginZshs <- fmap concat $ mapM findPluginZshs plugins
    contents <- fileToSourceContent config pluginZshs
    flip writefile contents =<< outputFileToSource
