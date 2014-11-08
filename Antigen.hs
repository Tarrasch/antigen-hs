{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Antigen where

import Prelude hiding (FilePath)
import Shelly hiding (path)
import Filesystem.Path (directory)
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
default (T.Text)

-- | Configuration that contains what plugins you use
data AntigenConfiguration = AntigenConfiguration
  { plugins :: ![ZshPlugin]
  }

-- | Data type representing a zsh plugin
data ZshPlugin =
    GitRepository { url :: !Text -- ^ Example: https://github.com/Tarrasch/zsh-functional
                  }
  | Development { filePath :: !FilePath -- ^ See 'developFromFileSystem'
                }
  deriving (Show, Eq)


-- | Root directory of where all generated files will be created
outputDirectory :: FilePath
outputDirectory = ".antigen-hs"


-- | Directory the repositories will be stored
reposDirectory :: FilePath
reposDirectory = outputDirectory </> "repos"


-- | The final output file which the user should source
--   (Note: init.zsh sources it automatically)
outputFileToSource :: FilePath
outputFileToSource = outputDirectory </> "antigen-hs.zsh"


-- | Like `antigen bundle` from antigen. It assumes you want a github
-- repository.
bundle :: Text -> ZshPlugin
bundle = GitRepository . ("https://github.com/" <>)


-- | A local repository, useful when testing plugins
--
-- Example: developFromFileSystem "/home/arash/repos/zsh-snakebite-completion"
developFromFileSystem :: FilePath -> ZshPlugin
developFromFileSystem = Development

-- | split Text on spaces
spaceSplit text = T.split (== ' ') text

-- | Get the repository part of the url/local repo
repoPart :: Text -> Text
repoPart pluginText = head $ spaceSplit pluginText

-- | Get the optional sub-repo extension of a plugin
subPath :: ZshPlugin -> FilePath
subPath plugin = let sub = tail $ spaceSplit $ getPluginText plugin
    in case sub of
      [] -> fromText "."
      x:_ -> fromText x

-- | Get the plugin in text form
getPluginText plugin = case plugin of
    GitRepository url -> url
    Development path  -> toTextIgnore path

-- | Get the folder in which the plugin will be stored on disk
pluginPath :: ZshPlugin -> FilePath
pluginPath plugin = case plugin of
    GitRepository url -> reposDirectory </> fromText (santitize $ repoPart url)
    Development path  -> reposDirectory </> fromText
                         (santitize $ repoPart $ toTextIgnore path)
  where
    santitize = T.concatMap aux
    aux ':' = "-COLON-"
    aux '/' = "-SLASH-"
    aux c   = T.singleton c


-- | Clone the repository if it already doesn't exist
ensurePlugin :: ZshPlugin -> Sh ()
ensurePlugin plugin = do
  needToClone <- pluginNeedsCloning plugin
  when needToClone $ clonePlugin plugin


-- | Yes if we need to clone the repository again
pluginNeedsCloning :: ZshPlugin -> Sh Bool
pluginNeedsCloning plugin@(GitRepository _) = fmap not $ test_d (pluginPath plugin)
pluginNeedsCloning (Development _) = return True -- Since it's development


-- | Clone the repository
clonePlugin :: ZshPlugin -> Sh ()
clonePlugin plugin@(GitRepository url) =
  cmd "git" "clone" "--recursive" "--" (repoPart url) (pluginPath plugin)
clonePlugin plugin@(Development path) = do
  rm_rf (pluginPath plugin)
  cp_r (fromText $ repoPart $ toTextIgnore path) (pluginPath plugin)


-- | (Used in my REPL development)
samplePlugin :: ZshPlugin
samplePlugin = bundle "Tarrasch/zsh-functional"


-- | (Used in my REPL development)
sampleConfig :: AntigenConfiguration
sampleConfig = AntigenConfiguration [samplePlugin]

-- | get the repo path plus the subpath to the dir/file to source
getExtPath plugin = (pluginPath plugin) </> (subPath plugin)

-- | Match for *.plugin.zsh file, or the plugin itself if it is a file
findPluginZsh :: ZshPlugin -> Sh FilePath
findPluginZsh plugin = let ppath = getExtPath plugin in do
  fileP <- test_f ppath
  if fileP then return ppath else do
    files <- ls ppath
    case filter (endsWith ".plugin.zsh" . toTextIgnore) files of
      [file] -> return file
      [] -> terror "No *.plugin.zsh file!"
      _  -> terror "Too many *.plugin.zsh files!"

-- | Get directory for fpath
findPluginDir :: ZshPlugin -> FilePath
findPluginDir plugin = directory $ getExtPath plugin

-- | endsWith ".txt" "hello.txt" ==> True
endsWith :: Text -> Text -> Bool
endsWith needle haystack =
  T.null $ snd $ T.breakOnEnd needle haystack


isDevelopment :: AntigenConfiguration -> Bool
isDevelopment (AntigenConfiguration {..}) =
    any isDev plugins
  where
    isDev (Development _) = True
    isDev _               = False

-- | Get the content that will be put in the file to source.
--
-- Since we need to aboslutify the FilePaths, this function is not pure.
fileToSourceContent ::
        AntigenConfiguration
     -> [FilePath] -- ^ List of all the *.plugin.zsh files
     -> Sh Text -- ^ What the file should contain
fileToSourceContent config@(AntigenConfiguration {..}) pluginZshs = do
    pluginZshsAbs <- mapM absPath pluginZshs
    repoDirectories <- mapM (absPath . findPluginDir) plugins
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
    mkdir_p reposDirectory
    mapM_ ensurePlugin plugins
    pluginZshs <- mapM findPluginZsh plugins
    contents <- fileToSourceContent config pluginZshs
    writefile outputFileToSource contents
