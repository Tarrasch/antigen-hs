{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Antigen where

import Prelude hiding (FilePath)
import Shelly
import qualified Data.Text as T
import Data.Text (Text)
import Data.Monoid
default (T.Text)

-- | Configuration that contains what plugins you use
data AntigenConfiguration = AntigenConfiguration
  { plugins :: ![ZshPlugin]
  }

-- | Data type representing a zsh plugin
data ZshPlugin = GitRepository
  { url :: !Text -- ^ Example: https://github.com/Tarrasch/zsh-functional
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


-- | Get the folder in which the plugin will be stored on disk
pluginPath :: ZshPlugin -> FilePath
pluginPath (GitRepository url) =
    reposDirectory </> toFilePath url
  where
    toFilePath s = fromText $ T.concatMap aux s
    aux ':' = "-COLON-"
    aux '/' = "-SLASH-"
    aux c   = T.singleton c


-- | Clone the repository if it already doesn't exist
ensurePlugin :: ZshPlugin -> Sh ()
ensurePlugin plugin = do
  needToClone <- fmap not $ test_d (pluginPath plugin)
  when needToClone $ clonePlugin plugin


-- | Clone the repository
clonePlugin :: ZshPlugin -> Sh ()
clonePlugin plugin@(GitRepository url) =
  cmd "git" "clone" "--recursive" "--" url (pluginPath plugin)


-- | (Used in my REPL development)
samplePlugin :: ZshPlugin
samplePlugin = bundle "Tarrasch/zsh-functional"


-- | (Used in my REPL development)
sampleConfig :: AntigenConfiguration
sampleConfig = AntigenConfiguration [samplePlugin]


-- | Match for *.plugin.zsh file
findPluginZsh :: ZshPlugin -> Sh FilePath
findPluginZsh plugin = do
  files <- ls (pluginPath plugin)
  case filter (endsWith ".plugin.zsh" . toTextIgnore) files of
    [file] -> return file
    [] -> terror "No *.plugin.zsh file!"
    xs -> terror "Too many *.plugin.zsh files!"


-- | endsWith ".txt" "hello.txt" ==> True
endsWith :: Text -> Text -> Bool
endsWith needle haystack =
  T.null $ snd $ T.breakOnEnd needle haystack


-- | Get the content that will be put in the file to source.
--
-- Since we need to aboslutify the FilePaths, this function is not pure.
fileToSourceContent ::
        AntigenConfiguration
     -> [FilePath] -- ^ List of all the *.plugin.zsh files
     -> Sh Text -- ^ What the file should contain
fileToSourceContent (AntigenConfiguration {..}) pluginZshs = do
    pluginZshsAbs <- mapM absPath pluginZshs
    repoDirectories <- mapM (absPath . pluginPath) plugins
    return $ T.unlines
      [ "# THIS FILE IS GENERATED BY antigen-hs!!!!\n"
      , T.unlines $ map (("source " <>) . toTextIgnore) pluginZshsAbs
      , T.unlines $ map (("fpath+=" <>) . toTextIgnore) repoDirectories
      ]


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
