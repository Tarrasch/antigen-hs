{-# LANGUAGE CPP               #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
module Antigen
    ( antigen
    , AntigenConfig(..)
    , defaultConfig

    , ZshPlugin(..)
    , RepoStorage(..)
    , SourcingStrategy
    , defaultZshPlugin

    , bundle
    , local

    , strictSourcingStrategy
    , antigenSourcingStrategy
    , filePathsSourcingStrategy
    ) where


#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif

import Control.Exception  (bracket_)
import Control.Monad
import Data.List          (isSuffixOf)
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Data.Text          (Text)
import System.Directory   (createDirectoryIfMissing, doesDirectoryExist,
                           getCurrentDirectory, getDirectoryContents,
                           getHomeDirectory, setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit        (exitFailure)
import System.FilePath    (isRelative, normalise, (</>))

import qualified Data.Text      as Text
import qualified Data.Text.IO   as Text
import qualified System.Process as Proc

-- | Configuration of Antigen.
data AntigenConfig = AntigenConfig
    { plugins         :: ![ZshPlugin]
    -- ^ List of plugins to install.
    , outputDirectory :: FilePath
    -- ^ Directory to which all generated files will be written.
    --
    -- This may be overridden with the @ANTIGEN_HS_OUT@ environment variable.
    }

-- | Get a default AntigenConfig.
defaultConfig :: AntigenConfig
defaultConfig = AntigenConfig
    { plugins = []
    , outputDirectory = ".antigen-hs"
    }

-- | Data type representing a zsh plugin
data ZshPlugin = ZshPlugin
    { storage           :: RepoStorage
    -- ^ Where can this plugin be found?
    --
    -- If this is a git repository, it will automatically be checked out in a
    -- system-determined location.
    , sourcingStrategy  :: SourcingStrategy
    -- ^ How to determine which scripts to source and in what order?
    , sourcingLocations :: [FilePath]
    -- ^ List of paths relative to the plugin root in which @sourcingStrategy@
    -- will be executed.
    , fpathLocations    :: [FilePath]
    -- ^ Paths relative to plugin root which will be added to @fpath@.
    }

-- | The SourcingStrategy is executed inside the plugin's @sourcingLocations@,
-- and the paths returned by it are sourced in-order.
type SourcingStrategy = IO [FilePath]

-- | A location where the plugin may be found.
data RepoStorage
    = -- | The plugin is available in a GitHub repository.
      --
      -- A local copy will be cloned.
      GitRepository
        { url :: !Text
        -- ^ Example: https://github.com/Tarrasch/zsh-functional
        }
    | -- | The plugin is available locally.
      Local
        { filePath :: !FilePath
        -- ^ See 'local'
        }
  deriving (Show, Eq)


-- | Directory where the repositories will be stored.
reposDirectory :: AntigenConfig -> FilePath
reposDirectory config = outputDirectory config </> "repos"


-- | The final output script.
--
-- This is automatically sourced by init.zsh.
outputFileToSource :: AntigenConfig -> FilePath
outputFileToSource config = outputDirectory config </> "antigen-hs.zsh"


-- | A default ZshPlugin
--
-- The default plugin uses the 'strictSourcingStrategy' inside the plugin
-- root, and adds the plugin root to @fpaths@.
defaultZshPlugin :: ZshPlugin
defaultZshPlugin = ZshPlugin
    { storage = error "Please specify a plugin storage."
    , sourcingStrategy = strictSourcingStrategy
    , sourcingLocations = ["."]
    , fpathLocations = [""]
    }

-- | Like @antigen bundle@ from antigen. It assumes you want a GitHub
-- repository.
bundle :: Text -> ZshPlugin
bundle repo = defaultZshPlugin
    { storage = GitRepository $ "https://github.com/" <> repo
    }


-- | A local repository, useful when testing plugins
--
-- > local "/home/arash/repos/zsh-snakebite-completion"
local :: FilePath -> ZshPlugin
local filePath = defaultZshPlugin
    { storage = Local filePath
    } -- TODO should resolve path to absolute


-- | Get the folder in which the storage will be stored on disk
storagePath :: AntigenConfig -> RepoStorage -> FilePath
storagePath config storage = case storage of
    GitRepository repo ->
      reposDirectory config </> Text.unpack (santitize repo)
    Local path -> path
  where
    santitize = Text.concatMap aux
    aux ':' = "-COLON-"
    aux '/' = "-SLASH-"
    aux c   = Text.singleton c


-- | Clone the repository if it already doesn't exist.
ensurePlugin :: AntigenConfig -> RepoStorage -> IO ()
ensurePlugin _ (Local path) = do
    exists <- doesDirectoryExist path
    unless exists $
        die $ "Local plugin " ++ show path ++ " does not exist. " ++
              "Make sure that the path is absolute."
ensurePlugin config storage@(GitRepository url) = do
    exists <- doesDirectoryExist path
    unless exists $
        gitClone url path
  where
    path = storagePath config storage

-- TODO URL should be ByteString?

gitClone :: Text -> FilePath -> IO ()
gitClone url path =
    Proc.callProcess "git"
      ["clone", "--recursive", "--", Text.unpack url, path]


-- | Convert a relative path to absolute by prepending the current directory.
--
-- This is available in directory >= 1.2.3, but GHC 7.8 uses 1.2.1.0.
makeAbsolute :: FilePath -> IO FilePath
makeAbsolute = (normalise <$>) . absolutize
  where
    absolutize path
        | isRelative path = (</> path) <$> getCurrentDirectory
        | otherwise = return path

withDirectory :: FilePath -> IO a -> IO a
withDirectory p io = do
    old <- getCurrentDirectory
    bracket_ (setCurrentDirectory p)
             (setCurrentDirectory old)
             io

-- | Gather scripts to source for each sourcing location of the plugin.
findPluginZshs :: AntigenConfig -> ZshPlugin -> IO [FilePath]
findPluginZshs config plugin =
    withDirectory (storagePath config (storage plugin)) $
    fmap concat . forM (sourcingLocations plugin) $ \loc ->
        withDirectory loc $
            sourcingStrategy plugin


-- | Get full paths to all files in the given directory.
listFiles :: FilePath -> IO [FilePath]
listFiles p
    = map (p </>)
    . filter (`notElem` [".", ".."])
  <$> getDirectoryContents p


die :: String -> IO a
die msg = putStrLn msg >> exitFailure


-- | Match for one single *.plugin.zsh file
strictSourcingStrategy :: SourcingStrategy
strictSourcingStrategy = do
    directory <- getCurrentDirectory
    files <- listFiles directory
    let matches = filter (".plugin.zsh" `isSuffixOf`) files
    case matches of
        [file] -> return [file]
        [] -> die $
            "No *.plugin.zsh file in " ++
            directory ++ "! " ++
            "See antigenSourcingStrategy example in README " ++
            "on how to configure this."
        _  -> die ("Too many *.plugin.zsh files in " ++ directory ++ "!")


-- | Find what to source, using the strategy described here:
--
-- https://github.com/zsh-users/antigen#notes-on-writing-plugins
antigenSourcingStrategy :: SourcingStrategy
antigenSourcingStrategy = do
    files <- getCurrentDirectory >>= listFiles
    let candidatePatterns = [".plugin.zsh", "init.zsh", ".zsh", ".sh"]
        filesMatching pat = filter (pat `isSuffixOf`) files
        filteredResults =  map filesMatching candidatePatterns
        results = filter (not . null) filteredResults
    case results of
        (matchedFiles:_) -> return matchedFiles
        [] -> die $ "No files to source among " ++ show files


-- | Source all files in the given order. Currently does no file existence
-- check or anything.
filePathsSourcingStrategy :: [FilePath] -> SourcingStrategy
filePathsSourcingStrategy paths = do
  cwd <- getCurrentDirectory
  return $ map (cwd </>) paths


getAbsoluteFpaths :: AntigenConfig -> ZshPlugin -> IO [FilePath]
getAbsoluteFpaths config plugin = do
    let path = storagePath config (storage plugin)
    mapM (makeAbsolute . (path </>)) (fpathLocations plugin)

-- | Get the content that will be put in the file to source.
fileToSourceContent
    :: AntigenConfig
    -> [FilePath] -- ^ List of all the *.plugin.zsh files
    -> IO Text    -- ^ What the file should contain
fileToSourceContent config@AntigenConfig{plugins} pluginZshs = do
    pluginZshsAbs <- mapM makeAbsolute pluginZshs
    fpaths <- concat <$> mapM (getAbsoluteFpaths config) plugins
    return $ Text.unlines
      $ "# THIS FILE IS GENERATED BY antigen-hs!!!!\n"
      : map (("source " <>) . Text.pack) pluginZshsAbs
     ++ map (("fpath+=" <>) . Text.pack) fpaths


-- | Do an action inside the home directory
inHomeDir :: IO a -> IO a
inHomeDir io = do
    home <- getHomeDirectory
    withDirectory home io


-- | The main function that will clone all the repositories and create the
-- file to be sourced by the user
antigen :: AntigenConfig -> IO ()
antigen config'@AntigenConfig{plugins} = inHomeDir $ do
    hsOut <- lookupEnv "ANTIGEN_HS_OUT"
    let config = config'
            { outputDirectory = fromMaybe (outputDirectory config') hsOut }

    createDirectoryIfMissing True (reposDirectory config)
    mapM_ (ensurePlugin config . storage) plugins
    pluginZshs <- concat <$> mapM (findPluginZshs config) plugins
    contents <- fileToSourceContent config pluginZshs
    Text.writeFile (outputFileToSource config) contents
