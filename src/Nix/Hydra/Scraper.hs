{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Nix.Hydra.Scraper where

import Control.Applicative ((<|>))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT (..))
import Data.Aeson (Options (..), ToJSON (..), defaultOptions, encode)
import Data.Aeson.Types (genericToEncoding)
import Data.ByteString.Lazy (ByteString)
import Data.Char (toLower)
import Data.List (stripPrefix)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import GHC.Generics (Generic)
import Numeric.Natural (Natural)
import Text.HTML.Scalpel
import Text.Read (readMaybe)
import Text.Regex.Posix ((=~))

-- Types -------------------------------------------------------------------------------------------

-- | Type alias used to make function signatures more readable.
type EvalId = Natural

-- | Type alias used to make function signatures more readable.
type BuildId = Natural

-- | Type alias used to make function signatures more readable.
type BuildStatus = String

-- | Type alias used to make function signatures more readable.
type UnixTime = Natural

-- | Information scraped from a Hydra jobset evaluation page.
data Build = Build
  { -- | The status of the build. Can be one of:
    --
    --    - Dependency failed
    --    - Timed out
    --    - Failed
    --    - Succeeded
    --    - Queued
    --    - Cancelled
    --    - ...
    buildStatus :: BuildStatus,
    -- | The ID of the build, e.g., @196801393@.
    buildId :: BuildId,
    -- | The name of the build job, e.g., @haskellPackages.gnome-keyring.aarch64-linux@.
    buildJob :: String,
    -- | The time build stopped. If `Nothing`, the build is still queued.
    buildStoptime :: Maybe UnixTime,
    -- | The name of the package, e.g., @gnome-keyring-0.3.1.1@.
    buildNixname :: String,
    -- | The system of the package, e.g., @aarch64-linux@.
    buildSystem :: String
  }
  deriving (Eq, Show, Generic)

instance ToJSON Build where
  toEncoding =
    genericToEncoding $
      defaultOptions
        { fieldLabelModifier = fromJust . stripPrefix "build" . fmap toLower
        }

-- | Type alias used to make function signatures more readable.
--
-- Collection of builds.
type Builds = Map BuildId Build

-- | Type alias used to make function signatures more readable.
--
-- The `BuildId` is the ID of a failed build and [`Build`] are the builds that depended on that
-- build.
type ProblemDeps = Map BuildId [Build]

-- | Information for a Hydra jobset evaluation.
data EvalInfo = EvalReport
  { -- | The ID of the jobset evaluation, e.g., @1784238@.
    evalId :: EvalId,
    -- | The builds jobs in for the jobset evaluation.
    evalBuilds :: Builds,
    -- | Problematic dependencies, i.e., builds that other builds depend on that failed.
    evalProblemdeps :: ProblemDeps
  }
  deriving (Eq, Show, Generic)

instance ToJSON EvalInfo where
  toEncoding =
    genericToEncoding $
      defaultOptions
        { fieldLabelModifier = fromJust . stripPrefix "eval" . fmap toLower
        }

-- | Scrapes a Hydra jobset evaluation page (e.g., <https://hydra.nixos.org/eval/1784238>) and
-- extracts information about the builds that makeup the evaluation.
buildsForEval :: EvalId -> MaybeT IO Builds
buildsForEval eid =
  MaybeT $ scrapeURL ("https://hydra.nixos.org/eval/" <> show eid <> "?full=1") buildsScraper
  where
    buildsScraper :: Scraper String Builds
    buildsScraper =
      fmap M.fromList $
        chroots ("div" // "tbody" // "tr") $
          (\b -> (buildId b, b)) <$> inSerial buildScraper

    buildScraper :: SerialScraper String Build
    buildScraper =
      Build
        <$> seekNext (attr "title" ("td" // "img"))
        <*> (read <$> seekNext (text "td"))
        <*> seekNext (text "td")
        <*> seekNext timeScraper
        <*> seekNext (text "td")
        <*> seekNext (text "td")

    timeScraper :: Scraper String (Maybe UnixTime)
    timeScraper = readMaybe <$> (attr "data-timestamp" ("td" // "time") <|> text "td")

-- | Scrapes the Hydra build page (e.g., <https://hydra.nixos.org/build/196800181>) and, if the
-- build failed due to one of it's dependencies failing, extracts the build ID for the dependency.
--
-- Specifically it extracts the build ID present in the "Status" column in the first row of the
-- "Failed build steps" table. For example for build @196800181@, it would extract @196835188@ from:
--
-- @
--   Cached failure (log, raw, tail) (propagated from build 196835188)
-- @
--
-- Sometimes the "(propagated from build 196835188)" portion isn't present when the build failed due
-- to a dependency failing. In those cased the function returns `Nothing`.
failedDepIdForBuild :: BuildId -> MaybeT IO (Maybe BuildId)
failedDepIdForBuild bid =
  MaybeT $ scrapeURL ("https://hydra.nixos.org/build/" <> show bid) depIdScraper
  where
    depIdScraper :: Scraper String (Maybe BuildId)
    depIdScraper =
      readMaybe . (=~ ("[[:digit:]]+" :: String)) <$> text ("td" @: [hasClass "step-status"])

-- | Filters a collection of builds for builds with a given status.
buildsWithStatus :: BuildStatus -> Builds -> Builds
buildsWithStatus s = M.filter ((== s) . buildStatus)

-- | Gives information on the problematic builds in a collection of builds.
problemDeps :: Builds -> MaybeT IO ProblemDeps
problemDeps bs = foldr go (lift mempty) buildsWithFailedDeps
  where
    go :: Build -> MaybeT IO ProblemDeps -> MaybeT IO ProblemDeps
    go b pds =
      failedDepIdForBuild (buildId b) >>= \case
        Just bid -> M.insertWith (<>) bid [b] <$> pds
        Nothing -> pds

    buildsWithFailedDeps :: Builds
    buildsWithFailedDeps = buildsWithStatus "Dependency failed" bs

-- | Gives information for a Hydra jobset evaluation.
evalInfo :: EvalId -> MaybeT IO EvalInfo
evalInfo eid = do
  bs <- buildsForEval eid
  EvalReport eid bs <$> problemDeps bs

-- | Runs `evalInfo` and converts the output to JSON.
evalInfoJson :: EvalId -> IO (Maybe ByteString)
evalInfoJson eid = runMaybeT $ encode <$> evalInfo eid
