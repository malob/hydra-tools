{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BS
import Nix.Hydra.Scraper (evalInfoJson)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= evalInfoJson . read . head >>= BS.putStr . fromMaybe "Error"
