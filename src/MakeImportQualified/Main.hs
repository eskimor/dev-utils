#!/usr/bin/env nix-shell
#! nix-shell -i runghc --pure -p 'pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ text lens safe ])'

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MakeImportQualified.Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Functor
import Data.Char
import System.IO
import Data.Monoid
import Control.Lens
import Safe

import System.Exit

import DevUtils.ImportLine as ImportLine


main :: IO ()
main = do
  modules <- T.lines <$> T.getContents
  let imports = map ImportLine.toQualified $ modules
  T.putStrLn $ T.unlines imports

