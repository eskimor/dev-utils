{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module GenerateLenses.Main where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec
import Text.Megaparsec.Char (spaceChar, space1, char, anyChar, string, notChar, satisfy)
import Data.Functor
import Data.Char
import System.IO
import Data.Monoid
import Control.Lens

import GenerateLenses.Parser
import GenerateLenses.Lens


example :: Text
example = "data Test i a b = Test { a -- Very cool a!\n:: Maybe Int\n, b :: Eiter a b,\n hugo :: (Dynamic t (Maybe a)) -- Cool shit again!\n}"


main :: IO ()
main = do
  definitions <- T.getContents
  let r = parse parseRecords "data Definitions" definitions
  case r of
    Left e -> hPutStrLn stderr $ show e
    Right records -> T.putStr $ showRecords records
