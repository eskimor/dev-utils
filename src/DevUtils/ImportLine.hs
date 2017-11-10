{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module DevUtils.ImportLine (ImportLine(..)
                           -- * Creation
                           , parseSloppy
                           , fromFilePath
                           -- * Printing
                           , printQualified
                           , toQualified
                            -- * Lenses
                           , moduleFullName
                           , moduleName
                           , moduleAlias
                           ) where

import           Control.Lens
import           Data.Char       (isUpper)
import           Data.List       (intercalate)
import           Data.Maybe
import           Data.Monoid
import           Data.Text       (Text)
import qualified Data.Text       as T
import           System.FilePath (dropExtension, splitDirectories)


-- | Module being imported:
data ImportLine
  = ImportLine { _moduleFullName :: !Text
               , _moduleName     :: !Text
               , _moduleAlias    :: !Text
               } deriving (Eq, Show)


-- | Sloppy import line parser (import and qualified keywords can be ommited)
--
--   Accepted formats:
--
--   >>> :set -XOverloadedStrings
--   >>> parseSloppy "Data.Text"
--   ImportLine {_moduleFullName = "Data.Text", _moduleName = "Text", _moduleAlias = "Text"}
--
--   >>> parseSloppy "Data.Text as T"
--   ImportLine {_moduleFullName = "Data.Text", _moduleName = "Text", _moduleAlias = "T"}
--
--   >>> parseSloppy "Data.Text T"
--   ImportLine {_moduleFullName = "Data.Text", _moduleName = "Text", _moduleAlias = "T"}
--
--   >>> parseSloppy "import Data.Text as T"
--   ImportLine {_moduleFullName = "Data.Text", _moduleName = "Text", _moduleAlias = "T"}
--
--   >>> parseSloppy "import qualified Data.Text as T"
--   ImportLine {_moduleFullName = "Data.Text", _moduleName = "Text", _moduleAlias = "T"}
parseSloppy :: Text -> ImportLine
parseSloppy l =
  let
    (_moduleFullName, _moduleAlias') = fmap T.strip . T.breakOn " " . normalize $ l
    _moduleAlias = if T.null _moduleAlias' then _moduleName else _moduleAlias'
    -- head is safe as splitOn should never return an empty list:
    _moduleName = head . reverse . T.splitOn "." $ _moduleFullName
  in
    ImportLine {..}
  where
    normalize :: Text -> Text
    normalize = dropAs . dropQualified . dropImport

    dropImport :: Text -> Text
    dropImport = stripPrefix "import "

    dropQualified :: Text -> Text
    dropQualified = stripPrefix "qualified "

    dropAs :: Text -> Text
    dropAs = T.unwords . filter (/= "as") . T.words

    stripPrefix :: Text -> Text -> Text
    stripPrefix prefix str = fromMaybe str $ T.stripPrefix prefix str


-- | Build ImportLine/module name from file path
--
--   It simply assumes that the last path pieces that start with an uppercase
--   letter are package names / the module name.
--
--   Example:
--
--   >>> import qualified Data.Text.IO as T
--   >>> T.putStr $ printQualified $ fromFilePath "/home/eskimor/projects/dev-utils/src/DevUtils/ImportLine.hs"
--   import           DevUtils.ImportLine (ImportLine)
--   import qualified DevUtils.ImportLine as ImportLine
fromFilePath :: FilePath -> ImportLine
fromFilePath path
  = let
      pieces = splitDirectories . dropExtension $ path
      modulePieces = reverse . takeWhile isCapitalized . reverse $ pieces
      moduleName = T.pack $ intercalate "." modulePieces
    in
      parseSloppy moduleName
  where
    -- Should be safe on the result of splitPath:
    isCapitalized = isUpper . head

-- | Render as typical qualified import pattern. For Data.Text this would be:
--
--   >>> import qualified Data.Text.IO as T
--   >>> T.putStr $ printQualified $ ImportLine { _moduleFullName = "Data.Text", _moduleName = "Text", _moduleAlias = "T"}
--   import           Data.Text (Text)
--   import qualified Data.Text as T
printQualified :: ImportLine -> Text
printQualified ImportLine{..}
  = T.unlines $ [ "import           " <> _moduleFullName <> " (" <> _moduleName <> ")"
                , "import qualified " <> _moduleFullName <> " as " <> _moduleAlias
                ]

-- | Convenience function: printQualified . parseSloppy
--
--   >>> import qualified Data.Text.IO as T
--   >>> T.putStr $ toQualified "Data.Text"
--   import           Data.Text (Text)
--   import qualified Data.Text as Text
toQualified :: Text -> Text
toQualified = printQualified . parseSloppy

-- Lenses for ImportLine:

moduleFullName :: Lens' ImportLine Text
moduleFullName f importLine' = (\moduleFullName' -> importLine' { _moduleFullName = moduleFullName' }) <$> f (_moduleFullName importLine')

moduleName :: Lens' ImportLine Text
moduleName f importLine' = (\moduleName' -> importLine' { _moduleName = moduleName' }) <$> f (_moduleName importLine')

moduleAlias :: Lens' ImportLine Text
moduleAlias f importLine' = (\moduleAlias' -> importLine' { _moduleAlias = moduleAlias' }) <$> f (_moduleAlias importLine')


