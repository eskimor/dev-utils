{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GenerateLenses.Lens where

import           Control.Lens
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as T

import GenerateLenses.Parser (Field, Record(..))
import qualified GenerateLenses.Parser as P

-- | Lens representation suitable for easily printing it.
--
--   A non classy lens looks something like this (pseudo code):
--
--   @@@
--      _name :: Lens' _fullType _fieldType
--      _name f _fullArgName = (\_fieldArgName -> _fullArgName { _fieldName = _fieldArgName }) <$> f (_fieldName _fullArgName)
--   @@@
data LensRep
  = LensRep { _name         :: !Text
            , _fullType     :: !Text
            , _fieldType    :: !Text
            , _fieldName    :: !Text
            , _fieldArgName :: !Text
            , _fullArgName  :: !Text
            }

fromField :: Text -> Field -> LensRep
fromField typeName field
  = LensRep { .. }
  where
    addParens t = case T.find (== ' ') t of
                    Nothing -> t
                    Just _  -> "(" <> t <> ")"

    _fieldType = addParens $ P._fieldType field

    _fullType = addParens typeName

    _name = if "_" `T.isPrefixOf` _fieldName
                   then
                     T.drop 1 _fieldName
                   else
                     _fieldName

    _fieldName = P._fieldName field

    _fullArgName = (\(h, rest) -> T.toLower h <> rest <> "'") . T.splitAt 1 . T.takeWhile (/= ' ') $ typeName

    _fieldArgName = _name <> "'"

showLens :: LensRep -> Text
showLens LensRep {..} = T.unlines
    [ signature
    , body
    ]
  where
    signature = _name <> " :: Lens' " <> _fullType <> " " <> _fieldType

    body = _name <> " f " <> _fullArgName <> " = (\\" <> _fieldArgName <> " -> " <> _fullArgName <> " { " <> _fieldName <> " = " <> _fieldArgName <> " }) <$> f (" <> _fieldName <> " " <> _fullArgName <> ")"

showLenses :: Record -> Text
showLenses record = T.unlines
  $ [ "-- Lenses for " <> _recordType record <> ":\n" ]
  <> map (showLens . fromField (_recordType record)) (_recordFields record)


showRecords :: [Record] -> Text
showRecords = T.unlines . map showLenses

-- Lenses for LensRep:

name :: Lens' LensRep Text
name f lensRep' = (\name' -> lensRep' { _name = name' }) <$> f (_name lensRep')

fullType :: Lens' LensRep Text
fullType f lensRep' = (\fullType' -> lensRep' { _fullType = fullType' }) <$> f (_fullType lensRep')

fieldType :: Lens' LensRep Text
fieldType f lensRep' = (\fieldType' -> lensRep' { _fieldType = fieldType' }) <$> f (_fieldType lensRep')

fieldName :: Lens' LensRep Text
fieldName f lensRep' = (\fieldName' -> lensRep' { _fieldName = fieldName' }) <$> f (_fieldName lensRep')

fieldArgName :: Lens' LensRep Text
fieldArgName f lensRep' = (\fieldArgName' -> lensRep' { _fieldArgName = fieldArgName' }) <$> f (_fieldArgName lensRep')

fullArgName :: Lens' LensRep Text
fullArgName f lensRep' = (\fullArgName' -> lensRep' { _fullArgName = fullArgName' }) <$> f (_fullArgName lensRep')


