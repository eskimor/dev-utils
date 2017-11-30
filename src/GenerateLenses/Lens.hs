{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GenerateLenses.Lens where

import           Control.Lens
import           Data.Monoid
import           Data.Text             (Text)
import qualified Data.Text             as T
import Data.Char
import Data.Text.Lens


import           GenerateLenses.Parser (Field, Record (..))
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

showLensSignature :: LensRep -> Text
showLensSignature LensRep {..} = _name <> " :: Lens' " <> _fullType <> " " <> _fieldType

showLens :: LensRep -> Text
showLens rep@LensRep {..} = T.unlines
    [ signature
    , body
    ]
  where
    signature = showLensSignature rep
    body = _name <> " f " <> _fullArgName <> " = (\\" <> _fieldArgName <> " -> " <> _fullArgName <> " { " <> _fieldName <> " = " <> _fieldArgName <> " }) <$> f (" <> _fieldName <> " " <> _fullArgName <> ")"


showLenses :: Record -> Text
showLenses record = T.unlines
  $ [ "-- Lenses for " <> _recordType record <> ":\n" ]
  <> map (showLens . fromField (_recordType record)) (_recordFields record)

showLensesClassy :: Record -> Text
showLensesClassy Record {..} = showClass <> showInstance
  where
    showClass :: Text
    showClass = T.unlines
      $ [ "class " <> className <> " a where"
        , "  " <> recordName <> " :: Lens' " <> instantiatedClassType <> " " <> addParens _recordType
        , ""
        ]
        <> map showLensClassy lensReps

    showInstance :: Text
    showInstance = T.unlines
      [ "instance " <> className <> " " <> typeConstrName <> " where"
      , "  " <> recordName <> " = id"
      ]

    className = "Has" <> T.words _recordType^._Cons._1
    classType = "a"
    instantiatedClassType = addParens $ replaceTypeConstructor _recordType
    replaceTypeConstructor = T.unwords . (_Cons._1 .~ classType) . T.words

    lensReps = map (fromField _recordType) _recordFields

    typeConstrName = (^._Cons._1) $ T.words _recordType
    recordName = (unpacked._Cons._1 %~ toLower) typeConstrName

    showLensClassy :: LensRep -> Text
    showLensClassy rep@LensRep {..} = T.unlines
        [ "  " <> showLensSignature (rep & fullType .~ instantiatedClassType)
        , "  " <> _name <> " = " <> recordName <> " . go"
        , "    where"
        , innerLens
        ]
      where
        innerLens = T.unlines . fmap ("      " <>) . T.lines $ showLens (rep & name .~ "go")


showRecords :: [Record] -> Text
showRecords = T.unlines . map showLenses

showRecordsClassy :: [Record] -> Text
showRecordsClassy = T.unlines . map showLensesClassy

-- Little helper function for adding parentheses where necessary.
addParens :: Text -> Text
addParens t = case T.find (== ' ') t of
                Nothing -> t
                Just _  -> "(" <> t <> ")"

class HasLensRep a where
  lensRep :: Lens' a LensRep

  name :: Lens' a Text
  name = lensRep . go
    where
      go :: Lens' LensRep Text
      go f lensRep' = (\name' -> lensRep' { _name = name' }) <$> f (_name lensRep')


  fullType :: Lens' a Text
  fullType = lensRep . go
    where
      go :: Lens' LensRep Text
      go f lensRep' = (\fullType' -> lensRep' { _fullType = fullType' }) <$> f (_fullType lensRep')


  fieldType :: Lens' a Text
  fieldType = lensRep . go
    where
      go :: Lens' LensRep Text
      go f lensRep' = (\fieldType' -> lensRep' { _fieldType = fieldType' }) <$> f (_fieldType lensRep')


  fieldName :: Lens' a Text
  fieldName = lensRep . go
    where
      go :: Lens' LensRep Text
      go f lensRep' = (\fieldName' -> lensRep' { _fieldName = fieldName' }) <$> f (_fieldName lensRep')


  fieldArgName :: Lens' a Text
  fieldArgName = lensRep . go
    where
      go :: Lens' LensRep Text
      go f lensRep' = (\fieldArgName' -> lensRep' { _fieldArgName = fieldArgName' }) <$> f (_fieldArgName lensRep')


  fullArgName :: Lens' a Text
  fullArgName = lensRep . go
    where
      go :: Lens' LensRep Text
      go f lensRep' = (\fullArgName' -> lensRep' { _fullArgName = fullArgName' }) <$> f (_fullArgName lensRep')


instance HasLensRep LensRep where
  lensRep = id

