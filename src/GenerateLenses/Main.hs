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

data Error = UnknownError deriving (Show, Ord, Eq)

instance ShowErrorComponent Error where
  showErrorComponent = show

type Parser m = MonadParsec Error Text m

data Record
  = Record { _recordType :: Text
           , _recordFields :: [Field]
           } deriving Show

data Field
  = Field { _fieldName :: Text
          , _fieldType :: Text
          } deriving Show

spaceConsumer :: Parser m => m ()
spaceConsumer = L.space space1 (L.skipLineComment "--") skipBlockComment


skipBlockComment :: Parser m => m ()
skipBlockComment = L.skipBlockComment "{--" "--}"

lexeme :: Parser m => m a -> m a
lexeme = L.lexeme spaceConsumer

symbol :: Parser m => Text -> m Text
symbol = L.symbol spaceConsumer

dataConsumer :: Parser m => m ()
dataConsumer = void $ symbol "data"

parseIdentifier :: Parser m => m Text
parseIdentifier = do
  void $ try (char '!') -- Drop strictness
  try parseTuple <|> (lexeme $ takeWhile1P (Just "No valid identifier") isValid)
  where
    isValid c = isAlphaNum c || c == '_' || c == '[' || c == ']' || c == '.' || c == '-' || c == '>'

parseSplitIdentifier :: Parser m => m Text
parseSplitIdentifier = T.unwords <$> some parseIdentifier

brackets :: Parser m => m a -> m a
brackets = between (symbol "{") (symbol "}")

braces :: Parser m => m a -> m a
braces = between (symbol "(") (symbol ")")

equals :: Parser m => m ()
equals = void $ symbol "="

doubleColon :: Parser m => m ()
doubleColon = void $ symbol "::"

comma :: Parser m => m ()
comma = void $ symbol ","

parseLHS :: Parser m => m Text
parseLHS = parseSplitIdentifier


dropUntilData :: Parser m => m ()
dropUntilData = void $ manyTill anyChar (try dataConsumer)



dropConstructorName :: Parser m => m ()
dropConstructorName = void $ parseIdentifier

parseFieldName :: Parser m => m Text
parseFieldName = parseIdentifier

parseFieldType :: Parser m => m Text
parseFieldType = parseSplitIdentifier

parseTuple :: Parser m => m Text
parseTuple = ((\inner -> "(" <> inner <> ")") . T.intercalate ", ")
             <$> braces (parseSplitIdentifier `sepBy` comma)


parseField :: Parser m => m Field
parseField = do
  _fieldName <- parseFieldName
  doubleColon
  _fieldType <- parseFieldType
  pure $ Field {..}

parseFields :: Parser m => m [Field]
parseFields = parseField `sepBy` comma

parseRecord :: Parser m => m Record
parseRecord = do
  dropUntilData
  _recordType <- parseLHS
  equals
  dropConstructorName
  _recordFields <- brackets parseFields
  pure $ Record {..}

parseRecords :: Parser m => m [Record]
parseRecords = some $ try parseRecord

example :: Text
example = "data Test i a b = Test { a -- Very cool a!\n:: Maybe Int\n, b :: Eiter a b,\n hugo :: (Dynamic t (Maybe a)) -- Cool shit again!\n}"

showLens :: Text -> Field -> Text
showLens typeName field = T.unlines
    [ signature
    , body
    ]
  where
    addParens t = case T.find (== ' ') t of
                    Nothing -> t
                    Just _ -> "(" <> t <> ")"

    fieldTypeText = addParens $ _fieldType field

    typeNameText = addParens typeName

    signature = lensName <> " :: Lens' " <> typeNameText <> " " <> fieldTypeText

    lensName = if "_" `T.isPrefixOf`fieldNameText
               then
                 T.drop 1 fieldNameText
               else
                 fieldNameText

    fieldNameText = _fieldName field

    objName = (\(h, rest) -> T.toLower h <> rest <> "'") . T.splitAt 1 . T.takeWhile (/= ' ') $ typeName

    argName = lensName <> "'"

    body = lensName <> " f " <> objName <> " = (\\" <> argName <> " -> " <> objName <> " { " <> fieldNameText <> " = " <> argName <> " }) <$> f (" <> fieldNameText <> " " <> objName <> ")"

showLenses :: Record -> Text
showLenses record = T.unlines
  $ [ "-- Lenses for " <> _recordType record <> ":\n" ]
  <> map (showLens (_recordType record)) (_recordFields record)


showRecords :: [Record] -> Text
showRecords = T.unlines . map showLenses

test :: Text -> IO ()
test input = parseTest parseRecord input

main :: IO ()
main = do
  definitions <- T.getContents
  let r = parse parseRecords "data Definitions" definitions
  case r of
    Left e -> hPutStrLn stderr $ show e
    Right records -> T.putStr $ showRecords records

-- Lenses for Record:
recordType :: Lens' Record Text
recordType f record' = (\recordType' -> record' { _recordType = recordType' }) <$> f (_recordType record')

recordFields :: Lens' Record [Field]
recordFields f record' = (\recordFields' -> record' { _recordFields = recordFields' }) <$> f (_recordFields record')


-- Lenses for Field:
fieldName :: Lens' Field Text
fieldName f field' = (\fieldName' -> field' { _fieldName = fieldName' }) <$> f (_fieldName field')

fieldType :: Lens' Field Text
fieldType f field' = (\fieldType' -> field' { _fieldType = fieldType' }) <$> f (_fieldType field')
