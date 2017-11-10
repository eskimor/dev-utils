{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GenerateModule.Main where

import           Control.Lens
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import           Data.Text.Prettyprint.Doc
import           Data.Text.Prettyprint.Doc.Render.Text
import           Data.Time                             (UTCTime (..),
                                                        getCurrentTime,
                                                        toGregorian)
import           Safe                                  (headMay)
import           System.Directory                      (getCurrentDirectory, createDirectory)
import           System.Environment                    (getArgs)
import           System.IO                             (withFile, IOMode(..))
import           System.FilePath ((</>))



import           DevUtils.ImportLine                   (ImportLine)
import qualified DevUtils.ImportLine                   as ImportLine
import           GenerateModule.Internal
import           GenerateModule.Templates


makeModule :: IO Module
makeModule = do
  _basePath <- getCurrentDirectory
  args <- getArgs
  let name = case headMay args of
        Nothing -> error "Usage: dev-GenerateModule <moduleName>"
        Just n  -> n
  let fullPath = _basePath </> name <> ".hs"
  let importLine = ImportLine.fromFilePath fullPath
  _copyrightNotice <- buildCopyrightNotice

  let
    _fullName = pretty $ importLine^.ImportLine.moduleFullName
    _name = pretty $ importLine^.ImportLine.moduleName
    _internalName = _fullName <> ".Internal"
  pure $ Module {..}

writeModule :: Module -> IO ()
writeModule m@(Module{..}) = do
  let modulePath = _basePath </> show (fileName m)
  let internalPath = _basePath </> show _name </> "Internal.hs"
  createDirectory $ _basePath </> show _name
  withFile modulePath WriteMode (`hPutDoc` exposedContent m)
  withFile internalPath WriteMode (`hPutDoc` internalContent m)

main :: IO ()
main = makeModule >>= writeModule
