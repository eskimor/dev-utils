{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module GenerateModule.Internal where

import           Control.Lens
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc
import Data.Time (toGregorian, getCurrentTime, UTCTime(..))

import           DevUtils.ImportLine       (ImportLine)
import qualified DevUtils.ImportLine       as ImportLine



data Module
  = Module { _fullName :: !(Doc Text)
           , _internalName :: !(Doc Text)
           , _name :: !(Doc Text)
           , _copyrightNotice :: !(Doc Text)
           , _basePath        :: !FilePath
           }



fileName :: Module -> Doc Text
fileName m = m^.name <> ".hs"


buildCopyrightNotice :: IO (Doc Text)
buildCopyrightNotice = do
  day <- utctDay <$> getCurrentTime -- Is utc, but that's good enough for us.
  let (year, _, _) = toGregorian day
  pure $ "(c) Robert Klotzner," <+> pretty year


-- Lenses for Module:

fullName :: Lens' Module ( (Doc Text))
fullName f module' = (\fullName' -> module' { _fullName = fullName' }) <$> f (_fullName module')

internalName :: Lens' Module ( (Doc Text))
internalName f module' = (\internalName' -> module' { _internalName = internalName' }) <$> f (_internalName module')

name :: Lens' Module ( (Doc Text))
name f module' = (\name' -> module' { _name = name' }) <$> f (_name module')

copyrightNotice :: Lens' Module ( (Doc Text))
copyrightNotice f module' = (\copyrightNotice' -> module' { _copyrightNotice = copyrightNotice' }) <$> f (_copyrightNotice module')

basePath :: Lens' Module FilePath
basePath f module' = (\basePath' -> module' { _basePath = basePath' }) <$> f (_basePath module')


