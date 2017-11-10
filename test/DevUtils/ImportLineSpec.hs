{-# Language OverloadedStrings #-}
module DevUtils.ImportLineSpec where

import Test.Hspec


import DevUtils.ImportLine


spec :: Spec
spec = do
  describe "parseSloppy" $ do
    it "fully ignores the import keyword" $ do
      parseSloppy "import Data.Text T" == parseSloppy "Data.Text T"
    it "ignores the qualified keyword too" $ do
      parseSloppy "import qualified Data.Text T" == parseSloppy "Data.Text T"
      && parseSloppy "qualified Data.Text T" == parseSloppy "Data.Text T"
    it "ignores the as keyword" $ do
      parseSloppy "import qualified Data.Text as T" == parseSloppy "Data.Text T"
      && parseSloppy "Data.Text as T" == parseSloppy "Data.Text T"
    it "does not crash on empty input" $ do
      parseSloppy "" `shouldBe` ImportLine "" "" ""
  describe "fromFilePath" $ do
    it "does not crash on empty input" $ do
      fromFilePath "" `shouldBe` ImportLine "" "" ""
