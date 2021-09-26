{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Text (Text, unpack)
import Lib.ContentType
import Test.Hspec
import Lib.Conversion

main :: IO ()
main = hspec $ do
  describe "getContentType" $ do
    it "recognizes docx" $ do
      getContentType "application/vnd.openxmlformats-officedocument.wordprocessingml.document" `shouldBe` (DocX :: ContentType Text)
    it "recognizes html" $ do
      getContentType "text/html" `shouldBe` (HTML :: ContentType Text)
    it "recognizes markdown" $ do
      getContentType "text/markdown" `shouldBe` (Markdown :: ContentType Text)
    it "wraps unknown types in the Unknown constructor" $ do
      let unknownContentType = "fake/type" :: Text
      getContentType unknownContentType `shouldBe` Unknown unknownContentType
  describe "Show instance returns the opposite of getContentType" $ do
    let unknownContentType = "fake/type" :: Text
    let t = map (\c -> (c, showContentType c)) ([HTML, Markdown, DocX, Unknown unknownContentType] :: [ContentType Text])
    forM_
      t
      ( \(c, shown) -> do
          it ("reverses " <> unpack shown) $ do
            getContentType shown `shouldBe` c
      )
  describe "convert" $ do
    it "returns MissingBoth when passed two Unknown ContentTypes" $ do
      convert (Unknown ("fake" :: Text)) (Unknown ("another/fake" :: Text)) "" `shouldBe` MissingBoth
    it "returns MissingReader when the first ContentType is Unknown" $ do
      convert (Unknown ("another/fake" :: Text)) HTML "" `shouldBe` MissingReader
    it "returns MissingWriter when the second ContentType is Unknown" $ do
      convert HTML (Unknown ("another/fake" :: Text)) "" `shouldBe` MissingWriter
    it "returns Failure when the input content is invalid" $ do
      conversionResultIsFailure (convert DocX HTML "not a real docx") `shouldBe` True
    it "returns Success with Markdown -> HTML conversion" $ do
      convert Markdown HTML "# Test" `shouldBe` Success "<h1>Test</h1>"
    it "returns Success with HTML -> Markdown conversion" $ do
      convert HTML Markdown "<h1>Test</h1>" `shouldBe` Success "# Test\n"
    -- TODO: DocX conversion tests. Should be basic, just to test that the mappings are correct
    -- Beyond that, assume pandoc does its job

conversionResultIsFailure :: ConversionResult -> Bool
conversionResultIsFailure (Failure _) = True
conversionResultIsFailure _ = False
