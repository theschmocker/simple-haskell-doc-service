{-# LANGUAGE OverloadedStrings #-}

import Control.Monad (forM_)
import Data.Text (Text, unpack)
import Lib.ContentType
import Lib.Conversion
import Test.Hspec

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
    let fakeType1 = Unknown ("fake" :: Text)
    let fakeType2 = Unknown ("another/fake" :: Text)
    it "returns both types when passed two Unknown ContentTypes" $ do
      case convert fakeType1 fakeType2 "" of
        Right _ -> expectationFailure "Should not be able to convert unknown types"
        Left err -> case err of
          InternalError _ -> expectationFailure "Should never attempt to convert unknown types"
          UnknownContentTypes types -> types `shouldMatchList` [fakeType1, fakeType2]
    it "returns first type when passed Unknown from content type" $ do
      case convert fakeType1 HTML "" of
        Right _ -> expectationFailure "Should not be able to convert unknown types"
        Left err -> case err of
          InternalError _ -> expectationFailure "Should never attempt to convert unknown types"
          UnknownContentTypes types -> types `shouldMatchList` [fakeType1]
    it "returns second type when passed Unknown to content type" $ do
      case convert HTML fakeType2 "" of
        Right _ -> expectationFailure "Should not be able to convert unknown types"
        Left err -> case err of
          InternalError _ -> expectationFailure "Should never attempt to convert unknown types"
          UnknownContentTypes types -> types `shouldMatchList` [fakeType2]
    it "returns Failure when the input content is invalid" $ do
      case convert DocX HTML "not a real docx" of
        Right _ -> expectationFailure "Shouldn't return success with invalid input"
        Left err -> case err of
          InternalError message -> not (null message) `shouldBe` True
          UnknownContentTypes _ -> expectationFailure "Content types should be known"

    --   -- isLeft (convert DocX HTML "not a real docx") `shouldBe` True
    --   expectationFailure "Ope"
    it "returns Success with Markdown -> HTML conversion" $ do
      case convert Markdown HTML "# Test" of
        Left _ -> expectationFailure "Didn't convert Markdown to HTML"
        Right output -> output `shouldBe` "<h1>Test</h1>"

    it "returns Success with HTML -> Markdown conversion" $ do
      case convert HTML Markdown "# Test" of
        Left _ -> expectationFailure "Didn't convert HTML to Markdown"
        Right output -> output `shouldBe` "# Test\n"

-- TODO: DocX conversion tests. Should be basic, just to test that the mappings are correct
-- Beyond that, assume pandoc does its job
