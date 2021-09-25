{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.String (IsString)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LB
import qualified Data.Text.Lazy.Encoding as LT
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import Prelude.Compat
import Text.Pandoc (Pandoc, ReaderOptions, WriterOptions, def, readDocx, readHtml, readMarkdown, runPure, writeDocx, writeHtml5String, writeMarkdown)
import Text.Pandoc.Class (PandocMonad)
import Web.Scotty
import Prelude ()

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  post "/convert" $ do
    fs <- files
    let [(_, fileInfo)] = fs

    acceptHeader <- header "Accept"
    let fromContentType = (getContentType . LT.decodeUtf8 . LB.fromStrict) $ fileContentType fileInfo

    case getContentType <$> acceptHeader of
      Nothing -> do
        status badRequest400
        text "Missing \"Accept\" header"
      Just toContentType -> case convert fromContentType toContentType (LB.toStrict $ fileContent fileInfo) of
        MissingReader -> do
          status badRequest400
          text $ "Cannot convert from type: " <> fromContentType'
        MissingWriter -> do
          status badRequest400
          text $ "Cannot convert to type: " <> toContentType'
        MissingBoth -> do
          status badRequest400
          text $ "Cannot convert files from \"" <> fromContentType' <> "\" to \"" <> toContentType' <> "\""
        Failure err -> do
          status badRequest400
          text $ "Conversion failed: " <> LT.pack err
        Success contents -> do
          setHeader "Content-Type" toContentType'
          raw $ LB.fromStrict contents
        where
          packCt = LT.pack . show
          fromContentType' = packCt fromContentType
          toContentType' = packCt toContentType

data ConversionResult
  = MissingReader
  | MissingWriter
  | MissingBoth
  | Failure String
  | Success BS.ByteString

data ContentType a
  = HTML
  | Markdown
  | DocX
  | Unknown a

convert :: ContentType a -> ContentType b -> BS.ByteString -> ConversionResult
convert fromCt toCt content = case (maybeReader, maybeWriter) of
  (Nothing, Nothing) -> MissingBoth
  (Nothing, _) -> MissingReader
  (_, Nothing) -> MissingWriter
  (Just reader, Just writer) ->
    case tryConvert reader writer content of
      Left x -> Failure $ show x
      Right doc -> Success $ LB.toStrict doc
  where
    maybeReader = getReader fromCt def
    maybeWriter = getWriter toCt def
    tryConvert reader' writer' content' = runPure $ reader' content' >>= writer'

getReader :: (PandocMonad m) => ContentType a -> ReaderOptions -> Maybe (BS.ByteString -> m Pandoc)
getReader ct opts = ($ opts) <$> reader
  where
    reader = case ct of
      DocX -> pure $ mkBSReader readDocx
      Markdown -> pure $ mkTextReader readMarkdown
      HTML -> pure $ mkTextReader readHtml
      _ -> Nothing
    mkTextReader f opts' = f opts' . T.decodeUtf8
    mkBSReader f opts' = f opts' . LB.fromStrict

getWriter :: (PandocMonad m) => ContentType a -> WriterOptions -> Maybe (Pandoc -> m LB.ByteString)
getWriter ct opts = ($ opts) <$> writer
  where
    writer = case ct of
      DocX -> pure writeDocx
      HTML -> pure $ mkTextWriter writeHtml5String
      Markdown -> pure $ mkTextWriter writeMarkdown
      _ -> Nothing
    mkTextWriter f opts' p = LB.encodeUtf8 . LT.fromStrict <$> f opts' p

getContentType :: (IsString a, Eq a) => a -> ContentType a
getContentType contentType = case contentType of
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document" -> DocX
  "text/html" -> HTML
  "text/markdown" -> Markdown
  _ -> Unknown contentType

instance (Show a) => Show (ContentType a) where
  show DocX = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
  show HTML = "text/html"
  show Markdown = "text/markdown"
  show (Unknown ct) = show ct
