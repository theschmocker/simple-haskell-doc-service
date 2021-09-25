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

    toContentType <- header "Accept"
    let fromContentType = (LT.decodeUtf8 . LB.fromStrict) $ fileContentType fileInfo

    case toContentType of
      Nothing -> do
        status badRequest400
        text "Missing \"Accept\" header"
      Just ct -> case convert fromContentType ct (LB.toStrict $ fileContent fileInfo) of
        MissingReader -> do
          status badRequest400
          text $ "Cannot convert from type: " <> fromContentType
        MissingWriter -> do
          status badRequest400
          text $ "Cannot convert to type: " <> ct
        MissingBoth -> do
          status badRequest400
          text $ "Cannot convert files from \"" <> fromContentType <> "\" to \"" <> ct <> "\""
        Failure err -> do
          status badRequest400
          text $ "Conversion failed: " <> LT.pack err
        Success contents -> do
          setHeader "Content-Type" ct
          raw $ LB.fromStrict contents

data ConversionResult
  = MissingReader
  | MissingWriter
  | MissingBoth
  | Failure String
  | Success BS.ByteString

convert :: (IsString a, Eq a, IsString b, Eq b) => a -> b -> BS.ByteString -> ConversionResult
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

getReader :: (IsString a, PandocMonad m, Eq a) => a -> ReaderOptions -> Maybe (BS.ByteString -> m Pandoc)
getReader ct opts = ($ opts) <$> reader
  where
    reader = case ct of
      "text/docx" -> pure $ mkBSReader readDocx
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" -> pure $ mkBSReader readDocx
      "text/markdown" -> pure $ mkTextReader readMarkdown
      "text/html" -> pure $ mkTextReader readHtml
      _ -> Nothing
    mkTextReader f opts' = f opts' . T.decodeUtf8
    mkBSReader f opts' = f opts' . LB.fromStrict

getWriter :: (PandocMonad m, IsString a, Eq a) => a -> WriterOptions -> Maybe (Pandoc -> m LB.ByteString)
getWriter ct opts = ($ opts) <$> writer
  where
    writer = case ct of
      "text/docx" -> pure writeDocx
      "application/vnd.openxmlformats-officedocument.wordprocessingml.document" -> pure writeDocx
      "text/html" -> pure $ mkTextWriter writeHtml5String
      "text/markdown" -> pure $ mkTextWriter writeMarkdown
      _ -> Nothing
    mkTextWriter f opts' p = LB.encodeUtf8 . LT.fromStrict <$> f opts' p
