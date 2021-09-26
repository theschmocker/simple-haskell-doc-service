-- |
module Lib.Conversion (ConversionResult(..), convert) where

import Lib.ContentType

import qualified Data.ByteString.Char8 as BS
import Text.Pandoc (Pandoc, ReaderOptions, WriterOptions, def, readDocx, readHtml, readMarkdown, runPure, writeDocx, writeHtml5String, writeMarkdown)
import Text.Pandoc.Class (PandocMonad)
import qualified Data.Text.Lazy.Encoding as LB
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT

data ConversionResult
  = MissingReader
  | MissingWriter
  | MissingBoth
  | Failure String
  | Success BS.ByteString

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
