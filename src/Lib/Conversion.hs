-- |
module Lib.Conversion (ConversionError (..), convert) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LB
import Lib.ContentType
import Text.Pandoc (Pandoc, ReaderOptions, WriterOptions, def, readDocx, readHtml, readMarkdown, runPure, writeDocx, writeHtml5String, writeMarkdown)
import Text.Pandoc.Class (PandocMonad)

data ConversionError a = UnknownContentTypes [ContentType a] | InternalError String
  deriving (Show, Eq)

convert :: ContentType a -> ContentType a -> BS.ByteString -> Either (ConversionError a) BS.ByteString
convert fromCt toCt content = case (maybeReader, maybeWriter) of
  (Nothing, Nothing) -> Left $ UnknownContentTypes [fromCt, toCt]
  (Nothing, _) -> Left $ UnknownContentTypes [fromCt]
  (_, Nothing) -> Left $ UnknownContentTypes [toCt]
  (Just reader, Just writer) ->
    case tryConvert reader writer content of
      Left err -> Left $ InternalError (show err)
      Right output -> Right $ LB.toStrict output
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
      Unknown _ -> Nothing
    mkTextReader f opts' = f opts' . T.decodeUtf8
    mkBSReader f opts' = f opts' . LB.fromStrict

getWriter :: (PandocMonad m) => ContentType a -> WriterOptions -> Maybe (Pandoc -> m LB.ByteString)
getWriter ct opts = ($ opts) <$> writer
  where
    writer = case ct of
      DocX -> pure writeDocx
      HTML -> pure $ mkTextWriter writeHtml5String
      Markdown -> pure $ mkTextWriter writeMarkdown
      Unknown _ -> Nothing
    mkTextWriter f opts' p = LB.encodeUtf8 . LT.fromStrict <$> f opts' p
