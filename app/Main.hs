{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forM_)
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as LB
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Encoding as BS
import qualified Data.Text.Lazy.Encoding as LB
import qualified Data.Text.Lazy.Encoding as LT
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import Prelude.Compat
import System.FilePath ((</>))
import Text.Pandoc (Pandoc, Reader (ByteStringReader, TextReader), ReaderOptions, Writer (ByteStringWriter, TextWriter), WriterOptions, def, readDocx, readMarkdown, runPure, writeDocx, writeHtml5, writeHtml5String, writeMarkdown)
import Text.Pandoc.Class (PandocMonad)
import Web.Scotty
import Prelude ()

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  post "/convert" $ do
    fs <- files
    let [(fName, fileInfo)] = fs

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
convert fromCt toCt content = case (reader, writer) of
  (Nothing, Nothing) -> MissingBoth
  (Nothing, _) -> MissingReader
  (_, Nothing) -> MissingWriter
  (Just read, Just write) ->
     case tryConvert read write content of
          Left x -> Failure $ show x
          Right doc -> Success $ LB.toStrict doc
  where
    reader = getReader fromCt def
    writer = getWriter toCt def
    tryConvert read write content = runPure $ read content >>= write

getReader :: (IsString a, PandocMonad m, Eq a) => a -> ReaderOptions -> Maybe (BS.ByteString -> m Pandoc)
getReader "text/docx" opts = Just $ readDocx opts . LB.fromStrict
getReader "application/vnd.openxmlformats-officedocument.wordprocessingml.document" opts = Just $ readDocx opts . LB.fromStrict
getReader "text/markdown" opts = Just $ readMarkdown opts . T.decodeUtf8
getReader _ _ = Nothing

getWriter :: (PandocMonad m, IsString a, Eq a) => a -> WriterOptions -> Maybe (Pandoc -> m LB.ByteString)
getWriter "text/docx" opts = pure $ writeDocx opts
getWriter "application/vnd.openxmlformats-officedocument.wordprocessingml.document" opts = pure $ writeDocx opts
getWriter "text/html" opts = pure (\p -> LB.encodeUtf8 . LT.fromStrict <$> writeHtml5String opts p)
getWriter "text/markdown" opts = pure (\p -> LB.encodeUtf8 . LT.fromStrict <$> writeMarkdown opts p)
getWriter _ _ = Nothing
