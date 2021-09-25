{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

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
import Network.Wai.Middleware.Static
import Network.Wai.Parse
import Prelude.Compat
import System.FilePath ((</>))
import Text.Blaze.Html.Renderer.Text (renderHtml)
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes
import Text.Pandoc (Pandoc, Reader (ByteStringReader, TextReader), ReaderOptions, Writer (ByteStringWriter, TextWriter), WriterOptions, def, readDocx, readMarkdown, runPure, writeDocx, writeHtml5, writeHtml5String, writeMarkdown)
import Text.Pandoc.Class (PandocMonad)
import Web.Scotty
import Prelude ()
import Control.Monad (forM_)

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

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "uploads")

  get "/" $ do
    html $
      renderHtml $
        H.html $ do
          H.body $ do
            H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/upload" $ do
              H.input H.! type_ "file" H.! name "foofile"
              H.br
              H.input H.! type_ "submit"

  get "/filetype" $ do
    html $
      renderHtml $
        H.html $ do
          H.body $ do
            H.form H.! method "post" H.! enctype "multipart/form-data" H.! action "/filetype" $ do
              H.input H.! type_ "file" H.! name "foofile"
              H.br
              H.input H.! type_ "submit"

  post "/filetype" $ do
    fs <- files
    html $ renderHtml $
      H.html $ do
        H.body $ do
          H.ul $ forM_ fs (\(fName, f) -> H.toHtml (T.decodeUtf8 (fileName f) <> ": " <> T.decodeUtf8 (fileContentType f)))




  post "/upload" $ do
    fs <- files
    let [(fName, fileInfo)] = fs

    -- liftIO $ mapM_ print fs

    --if fileContentType fileInfo == "text/docx"
    --if fileContentType fileInfo == "application/pdf"

    -- if fileContentType fileInfo == "text/docx"
    --   then text $

    toContentType <- header "Accept"
    let fromContentType = fileContentType fileInfo

    case toContentType of
      Nothing -> do
        status badRequest400
        text "wat"
      Just ct -> case (getWriter ct def, getReader fromContentType def) of
        (Just writer, Just reader) -> do
          setHeader "Content-Type" ct
          let content = let pd = runPure $ do
                              doc <- reader (LB.toStrict $ fileContent fileInfo)
                              writer doc
                        in case pd of
                              Left x -> LB.pack $ show x
                              Right doc -> doc
          liftIO $ LB.putStrLn content
          raw content
        (_, _) -> text $ "Cannot convert file type \"" <> (LT.decodeUtf8 . LB.fromStrict) fromContentType <> "\" to \"" <> ct

-- html $ let pd = runPure $ do
--                 doc <- readDocx def (fileContent fileInfo)
--                 writeHtml5String def doc
--         in case pd of
--                 Left x -> ""
--                 Right doc -> LT.fromStrict doc
-- else text (LT.pack $ show fileInfo)
-- write the files to disk, so they will be served by the static middleware
-- liftIO $ sequence_ [ B.writeFile ("uploads" </> fn) fc | (_,fn,fc) <- fs' ]
-- -- generate list of links to the files just uploaded
-- html $ mconcat [ mconcat [ fName
--                          , ": "
--                          , renderHtml $ H.a (H.toHtml fn) H.! (href $ H.toValue fn) >> H.br
--                          ]
--                | (fName,fn,_) <- fs' ]

-- Create an API endpoint(s) that can accept a docx file and convert it to HTML, PDF, and/or Markdown
