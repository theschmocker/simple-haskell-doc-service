{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as LT
import Network.HTTP.Types
import Network.Wai.Middleware.RequestLogger
import Network.Wai.Parse
import Prelude.Compat
import Web.Scotty
import Prelude ()

import Lib.ContentType
import Lib.Conversion

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev

  post "/convert" $ do
    fs <- files
    let [(_, fileInfo)] = fs

    acceptHeader <- header "Accept"
    let fromContentType = getContentTypeFromFileInfo fileInfo

    case getContentType <$> acceptHeader of
      Nothing -> do
        status badRequest400
        text "Missing \"Accept\" header"
      Just toContentType -> case convert fromContentType toContentType (LB.toStrict $ fileContent fileInfo) of
        Left err -> do
          status badRequest400
          case err of
            InternalError message -> text $ "Conversion " <> conversionText <> " failed: " <> LT.pack message
            UnknownContentTypes types -> do
              let typesPlural = if length types > 1 then "types" else "type"
              text $ "Conversion "
                        <> conversionText
                        <> " failed. Not sure how to handle "
                        <> typesPlural
                        <> ": "
                        <> LT.intercalate ", " (map showContentType types)
        Right contents -> do
          setHeader "Content-Type" toContentType'
          raw $ LB.fromStrict contents
        where
          fromContentType' = showContentType fromContentType
          toContentType' = showContentType toContentType
          conversionText = "from \"" <> fromContentType' <> "\" to \"" <> toContentType' <> "\""

getContentTypeFromFileInfo :: FileInfo c -> ContentType LT.Text
getContentTypeFromFileInfo = getContentType . LT.decodeUtf8 . LB.fromStrict . fileContentType
