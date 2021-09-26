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
          fromContentType' = showContentType fromContentType
          toContentType' = showContentType toContentType

getContentTypeFromFileInfo :: FileInfo c -> ContentType LT.Text
getContentTypeFromFileInfo = getContentType . LT.decodeUtf8 . LB.fromStrict . fileContentType
