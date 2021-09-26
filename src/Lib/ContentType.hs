{-# LANGUAGE OverloadedStrings #-}
-- |

module Lib.ContentType where
import Data.String (IsString)

data ContentType a
  = HTML
  | Markdown
  | DocX
  | Unknown a
  deriving (Eq, Show)

getContentType :: (IsString a, Eq a) => a -> ContentType a
getContentType contentType = case contentType of
  "application/vnd.openxmlformats-officedocument.wordprocessingml.document" -> DocX
  "text/html" -> HTML
  "text/markdown" -> Markdown
  _ -> Unknown contentType

showContentType :: (IsString a) => ContentType a -> a
showContentType DocX = "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
showContentType HTML = "text/html"
showContentType Markdown = "text/markdown"
showContentType (Unknown ct) = ct
