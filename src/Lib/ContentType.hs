{-# LANGUAGE OverloadedStrings #-}
-- |

module Lib.ContentType where
import Data.String (IsString)

data ContentType a
  = HTML
  | Markdown
  | DocX
  | Unknown a

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
