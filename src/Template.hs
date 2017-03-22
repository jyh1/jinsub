{-# LANGUAGE OverloadedStrings #-}
module Template where

import Turtle
import Prelude hiding(FilePath)
import qualified Data.Text as T


generatePBS :: FilePath -> [(Text, Text)] -> Shell Line
generatePBS temp vars = do
  let varDefs = map (uncurry getDef) vars
      pat = defPlaceHolder *> return (T.unlines varDefs)
  sed pat (input temp)
  where
    getDef :: Text -> Text -> Text
    getDef = format (s % "=\"" %s% "\"")
    defPlaceHolder = "#DEFS"
