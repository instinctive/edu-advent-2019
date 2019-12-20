{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Utils where

import BasePrelude
import Data.Text (Text)
import qualified Data.Text      as T
import qualified Data.Text.Read as T

decimal :: Text -> Int
decimal = either error fst . T.decimal

signed :: Text -> Int
signed = either error fst . T.signed T.decimal

parseCode :: Text -> [Int]
parseCode = map signed . T.splitOn ","
