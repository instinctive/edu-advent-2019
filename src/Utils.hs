{-# LANGUAGE NoImplicitPrelude #-}

module Utils where

import BasePrelude
import qualified Data.Text      as T
import qualified Data.Text.Read as T

decimal :: T.Text -> Int
decimal = either error fst . T.decimal
