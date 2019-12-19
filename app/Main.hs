{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude
import System.Directory

import Advent (run)

main :: IO ()
main = getArgs >>= \case
    [] -> listDirectory "i" >>= traverse_ run . sort
    ii -> traverse_ run ii
