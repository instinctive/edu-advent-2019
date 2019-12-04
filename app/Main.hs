{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import BasePrelude
import Advent ( run, days )

main :: IO ()
main = traverse_ (uncurry run) days
