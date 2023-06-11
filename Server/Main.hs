{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Web.Scotty

import Lib (readBook)

-- load data from files

main :: IO ()
main = do
  bidBookData <- readBook "data/bidbook.json"
  askBookData <- readBook "data/askbook.json"

  scotty 8000 $ do
    get "/books" $ do
      json $ (bidBookData, askBookData)
    get "/:word" $ do
      beam <- param "word"
      html $ mconcat ["<h1>Scotty, ", beam, " me up!</h1>"]

-- "/simulation/<simid>/orders/?start=21930&end=21940"
