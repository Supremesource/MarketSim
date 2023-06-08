{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Filepaths where

-- | FILEPATHS:

-- | orderbook
askBookP :: FilePath
askBookP = "data/askbook.json"
-- 2
bidBookP :: FilePath
bidBookP = "data/bidbook.json"

initPriceP :: FilePath
initPriceP = "data/initPrice.json"

logP :: FilePath
logP = "output/logInfo.json"

orderBookDetailsP :: FilePath
orderBookDetailsP = "output/bookInfo.json"

positionInfoP :: FilePath
positionInfoP = "output/positionInfo.json"
