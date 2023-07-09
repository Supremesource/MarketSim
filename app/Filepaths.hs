{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Filepaths where

-- | FILEPATHS:


-- ? data
-- | orderbook
askBookP :: FilePath
askBookP = "data/askbook.json"

bidBookP :: FilePath
bidBookP = "data/bidbook.json"

-- | price
initPriceP :: FilePath
initPriceP = "data/initPrice.json"

posFutureP :: FilePath
posFutureP = "data/posFuture.json"



-- ? output
logP :: FilePath
logP = "output/logInfo.json"

orderBookDetailsP :: FilePath
orderBookDetailsP = "output/bookInfo.json"

positionInfoP :: FilePath
positionInfoP = "output/positionInfo.json"
