{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Filepaths where
  -- | --------------------
  -- | Log
  -- | Usinng 4 file paths in total:
  -- | Askbook
  -- | Bidbook
  -- | Pricehistory
  -- | bidAskR
  -- | bidToAskR
  -- | volume
  -- | buyVol
  -- | sellVol
  -- | nLongs
  -- | nShorts
  -- | eLongs
  -- | eShorts
  -- | oInterest
  -- | --------------------
-- ! FILEPATHS:
--1
-- | orderbook
askBookPath :: FilePath
askBookPath = "data/askbook.txt"
-- 2
bidBookPath :: FilePath
bidBookPath = "data/bidbook.txt"
-- 3
-- | config file, log
logPath :: FilePath
logPath = "output/log.txt"
-- 4
-- | price history
pricePath :: FilePath
pricePath = "output/pricehistory.txt"
-- 5
-- | orderbook stats
-- | amount of bids in comparison to amount of asks in the orderbook
bidAskRPath :: FilePath
bidAskRPath = "output/bidAskR.txt"
-- 6
-- | $ amount of bids & asks in the orderbook
bidToAskRPath :: FilePath
bidToAskRPath = "output/bidToAskR.txt"
-- 7
-- | overal volume
volumePath :: FilePath
volumePath = "output/volume.txt"
-- 8
-- | buy volume
buyVolumePath :: FilePath
buyVolumePath = "output/buyVol.txt"
-- 9
-- | sell volume
sellVolumePath :: FilePath
sellVolumePath = "output/sellVol.txt"
-- 10
-- | opening longs
newLongsPath :: FilePath
newLongsPath = "output/nLongs.txt"
-- 11
-- | opening shorts
newShortsPath :: FilePath
newShortsPath = "output/nShorts.txt"
-- 12
-- | exit longs
exitLongsPath :: FilePath
exitLongsPath = "output/eLongs.txt"
-- 13
-- | exit shorts
exitShortsPath :: FilePath
exitShortsPath = "output/eShorts.txt"
-- 14
-- | open interest
openInterestPath :: FilePath
openInterestPath = "output/oInterest.txt"
