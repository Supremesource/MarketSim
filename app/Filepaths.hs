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
-- | orderbook
askBookPath :: FilePath
askBookPath = "data/askbook.txt"

bidBookPath :: FilePath
bidBookPath = "data/bidbook.txt"

-- | config file, log
logPath :: FilePath
logPath = "output/log.txt"

-- | price history
pricePath :: FilePath
pricePath = "output/pricehistory.txt"

-- | orderbook stats
-- | amount of bids in comparison to amount of asks in the orderbook
bidAskRPath :: FilePath
bidAskRPath = "output/bidAskR.txt"

-- | $ amount of bids & asks in the orderbook
bidToAskRPath :: FilePath
bidToAskRPath = "output/bidToAskR.txt"

-- | overal volume
volumePath :: FilePath
volumePath = "output/volume.txt"

-- | buy volume
buyVolumePath :: FilePath
buyVolumePath = "output/buyVol.txt"

-- | sell volume
sellVolumePath :: FilePath
sellVolumePath = "output/sellVol.txt"

-- | opening longs
newLongsPath :: FilePath
newLongsPath = "output/nLongs.txt"

-- | opening shorts
newShortsPath :: FilePath
newShortsPath = "output/nShorts.txt"

-- | exit longs
exitLongsPath :: FilePath
exitLongsPath = "output/eLongs.txt"

-- | exit shorts
exitShortsPath :: FilePath
exitShortsPath = "output/eShorts.txt"

-- | open interest
openInterestPath :: FilePath
openInterestPath = "output/oInterest.txt"
