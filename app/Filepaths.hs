module Filepaths where


  --  FILEPATHS:
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

-- | config file
logPath :: FilePath
logPath
              = "Data/codeconfiguration.txt"

-- | orderbook
askBookPath :: FilePath
askBookPath
              = "Data/askbook.txt"

bidBookPath :: FilePath
bidBookPath
              = "Data/bidbook.txt"

-- | price history
pricePath :: FilePath
pricePath
              = "Data/pricehistory.txt"

-- | orderbook stats
-- | amount of bids in comparison to amount of asks in the orderbook
bidAskRPath :: FilePath
bidAskRPath
              = "Data/bidAskR.txt"

-- | $ amount of bids & asks in the orderbook
bidToAskRPath :: FilePath
bidToAskRPath
              = "Data/bidToAskR.txt"

-- | overal volume
volumePath :: FilePath
volumePath
              = "Data/volume.txt"

-- | buy volume
buyVolumePath :: FilePath
buyVolumePath
              = "Data/buyVol.txt"

-- | sell volume
sellVolumePath :: FilePath
sellVolumePath
              = "Data/sellVol.txt"

-- | opening longs
newLongsPath :: FilePath
newLongsPath
              = "Data/nLongs.txt"

-- | opening shorts
newShortsPath :: FilePath
newShortsPath
              = "Data/nShorts.txt"

-- | exit longs
exitLongsPath :: FilePath
exitLongsPath
              = "Data/eLongs.txt"

-- | exit shorts
exitShortsPath :: FilePath
exitShortsPath
              = "Data/eShorts.txt"

-- | open interest
openInterestPath :: FilePath
openInterestPath
              = "Data/oInterest.txt"

