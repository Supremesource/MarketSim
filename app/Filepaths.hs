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
logPath :: String
logPath
              = "Data/codeconfiguration.txt"

-- | orderbook
askBookPath :: String
askBookPath
              = "Data/askbook.txt"

bidBookPath :: String
bidBookPath
              = "Data/bidbook.txt"

-- | price history
pricePath :: String
pricePath
              = "Data/pricehistory.txt"

-- | orderbook stats
-- | amount of bids in comparison to amount of asks in the orderbook
bidAskRPath :: String
bidAskRPath
              = "Data/bidAskR.txt"

-- | $ amount of bids & asks in the orderbook
bidToAskRPath :: String
bidToAskRPath
              = "Data/bidToAskR.txt"

-- | overal volume
volumePath :: String
volumePath
              = "Data/volume.txt"

-- | buy volume
buyVolumePath :: String
buyVolumePath
              = "Data/buyVol.txt"

-- | sell volume
sellVolumePath :: String
sellVolumePath
              = "Data/sellVol.txt"

-- | opening longs
newLongsPath :: String
newLongsPath
              = "Data/nLongs.txt"

-- | opening shorts
newShortsPath :: String
newShortsPath
              = "Data/nShorts.txt"

-- | exit longs
exitLongsPath :: String
exitLongsPath
              = "Data/eLongs.txt"

-- | exit shorts
exitShortsPath :: String
exitShortsPath
              = "Data/eShorts.txt"

-- | open interest
openInterestPath :: String
openInterestPath
              = "Data/oInterest.txt"

