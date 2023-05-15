module Orderbook.App.Settings.Arunsettings where
import Data.Text.Internal.Read (T(T))

-- defining data typ for volume side
data VolumeSide
  = Buy
  | Sell 
   deriving Eq

instance Show VolumeSide where
  show :: VolumeSide -> String
  show Buy = "Buy"
  show Sell = "Sell"


-- !    ⬇️ FILL IN ⬇️       :


-- ! RUN SETTINGS:

--〇 1 
-- ? WIPING RUN
-- | NEW RUN / Wiping, Deleting run
wipingRUN :: Bool
wipingRUN = False


--〇 2
-- ? STARTING VALUE IN WIPING RUN
wipingStartingValue :: Int
wipingStartingValue = 1000


--〇 3
-- ? AUTOMATED RUN
automatedrun :: Bool
automatedrun = False


--〇 4
--  manual run settings (case 🔼 set to False):
-- ? MANUAL RUN volume side
-- |FILL IN IF AUTOMATED RUN == "FALSE"
manualrunVside :: VolumeSide
manualrunVside = Buy


--〇 5
-- ? Manual run Volume amount
-- | FILL IN IF AUTOMATED RUN == "FALSE"
manualrunVolumeamount :: Int
manualrunVolumeamount = 90000000


--〇 6
-- automated run settings (case set to True):
-- ? Lower volume for auto run
-- | VOLUME SETTINGS IN AUTOMATED RUN
-- | the lower volume in run
lowerVolume :: Int
lowerVolume = 100 -- ** fill in


--〇 7
-- ? Higher volume in run
higherVolume :: Int
higherVolume = 90000000 -- ** fill in


--〇 8
-- ? Probability distriboution of the side of volume
-- | SIDE -> Buy/Sell probability distribution (in auto run)
-- | must be between 0 > 1
-- | 1 == TRUE, 0 == False -> 100% etc.
buySellProbability :: Double
buySellProbability = 0.9

-- ! ORDERBOOK SETTINGS

--〇 9
-- ? Size of bid orderbook
takeamountBID :: Int
takeamountBID = 10000 -- ** fill in / size of bid order book


--〇 10
-- ? Size of ask orderbook
takeamountASK :: Int
takeamountASK = 10000 -- ** fill in / size of ask book


-- ! LIQUIDITY SETTINGS:
-- | logic for definign intervals // prices at each level

--〇 11
-- ? Minimuim $ amount of order
minimum' :: Int
minimum' = 1000 --  minimum order  $ amount


--〇 12
-- ? Maximum $ amount of order
maximum' :: Int
maximum' = 1000000 -- maximum order $ amount


--〇 13
-- ? Minimum & Maximum UP move in the orderbook structure
-- | for bid = bid liquidity (< min move + max move <  = more liquidity, vice versa)
minUpMove :: Double
minUpMove = 0.1
maxUpMove :: Double
maxUpMove = 0.5


--〇 14
-- ? Minimum & Maximum DOWN move in the orderbook structure
-- | for ask = ask liquidity (< min move + max move <  = more liquidity, vice versa)
minDownMove :: Double
minDownMove = 0.1
maxDownMove :: Double
maxDownMove = 0.5

-- ? larger spread
-- TODO
largerSpread :: Bool
largerSpread = False

-- ! ORDER WALL SETTINGS:
-- | intervals for walls


--〇 15
-- ? Arder book WALL occurrences
-- | /2  -- (recommended 40-80, possibly even higher, it is going to be `div` by 2 so it gets distributed into bids and asks )
-- | defines in how many orders in the initial book will a wall occour
orderwalllikelyhood :: Int
orderwalllikelyhood = 100

--〇 16 
-- ? Amplifier of Wall  occurrences
-- | will amplify the maximum to liking (the higher the more the maximum will get multiplied, so the bigger the walls will be)
wallAmplifier :: Int
wallAmplifier = 4

-- !! ROUNDING SETTINGS:
-- | max decimals in the orderbook
-- | for illiquid coins use wider rounding
-- | 1 = x.x
-- | 2 = x.xx 
-- | 3 = ..

--〇 17
-- ? Rounding
maxDecimal :: Int
maxDecimal = 1 -- ** fill in
