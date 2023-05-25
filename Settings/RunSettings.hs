module RunSettings




where

import           DataTypes (Options (..))





-- !    ⬇️ FILL IN ⬇️       :


-- ! RUN SETTINGS:

--〇
maxmakers :: Int
maxmakers = 6

--〇
-- | note that max takers is hardcoded to be 95% of maxmakers (done on real market observtions)
-- ? Not recommended to change this from 0.95
maxtakers :: Int
maxtakers = round (fromIntegral maxmakers * (0.95 :: Double))


--〇
plotCharts :: Bool
plotCharts = True -- ** fill in / plot charts (True/False)

--〇
-- | number of runs
numberOfRuns :: Int
numberOfRuns = 10 -- Put your actual number of runs here

--〇
-- | number of positions
numPositions :: Int
numPositions = 100 -- ** fill in / number of positions

--〇
-- ? STARTING VALUE IN WIPING RUN
wipingStartingValue :: Int
wipingStartingValue = 1000 -- ** fill in / starting value in wiping run


-- ! ORDERBOOK SETTINGS

--〇
-- ? Size of bid orderbook
takeamountBID :: Int
takeamountBID = 1000 -- ** fill in / size of bid order book

--〇
-- ? Size of ask orderbook
takeamountASK :: Int
takeamountASK = 1000 -- ** fill in / size of ask book

--〇
-- ! LIQUIDITY SETTINGS:
-- | logic for definign intervals // prices at each level

--〇
-- ? Minimuim $ amount of order
minimum' :: Int
minimum' = 1000 --  minimum order  $ amount


--〇
-- ? Maximum $ amount of order
maximum' :: Int
maximum' = 1000000 -- maximum order $ amount

--〇
-- ? Minimum & Maximum UP move in the orderbook structure
-- | for bid = bid liquidity (< min move + max move <  = more liquidity, vice versa)
minUpMove :: Double
minUpMove = 0.1
maxUpMove :: Double
maxUpMove = 0.5


--〇
-- ? Minimum & Maximum DOWN move in the orderbook structure
-- | for ask = ask liquidity (< min move + max move <  = more liquidity, vice versa)
minDownMove :: Double
minDownMove = 0.1
maxDownMove :: Double
maxDownMove = 0.5

--〇
-- ? larger spread
-- TODO
largerSpread :: Bool
largerSpread = False

--〇
-- ! ORDER WALL SETTINGS:
-- | intervals for walls
-- ? Arder book WALL occurrences
-- | /2  -- (recommended 40-80, possibly even higher, it is going to be `div` by 2 so it gets distributed into bids and asks )
-- | defines in how many orders in the initial book will a wall occour
orderwalllikelyhood :: Int
orderwalllikelyhood = 10000000

--〇
-- ? Amplifier of Wall  occurrences
-- | will amplify the maximum to liking (the higher the more the maximum will get multiplied, so the bigger the walls will be)
wallAmplifier :: Int
wallAmplifier = 0

--〇
-- !! ROUNDING SETTINGS:
-- | max decimals in the orderbook
-- | for illiquid coins use wider rounding
-- | 1 = x.x
-- | 2 = x.xx
-- | 3 = ..

--〇
-- ? Rounding
maxDecimal :: Int
maxDecimal = 2

-- 〇
-- ? Volume settings:
-- | (functionality defined in Lib)

--〇
minvolume :: Int
minvolume = 1000

--〇
-- ! BUY VOUME
-- | longs NEW
basecaseValueLongNew :: Int
basecaseValueLongNew = 1000000
upperBoundLongNew :: Int
upperBoundLongNew = 2000000
-- | shorts CLOSE
basecaseValueShortClose :: Int
basecaseValueShortClose = 1000000
upperBoundShortClose :: Int
upperBoundShortClose = 2000000
-- ! SELL VOLUME
-- | shorts NEW
basecaseValueShortNew :: Int
basecaseValueShortNew = 1000000
upperBoundShortNew :: Int
upperBoundShortNew = 2000000
-- | longs CLOSE
basecaseValueLongClose :: Int
basecaseValueLongClose = 1000000
upperBoundLongClose :: Int
upperBoundLongClose = 2000000



-- ? Position-Status occurrence:
-- | in %
-- | note that setting must add up to 100 %

-- 〇 19 Taker Probability
-- | BUY VOLUME
xProbabilityTaker :: Int
xProbabilityTaker = 30
-- | SELL VOLUME
yProbabilityTaker :: Int
yProbabilityTaker = 30
-- ? CLOSING POSITION
-- | BUY VOLUME
zProbabilityTaker :: Int
zProbabilityTaker = 30
-- | SELL VOLUME
fProbabilityTaker :: Int
fProbabilityTaker = 30

-- 〇 20 Maker Probability
-- | BUY VOLUME
xProbabilityMaker :: Int
xProbabilityMaker = 30
-- | SELL VOLUME
yProbabilityMaker :: Int
yProbabilityMaker = 30
-- ? CLOSING POSITION
-- | BUY VOLUME
zProbabilityMaker :: Int
zProbabilityMaker = 30
-- | SELL VOLUME
fProbabilityMaker :: Int
fProbabilityMaker = 30

--〇
-- | how run is going to be structured
runlist :: [Options]
runlist = [UP , UUP , CN  , CN  , CN  , DWW , DW  , UUP , DW  , UUP  ]
      -- [0-10,10-20,20-30,30-40,40-50,50-60,60-70,70-80,80-90,90-100]
