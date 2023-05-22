module RunSettings
 ( numberOfRuns
 , numPositions
 , wipingStartingValue
 , takeamountBID
 , takeamountASK
 , minimum'
 , maximum'
 , minUpMove
 , maxUpMove
 , minDownMove
 , maxDownMove
 , largerSpread
 , orderwalllikelyhood
 , wallAmplifier
 , maxDecimal
 , plotCharts
 , basecaseValueLongNew
 , basecaseValueShortNew
 , upperBoundLongNew
 , basecaseValueShortClose
 , upperBoundShortClose
 , upperBoundShortNew
 , basecaseValueLongClose
 , upperBoundLongClose
 , xProbabilityTaker
 , yProbabilityTaker
 , zProbabilityTaker
 , fProbabilityTaker
 , xProbabilityMaker
 , yProbabilityMaker
 , zProbabilityMaker
 , fProbabilityMaker
 , maxmakers
 , minvolume
 , maxtakers

 )

where





-- !    ⬇️ FILL IN ⬇️       :


-- ! RUN SETTINGS:

maxmakers :: Int
maxmakers = 6


-- | note that max takers is hardcoded to be 95% of maxmakers (done on real market observtions)
-- ? Not recommended to change this from 0.95
maxtakers :: Int
maxtakers = round (fromIntegral maxmakers * (0.95 :: Double))



plotCharts :: Bool
plotCharts = False -- ** fill in / plot charts (True/False)

-- | number of runs
numberOfRuns :: Int
numberOfRuns = 2 -- Put your actual number of runs here


-- | number of positions
numPositions :: Int
numPositions = 2 -- ** fill in / number of positions


--〇 2
-- ? STARTING VALUE IN WIPING RUN
wipingStartingValue :: Int
wipingStartingValue = 1000 -- ** fill in / starting value in wiping run


-- ! ORDERBOOK SETTINGS

--〇 9
-- ? Size of bid orderbook
takeamountBID :: Int
takeamountBID = 1000 -- ** fill in / size of bid order book


--〇 10
-- ? Size of ask orderbook
takeamountASK :: Int
takeamountASK = 1000 -- ** fill in / size of ask book


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
orderwalllikelyhood = 10000000

--〇 16
-- ? Amplifier of Wall  occurrences
-- | will amplify the maximum to liking (the higher the more the maximum will get multiplied, so the bigger the walls will be)
wallAmplifier :: Int
wallAmplifier = 0

-- !! ROUNDING SETTINGS:
-- | max decimals in the orderbook
-- | for illiquid coins use wider rounding
-- | 1 = x.x
-- | 2 = x.xx
-- | 3 = ..

--〇 17
-- ? Rounding
maxDecimal :: Int
maxDecimal = 2


-- 〇 18
-- ? Volume settings:
-- | (functionality defined in Lib)

minvolume :: Int
minvolume = 1000

-- ! BUY VOUME
-- | longs NEW
basecaseValueLongNew :: Int
basecaseValueLongNew = 1000
upperBoundLongNew :: Int
upperBoundLongNew = 100000
-- | shorts CLOSE
basecaseValueShortClose :: Int
basecaseValueShortClose = 1000
upperBoundShortClose :: Int
upperBoundShortClose = 100000

-- ! SELL VOLUME
-- | shorts NEW
basecaseValueShortNew :: Int
basecaseValueShortNew = 1000
upperBoundShortNew :: Int
upperBoundShortNew = 100000
-- | longs CLOSE
basecaseValueLongClose :: Int
basecaseValueLongClose = 1000
upperBoundLongClose :: Int
upperBoundLongClose = 100000



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
zProbabilityTaker = 150
-- | SELL VOLUME
fProbabilityTaker :: Int
fProbabilityTaker = 150

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
zProbabilityMaker = 150
-- | SELL VOLUME
fProbabilityMaker :: Int
fProbabilityMaker = 150


