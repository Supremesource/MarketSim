module RunSettings where
import           DataTypes (Options (..))
-- ? NOTE:
-- YOU CAN READ MORE ABOUT THESE SETTINGS IN:
-- | doc/SETTINGS.md
-- | if you are not sure what to do, just leave the settings as they are, or read the documentation above

-- Contents:
-- | !GENERAL SETTINGS / simulation settings 
-- | !POSITIONING SETTINGS / run cycle
-- | !ORDERBOOK SETTINGS / liquidity  / order walls / orderbook structure


-- FILL IN THE SETTINGS BELOW !     :


-- ! GENERAL SETTINGS:
--〇 ID = PLTCHRT 
-- | if TRUE you will see price displayed as a chart
plotCharts :: Bool
plotCharts = True 

--〇 ID = STRVAL
-- | starting value
-- | you can activate this price point by running `w` - wiping run 
wipingStartingValue :: Int
wipingStartingValue = 10000 

--〇 ID = NUMPOS
-- | number of positions you want to take place in the simulation run
-- | number of positions
numPositions :: Int
numPositions = 1000

--〇 ID = NUMRUN
-- | number of runs, this is a loop how many times will the simulation repeat itself (random generators are updating each time though)
numberOfRuns :: Int
numberOfRuns = 1

--〇 ID = maxM/T
-- | what is the maximum of makers in one transaction , i.e 1000 buy matched with 1000 sell, now the max makers filled in that transaction can be specified below
maxMakers :: Int
maxMakers = 100
-- | note that max takers is hardcoded to be 95% of maxmakers (done on real market observtions)
-- ? Not recommended to change this from 0.95
maxTakers :: Int
maxTakers = round (fromIntegral maxMakers * (0.95 :: Double))

--〇 ID = ROUND
-- | ROUNDING SETTINGS:
-- | max decimals in the orderbook
-- | for illiquid coins use wider rounding
-- | 1 = x.x
-- | 2 = x.xx
-- | 3 = ..
-- Rounding
maxDecimal :: Int
maxDecimal = 2

-- ! POSITIONING SETTINGS
-- 〇 ID = CYCLE
-- | CUSTOM RUN SETTINGS:  
-- | how run is going to be structured
-- | you can choose from the following options:
-- | RANDOM -> UP, UUP, CN, DWW, DW
-- | RANDOM = random 
-- | UP = up
-- | UUP = up up
-- | CN = constant / consolidation
-- | DWW = down down
-- | DW = down
-- | for more info you can check the LIB.hs file, to see how everything works
runlist :: [Options]
runlist = [RANDOM, UP, DWW, RANDOM, RANDOM, UUP, RANDOM, RANDOM, UP, CN, CN, UP, UP, RANDOM, UUP, RANDOM, RANDOM, DW, DWW, UUP, CN, RANDOM, RANDOM, DW, DWW, UUP, RANDOM, CN, UP, RANDOM]


--  Volume settings:
-- 〇 ID = VOL
-- | (functionality defined in Lib)
-- | note that this function only works as a correctness checker for yourslf, exchanges always have a minimum volume allowed by the user, make yours
-- | not recommended to go below 10 , depends on your maxmakers, maxtakers, there is potential error catching metric implemented, but still set this rather high
minvolume :: Int
minvolume = 200

--〇 ID = VOL02
-- | BUY VOUME
-- | longs NEW
basecaseValueLongNew :: Int
basecaseValueLongNew = 250
upperBoundLongNew :: Int
upperBoundLongNew = 1000000
-- | shorts CLOSE
basecaseValueShortClose :: Int
basecaseValueShortClose = 250
upperBoundShortClose :: Int
upperBoundShortClose = 800000
-- | SELL VOLUME
-- | shorts NEW
basecaseValueShortNew :: Int
basecaseValueShortNew = 200
upperBoundShortNew :: Int
upperBoundShortNew = 1100000
-- | longs CLOSE
basecaseValueLongClose :: Int
basecaseValueLongClose = 200
upperBoundLongClose :: Int
upperBoundLongClose = 500000

-- Statistics : 
-- | Position-Status occurrence:
-- | in %
-- | note that setting 'should' add up to 100 %, it's a good practice at least, for keeping track :)

-- 〇 ID = STAT

--  Taker Probability
-- | OPENING POSITIONS
-- | BUY VOLUME
xProbabilityTaker :: Int
xProbabilityTaker = 40
-- | SELL VOLUME
yProbabilityTaker :: Int
yProbabilityTaker = 35
-- | CLOSING POSITION
-- | BUY VOLUME
zProbabilityTaker :: Int
zProbabilityTaker = 15
-- | SELL VOLUME
fProbabilityTaker :: Int
fProbabilityTaker = 10

--  Maker Probability
-- | BUY VOLUME
xProbabilityMaker :: Int
xProbabilityMaker = 40
-- | SELL VOLUME
yProbabilityMaker :: Int
yProbabilityMaker = 40
-- | CLOSING POSITION
-- | BUY VOLUME
zProbabilityMaker :: Int
zProbabilityMaker = 10
-- | SELL VOLUME
fProbabilityMaker :: Int
fProbabilityMaker = 10

-- ! ORDERBOOK SETINGS 
--  + liquidity settings
--  + wall settings
-- | logic for definign intervals // prices at each level

--〇 ID = bookMinMax$
-- ? Minimuim $ amount of order
minimum' :: Int
minimum' = 5000 --  minimum order  $ amount
-- ? Maximum $ amount of order
maximum' :: Int
maximum' = 1500000 -- maximum order $ amount

--〇 ID = bookMinMaxMove
-- | define how to orderbook grid is going to be moving
-- | Minimum & Maximum UP move in the orderbook structure
-- | for bid = bid liquidity (< min move + max move <  = more liquidity, vice versa)
minUpMove :: Double
minUpMove = 0.05
maxUpMove :: Double
maxUpMove = 0.5
-- | Minimum & Maximum DOWN move in the orderbook structure
-- | for ask = ask liquidity (< min move + max move <  = more liquidity, vice versa)
minDownMove :: Double
minDownMove = 0.06
maxDownMove :: Double
maxDownMove = 0.5

--〇 ID = SPREAD
-- | larger spread
-- TODO small note for myself, i should check if this function behaves correctly, on the low level not just the IO , cause that is working
largerSpread :: Bool
largerSpread = False

--〇 ID = TakeBidAsk
-- | Size of bid orderbook
takeamountBID :: Int
takeamountBID = 4000 
-- | Size of ask orderbook
takeamountASK :: Int
takeamountASK = 4000 

--  'WALL' SETTINGS:
--〇 ID = wallLikeHood
-- | intervals for walls
-- ? Order book WALL occurrences
-- | /2  -- (recommended 40-80, possibly even higher, it is going to be `div` by 2 so it gets distributed into bids and asks )
-- | defines in how many orders in the initial book will a wall occour
orderwalllikelyhood :: Int
orderwalllikelyhood = 300

--〇 ID = wallAmp
-- | Amplifier of Wall  occurrences
-- | will amplify the maximum to liking (the higher the more the maximum will get multiplied, so the bigger the walls will be)
wallAmplifier :: Int
wallAmplifier = 3
