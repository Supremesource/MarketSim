{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module RunSettings where
import           DataTypes (Options (..))
-- ? NOTE:
-- YOU CAN READ MORE ABOUT THESE SETTINGS IN:
-- | doc/SETTINGS.md
-- | if you are not sure what to do, just leave the settings as they are, or read the documentation above

-- Contents:
-- | !GENERAL SETTINGS / simulation settings
-- | !POSITIONING SETTINGS / runProgram cycle
-- | !ORDERBOOK SETTINGS / liquidity  / order walls / orderbook structure


-- FILL IN THE SETTINGS BELOW !     :


-- ! GENERAL SETTINGS:
--〇 ID = PLTCHRT
-- | if TRUE you will see price displayed as a chart
plotCharts :: Bool
plotCharts = False

--〇 ID = STRVAL
-- | starting value
-- | you can activate this price point by runProgramning `w` - wiping runProgram
wipingStartingValue :: Int
wipingStartingValue = 1

--〇 ID = NUMPOS
-- | number of positions you want to take place in the simulation runProgram
-- | number of positions
numPositions :: Int
numPositions = 5

--〇 ID = NUMrunProgram
-- | number of runPrograms, this is a loop how many times will the simulation repeat itself (random generators are updating each time though)
numberOfrunPrograms :: Int
numberOfrunPrograms = 1


-- ! activate later

--〇 ID = maxM/T
-- TODO add more complex statistic distribution
-- | what is the maximum of makers in one transaction , i.e 1000 buy matched with 1000 sell, now the max makers filled in that transaction can be specified below
maxMakers :: Int
maxMakers = 4
-- | note that max takers is hardcoded to be 95% of maxmakers (done on real market observtions)
-- ? Not recommended to change this from 0.95
maxTakers :: Int
maxTakers = round (fromIntegral maxMakers * (0.95 :: Double))


-- | in terms of newly opened positions



--〇 ID = ROUND
-- | ROUNDING SETTINGS:
-- | max decimals in the orderbook
-- | for illiquid coins use wider rounding
-- | 1 = x.x
-- | 2 = x.xx
-- | 3 = ..
-- Rounding
maxDecimal :: Int
maxDecimal = 4

-- ! POSITIONING SETTINGS
-- 〇 ID = CYCLE
-- | CUSTOM runProgram SETTINGS:
-- | how runProgram is going to be structured
-- | you can choose from the following options:
-- | RANDOM -> UP, UPP, CN, DWW, DW
-- | RANDOM = random
-- | UP = up
-- | UPP = up up
-- | CN = constant / consolidation
-- | DWW = down down
-- | DW = down
-- | for more info you can check the LIB.hs file, to see how everything works
runProgramlist :: [Options]
runProgramlist = [DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW] 


--  Volume settings:
-- 〇 ID = VOL
-- | (functionality defined in Lib)
-- | note that this function only works as a correctness checker for yourslf, exchanges always have a minimum volume allowed by the user, make yours
-- | not recommended to go below 10 , depends on your maxmakers, maxtakers, there is potential error catching metric implemented, but still set this rather high
minvolume :: Int
minvolume = 80000

--〇 ID = VOL02
-- | BUY VOUME
-- | longs NEW
basecaseValueLongNew :: Int
basecaseValueLongNew = 80000
upperBoundLongNew :: Int
upperBoundLongNew = 10000000
-- | shorts CLOSE
basecaseValueShortClose :: Int
basecaseValueShortClose = 80000
upperBoundShortClose :: Int
upperBoundShortClose = 10000000
-- | SELL VOLUME
-- | shorts NEW
basecaseValueShortNew :: Int
basecaseValueShortNew = 80000
upperBoundShortNew :: Int
upperBoundShortNew = 80000
-- | longs CLOSE
basecaseValueLongClose :: Int
basecaseValueLongClose = 80000
upperBoundLongClose :: Int
upperBoundLongClose = 80000

-- Statistics :
-- | Position-Status occurrence:
-- | in %
-- | note that setting 'should' add up to 100 %, it's a good practice at least, for keeping track :)

-- 〇 ID = STAT

--  VOLUME Probability

-- | BUY VOLUME
buyTakerProb :: Int
buyTakerProb = 50
-- | SELL VOLUME
sellTakerProb :: Int
sellTakerProb = 50



-- ? all Prob have a minimum value of 1 and maximum value of 10 ! 
-- | otherwise an error will be thrown
stopProb :: Int
stopProb = 7

-- TODO move into settings
takerxProb :: Int
takerxProb = 7

closingProb :: Int
closingProb = 7

-- ! ORDERBOOK SETINGS
--  + liquidity settings
--  + wall settings
-- | logic for definign intervals // prices at each level

--〇 ID = bookMinMax$
-- ? Minimuim $ amount of order
minimum' :: Int
minimum' = 100 --  minimum order  $ amount
-- ? Maximum $ amount of order
maximum' :: Int
maximum' = 400000 -- maximum order $ amount

--〇 ID = bookMinMaxMove
-- | define how to orderbook grid is going to be moving
-- | Minimum & Maximum UP move in the orderbook structure
-- | for bid = bid liquidity (< min move + max move <  = more liquidity, vice versa)
-- TODO add more complex statistical distribuiton
minUpMove :: Double
minUpMove = 0.009
maxUpMove :: Double
maxUpMove = 0.01
-- | Minimum & Maximum DOWN move in the orderbook structure
-- | for ask = ask liquidity (< min move + max move <  = more liquidity, vice versa)
minDownMove :: Double
minDownMove = 0.009
maxDownMove :: Double
maxDownMove = 0.01

--〇 ID = TakeBidAsk
-- | Size of bid orderbook
takeamountBID :: Int
takeamountBID = 10000
-- | Size of ask orderbook
takeamountASK :: Int
takeamountASK = 10000

--  'WALL' SETTINGS:
--〇 ID = wallLikeHood
-- | intervals for walls
-- ? Order book WALL occurrences
-- | /2  -- (recommended 40-80, possibly even higher, it is going to be `div` by 2 so it gets distributed into bids and asks )
-- | defines in how many orders in the initial book will a wall occour
orderwalllikelyhood :: Int
orderwalllikelyhood = 200

--〇 ID = wallAmp
-- | Amplifier of Wall  occurrences
-- | will amplify the maximum to liking (the higher the more the maximum will get multiplied, so the bigger the walls will be)
wallAmplifier :: Int
wallAmplifier = 5
