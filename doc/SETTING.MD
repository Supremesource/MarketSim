
# TEMPLEATES

`here are some predefine settings for different looking simulations:`

*(note that PLTCHRT is always set to `TRUE`)*

### 1. High cap liquid
note: the loading time on this one might be a bit longer (expected around 32 sec.) 
```Haskell
- STRVAL         = 10000
- NUMPOS         = 10000
- NUMrunProgram         = 5
- maxM/T         = 100
- ROUND          = 3
- CYCLE          :
[RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM] 

- VOL            = 200
- VOL2           = buy volume:
                   bs;250,up;1000000 |
                   bs;250,up; 800000 |
                  
                   sell volume:
                   bs;200, up;1200000 |
                   bs;200, up;500000  |

- STAT           = taker: x;40, y;35, z;15, f;10  
                   maker: x;40, y;40, z;10, f;10

- BookMinMax$    = min; 5000, max; 1500000
- bookMinMaxMove = min; 0.05, max; 0.5 | min; 0.06, max; 0.5 

- SPREAD         = FALSE
- TakeBidAsk     = bid; 4000, ask; 4000
- wallLikelyHood = 300
- wallAmp        = 3
```
### 2. Low cap illiquid

```Haskell
- STRVAL         = 1
- NUMPOS         = 5000
- NUMrunProgram         = 5
- maxM/T         = 40
- ROUND          = 4
- CYCLE          :
[RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM] 

- VOL            = 80
- VOL2           = buy volume:
                   bs;80,up;10000 |
                   bs;80,up; 11000 |
                  
                   sell volume:
                   bs;80, up;10500 |
                   bs;80, up;12000  |

- STAT           = taker: x;23, y;24, z;25, f;28  
                   maker: x;27, y;26, z;24, f;23

- BookMinMax$    = min; 100, max; 400000
- bookMinMaxMove = min; 0.009, max; 0.01 | min; 0.009, max; 0.01 

- TakeBidAsk     = bid; 10000, ask; 10000
- wallLikelyHood = 200
- wallAmp        = 5
```

# COPY & PASTE

 ```1.```

```haskell


-- ! GENERAL SETTINGS:
--〇 ID = PLTCHRT
-- | if TRUE you will see price displayed as a chart
plotCharts :: Bool
plotCharts = True

--〇 ID = STRVAL
-- | starting value
-- | you can activate this price point by runProgramning `w` - wiping runProgram
wipingStartingValue :: Int
wipingStartingValue = 10000

--〇 ID = NUMPOS
-- | number of positions you want to take place in the simulation runProgram
-- | number of positions
numPositions :: Int
numPositions = 8000

--〇 ID = NUMrunProgram
-- | number of runPrograms, this is a loop how many times will the simulation repeat itself (random generators are updating each time though)
numberOfrunPrograms :: Int
numberOfrunPrograms = 5

--〇 ID = maxM/T
-- TODO add more complex statistic distribution
-- | what is the maximum of makers in one transaction , i.e 1000 buy matched with 1000 sell, now the max makers filled in that transaction can be specified below
maxMakers :: Int
maxMakers = 125
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
maxDecimal = 3

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
runProgramlist = [RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM] 


--  Volume settings:
-- 〇 ID = VOL
-- | (functionality defined in Lib)
-- | note that this function only works as a correctness checker for yourslf, exchanges always have a minimum volume allowed by the user, make yours
-- | not recommended to go below 10 , depends on your maxmakers, maxtakers, there is potential error catching metric implemented, but still set this rather high
minvolume :: Int
minvolume = 250

--〇 ID = VOL02
-- | BUY VOUME
-- | longs NEW
minBuyVol :: Int
minBuyVol = 300
maxBuyVol :: Int
maxBuyVol = 162100
-- | shorts CLOSE
basecaseValueShortClose :: Int
basecaseValueShortClose = 500
upperBoundShortClose :: Int
upperBoundShortClose = 161200
-- | SELL VOLUME
-- | shorts NEW
minSellVol :: Int
minSellVol = 250
maxSellVol :: Int
maxSellVol = 160000
-- | longs CLOSE
basecaseValueLongClose :: Int
basecaseValueLongClose = 250
upperBoundLongClose :: Int
upperBoundLongClose = 160000

-- Statistics :
-- | Position-Status occurrence:
-- | in %
-- | note that setting should add up to 100 %, its a good practice at least, for keeping track :)  

-- 〇 ID = STAT

--  Taker Probability
-- | OPENING POSITIONS
-- | BUY VOLUME
buyTakerProb :: Int
buyTakerProb = 27
-- | SELL VOLUME
sellTakerProb :: Int
sellTakerProb = 24
-- | CLOSING POSITION
-- | BUY VOLUME
zProbabilityTaker :: Int
zProbabilityTaker = 25
-- | SELL VOLUME
fProbabilityTaker :: Int
fProbabilityTaker = 24

--  Maker Probability
-- | BUY VOLUME
xProbabilityMaker :: Int
xProbabilityMaker = 27
-- | SELL VOLUME
yProbabilityMaker :: Int
yProbabilityMaker = 26
-- | CLOSING POSITION
-- | BUY VOLUME
zProbabilityMaker :: Int
zProbabilityMaker = 24
-- | SELL VOLUME
fProbabilityMaker :: Int
fProbabilityMaker = 23

-- ! ORDERBOOK SETINGS
--  + liquidity settings
--  + wall settings
-- | logic for definign intervals // prices at each level

--〇 ID = bookMinMax$
-- ? Minimuim $ amount of order
minimum' :: Int
minimum' = 1000 --  minimum order  $ amount
-- ? Maximum $ amount of order
maximum' :: Int
maximum' = 1200000 -- maximum order $ amount

--〇 ID = bookMinMaxMove
-- | define how to orderbook grid is going to be moving
-- | Minimum & Maximum UP move in the orderbook structure
-- | for bid = bid liquidity (< min move + max move <  = more liquidity, vice versa)
-- TODO add more complex statistical distribuiton
minUpMove :: Double
minUpMove = 0.05
maxUpMove :: Double
maxUpMove = 0.6
-- | Minimum & Maximum DOWN move in the orderbook structure
-- | for ask = ask liquidity (< min move + max move <  = more liquidity, vice versa)
minDownMove :: Double
minDownMove = 0.06
maxDownMove :: Double
maxDownMove = 0.7


--〇 ID = TakeBidAsk
-- | Size of bid orderbook
takeamountBID :: Int
takeamountBID = 6000
-- | Size of ask orderbook
takeamountASK :: Int
takeamountASK = 6000

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
wallAmplifier = 4
```

2.

```Haskell
-- ! GENERAL SETTINGS:
--〇 ID = PLTCHRT
-- | if TRUE you will see price displayed as a chart
plotCharts :: Bool
plotCharts = True

--〇 ID = STRVAL
-- | starting value
-- | you can activate this price point by runProgramning `w` - wiping runProgram
wipingStartingValue :: Int
wipingStartingValue = 1

--〇 ID = NUMPOS
-- | number of positions you want to take place in the simulation runProgram
-- | number of positions
numPositions :: Int
numPositions = 5000

--〇 ID = NUMrunProgram
-- | number of runPrograms, this is a loop how many times will the simulation repeat itself (random generators are updating each time though)
numberOfrunPrograms :: Int
numberOfrunPrograms = 5

--〇 ID = maxM/T
-- TODO add more complex statistic distribution
-- | what is the maximum of makers in one transaction , i.e 1000 buy matched with 1000 sell, now the max makers filled in that transaction can be specified below
maxMakers :: Int
maxMakers = 40
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
runProgramlist = [RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM,RANDOM] 


--  Volume settings:
-- 〇 ID = VOL
-- | (functionality defined in Lib)
-- | note that this function only works as a correctness checker for yourslf, exchanges always have a minimum volume allowed by the user, make yours
-- | not recommended to go below 10 , depends on your maxmakers, maxtakers, there is potential error catching metric implemented, but still set this rather high
minvolume :: Int
minvolume = 80

--〇 ID = VOL02
-- | BUY VOUME
-- | longs NEW
minBuyVol :: Int
minBuyVol = 80
maxBuyVol :: Int
maxBuyVol = 10000
-- | shorts CLOSE
basecaseValueShortClose :: Int
basecaseValueShortClose = 80
upperBoundShortClose :: Int
upperBoundShortClose = 11000
-- | SELL VOLUME
-- | shorts NEW
minSellVol :: Int
minSellVol = 80
maxSellVol :: Int
maxSellVol = 10500
-- | longs CLOSE
basecaseValueLongClose :: Int
basecaseValueLongClose = 80
upperBoundLongClose :: Int
upperBoundLongClose = 12000

-- Statistics :
-- | Position-Status occurrence:
-- | in %
-- | note that setting 'should' add up to 100 %, it's a good practice at least, for keeping track :)

-- 〇 ID = STAT

--  Taker Probability
-- | OPENING POSITIONS
-- | BUY VOLUME
buyTakerProb :: Int
buyTakerProb = 23
-- | SELL VOLUME
sellTakerProb :: Int
sellTakerProb = 24
-- | CLOSING POSITION
-- | BUY VOLUME
zProbabilityTaker :: Int
zProbabilityTaker = 25
-- | SELL VOLUME
fProbabilityTaker :: Int
fProbabilityTaker = 28

--  Maker Probability
-- | BUY VOLUME
xProbabilityMaker :: Int
xProbabilityMaker = 27
-- | SELL VOLUME
yProbabilityMaker :: Int
yProbabilityMaker = 26
-- | CLOSING POSITION
-- | BUY VOLUME
zProbabilityMaker :: Int
zProbabilityMaker = 24
-- | SELL VOLUME
fProbabilityMaker :: Int
fProbabilityMaker = 23

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
```