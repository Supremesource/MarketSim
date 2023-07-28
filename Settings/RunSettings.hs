{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-
Supreme Source (c) 2023
All rights reserved.
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Supreme Source nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}
module RunSettings 
{-
defines settings
-}
where
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

-- every run new == True
autoRestore :: Bool
autoRestore = True

-- ! GENERAL SETTINGS:
--〇 ID = PLTCHRT
-- | if TRUE you will see price displayed as a chart
plotCharts :: Bool
plotCharts = False


-- must be between 1 and 500
-- note that 305 is the base leverage pareto distribution starts at
-- hence if you want highly dynamic leverage, set it to (250 - 305)
  -- 430 will make leverage > 10 all of the times
baseLeverage :: Int
baseLeverage = 305 

--〇 ID = STRVAL
-- | starting value
-- | you can activate this price point by runProgramning `w` - wiping runProgram
wipingStartingValue :: Int
wipingStartingValue = 100

--〇 ID = NUMPOS
-- | number of positions you want to take place in the simulation runProgram
-- | number of positions
numPositions :: Int
numPositions = 5000

--〇 ID = NUMrunProgram
-- | number of runPrograms, this is a loop how many times will the simulation repeat itself (random generators are updating each time though)
--numberOfrunPrograms :: Int
--numberOfrunPrograms = 1


-- ! activate later

{-
--〇 ID = maxM/T
-- TODO add more complex statistic distribution
-- | what is the maximum of makers in one transaction , i.e 1000 buy matched with 1000 sell, now the max makers filled in that transaction can be specified below
-- ! do not change , i will remove this later
maxMakers :: Int
maxMakers = 1
-- | note that max takers is hardcoded to be 95% of maxmakers (done on real market observtions)
-- ? Not recommended to change this from 0.95
maxTakers :: Int
maxTakers = round (fromIntegral maxMakers * (0.95 :: Double))
-}

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

-- ! list start is the end and list end is the start (SO IT MAIGHT BE CONFUSING)
-- (the thing you write at the start is the end of the run vice versa)
runProgramlist :: [Options]
runProgramlist = [DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW]

--  Volume settings:
-- 〇 ID = VOL
-- | (functionality defined in Lib)
-- | note that this function only works as a correctness checker for yourslf, exchanges always have a minimum volume allowed by the user, make yours
-- | not recommended to go below 10 , depends on your maxmakers, maxtakers, there is potential error catching metric implemented, but still set this rather high
-- ! note that volume can exceed your maximum value up to 5 x times when volume spike volume is activated in statistics

--〇 ID = VOL02
-- | BUY VOUME
minBuyVol :: Int
minBuyVol = 100
maxBuyVol :: Int
maxBuyVol = 400000
-- | SELL VOLUME
minSellVol :: Int
minSellVol = 100
maxSellVol :: Int
maxSellVol = 400000


-- previous stat amount now it's probability 
--  VOLUME Probability
-- | BUY VOLUME
buyTakerProb :: Int
buyTakerProb = 50
-- | SELL VOLUME
sellTakerProb :: Int
sellTakerProb = 50

-- ? all Prob have a minimum value of 1 and maximum value of 10 ! 
-- | otherwise an error will be thrown

-- TODO debug why low better performace 
stopProb :: Int
stopProb = 9

takerxProb :: Int
takerxProb = 5

closingProb :: Int
closingProb = 1

-- TODO add closing probability x
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
takeamountBID = 20000
-- | Size of ask orderbook
takeamountASK :: Int
takeamountASK = 20000

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
