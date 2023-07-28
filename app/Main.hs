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
module Main
{-
-- ! DESCRIPTION  
calls all functions in modules together to execute 
-}
where

  -- TODO
    -- | display in charts using javascript/ html / css
    -- | make frontend more efficient the way the python script is called now is just terrible+
    -- | better UI with timeframes
    -- | better orderbook market making logic
    
    -- // Todo add leverage info   
    -- add liquidation price into posInfo 
    -- TODO represent sides by data types
    -- TODO add much better leverage info
    -- todo readjust leverage at the end by looking at current margin and initial leverage with initial margin
    -- todo add stop calculation 
    -- TODO add slippage
    -- // todo add leverage info into pos future
    -- TODO delete Max makers to 1
   
    -- todo add restrictive settings for everything
    -- todo when closing take only some part of the position
    -- optimize code, there is too much exponetial time complexity
    -- make general bug check of output
    
    -- // fix negative orderbook levels on bids by stopping ask generation at that point as well, (probable stop ask gen at that point)
    -- //  | make reusable output 

    -- ! bugs
    -- // TODO fix liquidation bug
    -- // fix taker splitting to small amounts 
    -- // fix small volume low
 -- TODO


-- imports
-- | external libraries
import           Control.Monad
import           Control.Parallel.Strategies (parList, rseq, using)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)
--import           System.Process              (callCommand)
import           Data.Sequence               (fromList, Seq)
import           System.Random               (Random (randomRs))
import           Debug.Trace

-- | internal libraries
import           Colours
import           DataTypes
import           Filepaths
import           Generator
import           InputOutput
import           Lib
import           PosCycle
import           RunSettings
import           Statistics
import           System.Random.Stateful      (StdGen)
import           Util
import System.CPUTime
import Text.Printf

main :: IO ()
main = do
  writeFile logP ""
  runProgram

 --  Entry points of program
runProgram :: IO ()
runProgram = do
  start  <- getCPUTime
  putStrLn "Generating dataset ..."
  when autoRestore
    (newrunProgramSettings
      askBookP
      bidBookP
      logP
      orderBookDetailsP
      positionInfoP
      initPriceP
      posCloseDatP
      wipingStartingValue)

  isBidEmpty <- isFileEmpty bidBookP
  isAskEmpty <- isFileEmpty askBookP
  initstartingPoint <- startingPointFromFile initPriceP
  fileBidBook <- if isBidEmpty
                 then return []
                 else readBook bidBookP
  fileAskBook <- if isAskEmpty
                 then return []
                 else readBook askBookP
  -- ? preventing from passing a wrong orderbook due to invalid json read
  when (not isBidEmpty && (null fileBidBook || null fileAskBook)) $ error "book acc []"
  let seqFileBidBook = fromList fileBidBook
  let seqFileAskBook = fromList fileAskBook
  --  CHECKING SETTINGS
  localCheck
  --  RANDOM GENERATORS:
  -- > RANDOMNESS <
  gen1 <- randomGen
  gen2 <- randomGen
  -- ORDERBOOK GENERATION
  oBooks <- orderBook initstartingPoint gen1 gen2
  let (orderbook_ask, orderbook_bid, fullwallsASK, fullwallsBIDS, inittotakefromwall) =
        oBooks
  -- runProgramNING THE PROGRAM
  generator
    isBidEmpty
    isAskEmpty
    orderbook_bid
    orderbook_ask
    seqFileBidBook
    seqFileAskBook
    gen1
    gen2
    fullwallsASK
    fullwallsBIDS
    initstartingPoint
    inittotakefromwall
  -- WARININGS of what might be affest the quality of the settings
  warnings

  end <- getCPUTime
  let diff = fromIntegral (end - start) / (10^(12 :: Integer))
  printf "Dataset generated in %.6f seconds\n" (diff :: Double)
  plotGraph

genVolumeAux :: Int -> Int -> VolumeStage -> [(Int, VolumeSide)] -> IO [(Int, VolumeSide)]
genVolumeAux aux numPositions' stage takerAcc
  | aux >= numPositions' = return takerAcc
  | otherwise = do
      randomToTemplate <- randomOptionGen
      (taker, newStage) <- generateRandomPosition (processTempleaterunProgram aux randomToTemplate) stage
      genVolumeAux (aux + 1) numPositions' newStage (taker ++ takerAcc)

genVolume :: {-Stats-}  IO [(Int, VolumeSide)]
genVolume {-aggregatedStats-}    = do
 -- trace ("mainLoop - remainingrunPrograms: " ++ show remainingrunPrograms) $ return ()
 -- trace ("mainLoop - accumulatedStats: " ++ show accumulatedStats) $ return ()
  genVolumeAux 0 numPositions Undefined []

generator ::
     Bool
  -> Bool
  -> Seq (Double, Int)
  -> Seq (Double, Int)
  -> Seq (Double, Int)
  -> Seq (Double, Int)
  -> StdGen
  -> StdGen
  -> [Int]
  -> [Int]
  -> StartingPoint
  -> Totakefromwall
  -> IO ()
generator isBidEmpty isAskEmpty orderbook_bid orderbook_ask fileBidBook fileAskBook gen1 gen2 fullwallsASK fullwallsBIDS initstartingPoint inittotakefromwall = do
  let bidBook =
        if isBidEmpty
          then orderbook_bid
          else fileBidBook
  let askBook =
        if isAskEmpty
          then orderbook_ask
          else fileAskBook
             -- ? ADDING STATS FROM 'MAINLOOP' TOGETHER
             -- | price change
             -- ? volume ist everything is being generated out of
  volumesAndSides <- genVolume  {-initStats-}
  --putStrLn "volume list : "
  --print volumesAndSides

  let initialBookDetailsList = [initialBookDetails]
  let listofvolumes = volumesAndSides
  --putStrLn $ red "Monte carlo: "
  --print listofvolumes
  -- isCloseEmpty <- isCloseDataEmpty
  --  print isCloseEmpty
  let initAccLongClose = closeAccLong
     -- else do
  -- filterfuture no liquidation for exit long
      --  filterClosePos "f" <$> readClosePos
  let initAccShortClose = closeAccLong
     -- else do
  -- filterfuture no liquidation for exit short
        --filterClosePos  "z" <$> readClosePos

  _ <-
    generaterunProgram
     GenerationPass
      {   initLiquidationAccInput = initLiquidationAcc
        , initPositioningAccInput  = initPositioningAcc
        , initAccLongCloseInput    = fromList initAccLongClose
        , initAccShortCloseInput   = fromList initAccShortClose
        , listofvolumesInput       = listofvolumes
        , bidBookInput             = bidBook
        , askBookInput             = askBook
        , gen1Input                = gen1
        , gen2Input                = gen2
        , fullwallsASKInput        = fullwallsASK
        , fullwallsBIDSInput       = fullwallsBIDS
        , initstartingPointInput   = initstartingPoint
        , inittotakefromwallInput     = inittotakefromwall
        , initialBookDetailsListInput = initialBookDetailsList
        , initStatsInput              = [initStats]
        , liquidationTagInput       = [(False,"")] }

  -- | formating price document
  --   removeEmptyLines pricePath
 -- putStrLn $ gray "OUTPUT SUCCESFULLY GENERATED"

  return ()


-- checking settings, so bugs are caught before the program is runProgramning
localCheck :: IO ()
localCheck = do
  volumechecker
    minBuyVol
    minSellVol
    maxBuyVol
    maxSellVol
  --positionamountcheck  1 -- TODO take out this 1

warnings :: IO ()
warnings
 -- | optional warnings
 = do
  addsupto100 buyTakerProb sellTakerProb

orderBook ::
     Double
  -> StdGen
  -> StdGen
  -> IO (Seq (Double, Int), Seq (Double, Int), [Int], [Int], Int)
orderBook initstartingPoint gen1 gen2
-- ! - ORDERBOOK - ! --
-- | the price simulation is starting at
-- | orderbook
-- | making ask move upside
 = do
  let upMoves = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
  -- | making bid move downside
  let downMoves = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
  -- | liquidity  for ask, the limit setup gradient
  -- | grid of the orderbook
  let setupASK =
        take
          takeamountASK
          (tail (infiniteListUpConstant initstartingPoint gen1 upMoves)) `using`
        parList rseq
        -- | liquidity for ask, the limit setup gradient
  let setupBID =
        take
          takeamountBID
          (tail (infiniteListDownConstant initstartingPoint gen2 downMoves)) `using`
        parList rseq
        -- | generating prices for ASKS $$ amount
  usdamountASK <- printCustomRandomList takeamountASK
  -- | generating prices for BIDS $$ amount
  usdamountBID <- printRandomList' takeamountBID
  -- | Price walls (limit)
  -- | generate limit walls (in terms of their occurrence)
  let inittotakefromwall = taketowalls $ 2 * takeamount
  -- | generating walls, this is an infinite list
  pricewalls <- randomListwalls
  let pricewalllist = take inittotakefromwall pricewalls
  -- | first part of the list above going to bids
  let pricesBids1 = firstPartList pricewalllist
  -- | second part going to asks
  let pricesAsk1 = secondPartList pricewalllist
  -- | full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
  fullwallsASK <- randomlyInsert pricesAsk1 (take takeamountASK usdamountASK)
  -- | full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
  fullwallsBIDS <- randomlyInsert pricesBids1 (take takeamountBID usdamountBID)
  -- ? ADDING DATA TOGETHER
  -- | adding orderbook together & generating additional data
  -- |zipping so that we have orderwalls in  -> orderbook is built
  -- | zipping prices with $ AMOUNT
  let (orderbook_ask, orderbook_bid) =
        ( zipToTuples setupASK fullwallsASK, zipToTuples setupBID fullwallsBIDS)

  -- | the orderbook path which should change the bid price
  -- | orderbook logc:
  return
    ( orderbook_ask
    , orderbook_bid
    , fullwallsASK
    , fullwallsBIDS
    , inittotakefromwall)
