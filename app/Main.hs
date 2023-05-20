{-# OPTIONS_GHC -Wno-unused-matches #-}
module Main where
{-# LANGUAGE ScopedTypeVariables #-}


-- | external libraries

import           Control.Monad               (forM, replicateM, when)
import           Control.Parallel.Strategies (parList, rseq, using)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)
import           System.Process              (callCommand)
import           System.Random               (Random (randomRs))


-- | internal libraries
import           DataTypes                   (Stats, VolumeSide)
import           Filepaths                   (askBookPath, bidBookPath, logPath,
                                              pricePath)
import           InputOutput                 (orange, printPositionStats,
                                              printStats, red)
import           Lib                         (addsupto100, firstPartList,
                                              infiniteList, infiniteListDown,
                                              isFileEmpty, newRunSettings,
                                              printCustomRandomList,
                                              printRandomList', randomGen,
                                              randomListwalls, randomlyInsert,
                                              readBook, removeEmptyLines,
                                              secondPartList,
                                              startingPointFromFile, takeamount,
                                              taketowalls, zipToTuples)
import           RunSettings                 (fProbabilityMaker,
                                              fProbabilityTaker, maxDownMove,
                                              maxUpMove, minDownMove, minUpMove,
                                              numPositions, numberOfRuns,
                                              plotCharts, takeamountASK,
                                              takeamountBID,
                                              wipingStartingValue,
                                              xProbabilityMaker,
                                              xProbabilityTaker,
                                              yProbabilityMaker,
                                              yProbabilityTaker,
                                              zProbabilityMaker,
                                              zProbabilityTaker)
import           Statistics                  (generateRandomPosition)
import           Util                        (aggregateStats, initStats,
                                              recursiveList)



mainLoop :: Stats -> Int -> IO [(Int, VolumeSide)]
mainLoop aggregatedStats remainingRuns = do
      if remainingRuns > 0
        then do
          let numberR =  abs (1 - (remainingRuns -1))
          let timeframe = remainingRuns * 5 -- 5 for the time frame
          let hours = fromIntegral timeframe / 60
          print $ "number of remaining runs: " ++ show numberR ++ " you will go through: " ++ show timeframe ++ " minutes at the end | hours = " ++ show hours ++ " hours\n"
          positions <- replicateM numPositions generateRandomPosition
       
         -- TODO - resolve the conflict right here
      
          -- ⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️⬇️

          let takerl = [1,1..]
          let newAggregatedStats = foldr (\(pos, rnd) acc -> aggregateStats pos acc rnd) aggregatedStats (zip positions takerl)
          volumesAndSides <- Control.Monad.forM (zip (zip [1..] positions) takerl) $ \((i, pos), rnd) -> do
            (volume, side) <- printPositionStats i pos rnd
            print $ "TESTING :" ++ show volume
            print side
            return (volume, side)

          putStrLn "--------"
          printStats newAggregatedStats
          nextVolumesAndSides <- mainLoop newAggregatedStats (remainingRuns - 1)
          return (volumesAndSides ++ nextVolumesAndSides)
        else do
          printFinal aggregatedStats
          return []


printFinal :: Stats -> IO ()
printFinal aggregatedStats = do
  let timeframe = numberOfRuns * 5 -- 5 for the time frame
  let hours = fromIntegral timeframe / 60
  putStrLn $ "\n🍻congrats you went through: " ++ show hours ++ " hours (if you reached your set runs)"
  putStrLn "\n\n\n\n\n\n-------------\n\n\n you have reached the end of the generator \n\n\n📊📊AGGREGATED STATS📊📊: "
  printStats aggregatedStats

main :: IO ()
main = do

  -- CHECKING IF FILES ARE EMPTY
  isBidEmpty  <- isFileEmpty bidBookPath
  isAskEmpty  <- isFileEmpty askBookPath
  hSetBuffering stdout LineBuffering
  -- WIPING RUN == TRUE
  -- when wiping run is running the whole code is not evaluated
  -- wiping all of the text files, and changing the starting point
  putStrLn $ "Proceed (_ / n)" ++ red "\n\n * for run-restore (w)  *"
  proceed <- getLine

  if proceed == "W" || proceed == "w"
    then do
      let sayStart = show wipingStartingValue
      putStrLn "🚨 YOU JUST RAN WIPING RUN 🚨 "
      putStr "\nnew starting value will be set to: $"
      putStrLn sayStart
      putStrLn $ orange "\n * you can adjsut starting value in the 'RunSetting' * "
      newRunSettings logPath bidBookPath askBookPath pricePath wipingStartingValue
    else if proceed == "n" || proceed == "N"

        then  error (red "stopping program")


    else do


-- | random generator:
      gen1
        <- randomGen
      gen2
        <- randomGen

      -- the price we are starting at

      startingPoint <- startingPointFromFile
-- making ask move upside
      let upMoves = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
-- making bid move downside
      let downMoves =
            take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2

      -- liquidity definition for ask, the limit setup gradient
      let setupASK = take takeamountASK (tail (infiniteList startingPoint gen1 upMoves)) `using` parList rseq

      -- liquidity definition for ask, the limit setup gradient
      let setupBID = take takeamountBID (tail (infiniteListDown startingPoint gen2 downMoves)) `using` parList rseq

      -- generating prices for ASKS $$ amount
      amountASK  <- printCustomRandomList takeamountASK
      -- generating prices for BIDS $$ amount
      amountBID <- printRandomList' takeamountBID

      let usdamountASK = amountASK  :: [Int] -- // convertion into []
      let usdamountBID  = amountBID :: [Int] -- // converting into []

      -- !! WALLS:
      -- \| Price walls (limit)
      -- generate the size of limit walls (in terms of it's occurrence)
      let totakefromwall  =  taketowalls $ 2 * takeamount

      -- generating walls, this is an infinite list
      pricewalls <- randomListwalls
      let pricewalllist = take totakefromwall pricewalls

      -- first part of the list above going to bids
      let pricesBids1 = firstPartList pricewalllist

      -- second part going to asks
      let pricesAsk1  = secondPartList pricewalllist

      -- full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
      fullwallsASK <- randomlyInsert pricesAsk1 (take takeamountASK usdamountASK)

      -- full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
      fullwallsBIDS <- randomlyInsert pricesBids1 (take takeamountBID usdamountBID)

      -- !! ADDING DATA TOGETHER
      -- \| adding orderbook together & generating additional data
      -- zipping so that we have orderwalls in  -> orderbook is built
      -- zipping prices with $ AMOUNT
      let orderbook_ask = zipToTuples setupASK fullwallsASK

      let orderbook_bid  = zipToTuples setupBID fullwallsBIDS

      -- the orderbook which should change the bid price
      fileBidBook <- readBook bidBookPath
      fileAskBook <- readBook askBookPath

      let bidBook =
            if isBidEmpty
              then orderbook_bid
              else fileBidBook
      -- the orderbook which should change the ask price


      let askBook =
            if isAskEmpty
              then orderbook_ask
              else fileAskBook

-- | price change
      volumesAndSides <- mainLoop initStats numberOfRuns
 -- putStrLn "\nVolumes and Sides:"
  -- Store the result in a variable before printing
      let listofvolumes = volumesAndSides

  -- Print the elements in a formatted way
--  forM_ result $ \(volume, side) -> do
--    putStrLn $ "Volume: " ++ show volume ++ ", Side: " ++ show side
      (finalBidBook, finalAskBook) <- recursiveList listofvolumes bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall

          -- ... do something with finalBidBook and finalAskBook, e.g., print them out ...

      let finaltxtpolish = removeEmptyLines pricePath
      finaltxtpolish
      mapM_ print listofvolumes
      print $ length listofvolumes

-- checking settings

      addsupto100 xProbabilityTaker yProbabilityTaker zProbabilityTaker fProbabilityTaker
      addsupto100 xProbabilityMaker yProbabilityMaker zProbabilityMaker fProbabilityMaker
      -- calling python script (graph)



      Control.Monad.when plotCharts $ callCommand "python App/PlotPrices.py"
