{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where
  -- TODO
    -- | display in charts using elm/ javascript/ https/ css
    -- | make frontend more efficient the way the python script is called now is just terrible+
    -- | better UI with timeframes
  -- TODO

-- imports
-- | external libraries
import           Control.Monad               (forM, when)
import           Control.Parallel.Strategies (parList, rseq, using)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)
import           System.Process              (callCommand)
import           System.Random               (Random (randomRs))

-- | internal libraries
import           Colours
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib
import           RunSettings
import           Statistics
import           Util


-- | Entry point of program
runProgram :: Stats -> Int -> IO [(Int, VolumeSide)]
runProgram aggregatedStats remainingRuns = do
  handles <- openRewrites3
  result <- mainLoop aggregatedStats remainingRuns handles
  closeHandles3 handles
  return result

-- | loop initializing the main processes
mainLoop :: Stats -> Int -> RewriteHandle3 -> IO [(Int, VolumeSide)]
mainLoop aggregatedStats remainingRuns handles = do
  if remainingRuns > 0
    then do
      putStrLn "+------------+"
      putStrLn $ "|RUN ID   " ++ show remainingRuns
      putStrLn "+------------+"
      positions <-
        forM [1 .. numPositions] $ \indexPosition -> do
          randomToTempleate <- randomOptionGen
          generateRandomPosition
            (processTempleateRun indexPosition randomToTempleate)
      let newAggregatedStats =
            foldl (flip aggregateStats) aggregatedStats positions
      volumesAndSides <-
        forM (zip [1 ..] positions) $ \(indexPosition, positionInfo) -> do
          (volume, side) <-
            printPositionStats handles indexPosition positionInfo
          return (volume, side)
      putStrLn "+------------+"
      putStrLn $ "|END OF RUN " ++ show remainingRuns
      putStrLn "+------------+\n\n"
      putStrLn "+------------------------------------+"
      putStrLn $
        "|RUN: " ++ show remainingRuns ++ " CONTENTS & aggregatedStats" ++ "|"
      putStrLn "+------------------------------------+\n"
      printStats newAggregatedStats
      nextVolumesAndSides <-
        mainLoop newAggregatedStats (remainingRuns - 1) handles
      return (volumesAndSides ++ nextVolumesAndSides)
    else do
      printFinal aggregatedStats
      return []

-- | "indexPosition" is an index showing the number of that particular position
-- | IO ()
-- | initilizing the number of positions
-- | generating random number for the template / list described in settings
-- | generating positioning
-- | generating the stats for the positions
-- | initilizing the positioning
-- | returns the volume and side of the position
-- | IO ()
-- | overal stats
-- | initilizing the counter
-- | returning the volume and side of the position, for further processing in orderbook
-- | if the number of runs is reached
main :: IO ()
main = run

-- | main function
-- TODO run function
run :: IO ()
run
 -- ? IO
 -- | clening log file before every run
 = do
  writeFile logPath ""
  -- | optimizing the IO to be formated in lines
  hSetBuffering stdout LineBuffering
  -- | Asking user to proceed
  putStrLn $
    cyan $ "Proceed (_ / n)" ++ gray "\n\n { for run-restore press - (w) }"
  proceed <- getLine
  -- | WIPING RUN == TRUE
  -- | When wiping run is running the whole code is not evaluated
  -- | Wiping all of the text files, and changing the starting point
  if proceed == "W" || proceed == "w"
    then do
      let sayStart = show wipingStartingValue
      putStrLn $ purple "YOU DELETED OUTPUT & DATA FILES"
      putStr $ gray "\nnew starting value will be set to: $"
      putStrLn $ purple $ show sayStart
      putStrLn $
        gray
          "\n { you can adjsut starting value in the '..Settings/RunSetting' } "
      newRunSettings
        logPath
        bidBookPath
        askBookPath
        pricePath
        newLongsPath
        newShortsPath
        exitLongsPath
        exitShortsPath
        bidAskRPath
        bidToAskRPath
        buyVolumePath
        sellVolumePath
        volumePath
        openInterestPath
        wipingStartingValue
    else if proceed == "n" || proceed == "N"
           then error (red "stopping program")
           -- | user wants to proceed with the simulation generation
           -- | checking settings, catching potential bugs in the setting specified by user
           -- | if the settings are not correct, the program will not run
           -- | CHECKING IF FILES ARE EMPTY
           else do
   
             isBidEmpty <- isFileEmpty bidBookPath
             isAskEmpty <- isFileEmpty askBookPath
             initstartingPoint <- startingPointFromFile pricePath
             fileBidBook <- readBook bidBookPath
             fileAskBook <- readBook askBookPath
             -- ? CHECKING SETTINGS
             volumechecker
               minvolume
               basecaseValueLongNew
               basecaseValueShortNew
               basecaseValueLongClose
               basecaseValueShortClose
               upperBoundLongNew
               upperBoundShortNew
               upperBoundLongClose
               upperBoundShortClose
             positionamountcheck minvolume maxMakers
            -- ? RANDOM GENERATORS:
             gen1 <- randomGen
             gen2 <- randomGen
             -- ! - ORDERBOOK - ! --
             -- | the price simulation is starting at
             -- | orderbook
             -- | making ask move upside
             let upMoves =
                   take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
             -- | making bid move downside
             let downMoves =
                   take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
             -- | liquidity  for ask, the limit setup gradient
             -- | grid of the orderbook
             let setupASK =
                   take
                     takeamountASK
                     (tail (infiniteList initstartingPoint gen1 upMoves)) `using`
                   parList rseq
             -- | liquidity for ask, the limit setup gradient
             let setupBID =
                   take
                     takeamountBID
                     (tail (infiniteListDown initstartingPoint gen2 downMoves)) `using`
                   parList rseq
            -- | generating prices for ASKS $$ amount
             amountASK <- printCustomRandomList takeamountASK
             -- | generating prices for BIDS $$ amount
             amountBID <- printRandomList' takeamountBID
             -- | adding into a list / converting into []
             let usdamountASK = amountASK :: [Int]
             let usdamountBID = amountBID :: [Int]
             -- | Price walls (limit)
             -- | generate the size of limit walls (in terms of it's occurrence)
             let inittotakefromwall = taketowalls $ 2 * takeamount
             -- | generating walls, this is an infinite list
             pricewalls <- randomListwalls
             let pricewalllist = take inittotakefromwall pricewalls
             -- | first part of the list above going to bids
             let pricesBids1 = firstPartList pricewalllist
             -- | second part going to asks
             let pricesAsk1 = secondPartList pricewalllist
             -- | full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
             fullwallsASK <-
               randomlyInsert pricesAsk1 (take takeamountASK usdamountASK)
             -- | full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
             fullwallsBIDS <-
               randomlyInsert pricesBids1 (take takeamountBID usdamountBID)
             -- ? ADDING DATA TOGETHER
             -- | adding orderbook together & generating additional data
             -- |zipping so that we have orderwalls in  -> orderbook is built
             -- | zipping prices with $ AMOUNT
             let orderbook_ask = zipToTuples setupASK fullwallsASK
             let orderbook_bid = zipToTuples setupBID fullwallsBIDS
             -- | the orderbook path which should change the bid price
             -- | orderbook logc:
             let bidBook =
                   if isBidEmpty
                     then orderbook_bid
                     else fileBidBook
             -- |  ask
             let askBook =
                   if isAskEmpty
                     then orderbook_ask
                     else fileAskBook

             -- ? ADDING STATS FROM 'MAINLOOP' TOGETHER  
             -- | price change
             volumesAndSides <- runProgram initStats numberOfRuns
             let initialBookDetailsList = [initialBookDetails]
             let listofvolumes = volumesAndSides
          
             (_, _, _) <-
               recursiveList
                 ( listofvolumes
                 , bidBook
                 , askBook
                 , gen1
                 , gen2
                 , fullwallsASK
                 , fullwallsBIDS
                 , initstartingPoint
                 , inittotakefromwall
                 , initialBookDetailsList)
        

            
             -- | optional warnings
             addsupto100
               xProbabilityTaker
               yProbabilityTaker
               zProbabilityTaker
               fProbabilityTaker
             addsupto100
               xProbabilityMaker
               yProbabilityMaker
               zProbabilityMaker
               fProbabilityMaker
             
              
            -- | formating price document
             removeEmptyLines pricePath
             putStrLn $ gray "OUTPUT SUCCESFULLY GENERATED"

            -- // testing :
             print $ "List of Vol: \n" ++ show listofvolumes
             -- print takerX stats

            -- | calling python script (graph)
            --  TODO make this way more effective, calling the script belowú§
             Control.Monad.when plotCharts $
               callCommand "python scripts/plot_prices.py"
