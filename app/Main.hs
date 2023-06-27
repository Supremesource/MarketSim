{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Main where
  -- TODO
    -- | display in charts using elm/ javascript/ https/ css
    -- | make frontend more efficient the way the python script is called now is just terrible+
    -- | better UI with timeframes
    -- | fix negative orderbook levels on bids by stopping ask generation at that point as well, (probable stop ask gen at that point)
  -- TODO


-- imports
-- | external libraries
import           Control.Monad
import           Control.Parallel.Strategies (parList, rseq, using)
import           System.IO                   (BufferMode (LineBuffering),
                                              hSetBuffering, stdout)

--import           System.Process              (callCommand)
import           Data.Sequence               (fromList)
import           System.Random               (Random (randomRs))


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

main :: IO ()
main = initialSetup


--  Entry points of program
runProgram :: IO ()
runProgram = do
  isBidEmpty <- isFileEmpty bidBookP
  isAskEmpty <- isFileEmpty askBookP
  initstartingPoint <- startingPointFromFile initPriceP
  fileBidBook <- readBook bidBookP
  fileAskBook <- readBook askBookP
             --  CHECKING SETTINGS
  localCheck
            --  RANDOM GENERATORS:
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
    fileBidBook
    fileAskBook
    gen1
    gen2
    fullwallsASK
    fullwallsBIDS
    initstartingPoint
    inittotakefromwall
             -- WARININGS of what might be affest the quality of the settings
  warnings

runProgramProgramHelp :: Stats -> Int -> IO [(Int, VolumeSide)]
runProgramProgramHelp aggregatedStats remainingrunPrograms = do
  mainLoop aggregatedStats remainingrunPrograms []


-- | loop initializing the main processes
mainLoop :: Stats -> Int -> [(Int, Position)] -> IO [(Int, VolumeSide)]
mainLoop aggregatedStats remainingrunPrograms accumulatedStats = do
  if remainingrunPrograms > 0
    then do
      positions <-
        forM [1 .. numPositions] $ \indexPosition -> do
          randomToTempleate <- randomOptionGen
          generateRandomPosition
            (processTempleaterunProgram indexPosition randomToTempleate)
      let newAggregatedStats =
            foldl (flip aggregateStats) aggregatedStats positions
      let newAccumulatedStats
        -- TODO check if conncat is safe here
           = accumulatedStats ++ zip [1 ..] positions
      mainLoop newAggregatedStats (remainingrunPrograms - 1) newAccumulatedStats
    else do
      noRemainingrunProgram aggregatedStats accumulatedStats

noRemainingrunProgram :: Stats -> [(Int, Position)] -> IO [(Int, VolumeSide)]
noRemainingrunProgram aggregatedStats accumulatedStats = do
  printFinal aggregatedStats
      
-- Accumulate all the results first
  results <-
    forM accumulatedStats $ \(indexPosition, positionInfo) -> do
      (volume, side, acc) <-
        printPositionStats indexPosition positionInfo initialPositionData
      return (volume, side, acc)
      
-- Now write everything to the file at once
  let allPositions = concatMap (\(_, _, acc) -> acc) results
  writePositionsToFile positionInfoP allPositions
      
-- Return the results, discarding the [PositionData] part
  return [(volume, side) | (volume, side, _) <- results]

generator ::
     Bool
  -> Bool
  -> [(Double, Int)]
  -> [(Double, Int)]
  -> [(Double, Int)]
  -> [(Double, Int)]
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
  volumesAndSides <- runProgramProgramHelp initStats numberOfrunPrograms
  let initialBookDetailsList = [initialBookDetails]
  let listofvolumes = volumesAndSides
  isFutureEmpt <- isFutureEmpty
           --  print isFutureEmpt
  initAccLongFuture <-
    if isFutureEmpt
      then return futureAccLong
      else do
        filterFuture "f" <$> readFuture
  initAccShortFuture <-
    if isFutureEmpt
      then return futureAccLong
      else do
        filterFuture "z" <$> readFuture
  (_, _, _, _, _, _, _, _) <-
    generaterunProgram
      ( initLiquidationAcc
      , initLiquidationAcc
      , initPositioningAcc
      , fromList initAccLongFuture
      , fromList initAccShortFuture
      , listofvolumes
      , bidBook
      , askBook
      , gen1
      , gen2
      , fullwallsASK
      , fullwallsBIDS
      , initstartingPoint
      , inittotakefromwall
      , initialBookDetailsList
      , [initStats])
            -- | formating price document
       --      removeEmptyLines pricePath
  putStrLn $ gray "OUTPUT SUCCESFULLY GENERATED"

initialSetup :: IO ()
initialSetup = do
  -- | clening log file before every runProgram
  writeFile logP ""
  -- | optimizing the IO to be formated in lines
  hSetBuffering stdout LineBuffering
  -- | Asking user to proceed
  putStrLn $
    cyan $
    "Proceed (_ / n)" ++ gray "\n\n { for runProgram-restore press - (w) }"
  proceed <- getLine
  -- | WIPING runProgram == TRUE
  -- | When wiping runProgram is runProgramning the whole code is not evaluated
  -- | Wiping all of the text files, and changing the starting point
  if proceed == "W" || proceed == "w"
    -- | user wants to proceed with the simulation generation
    -- | checking settings, catching potential bugs in the setting specified by user
    -- | if the settings are not correct, the program will not runProgram
    -- | CHECKING IF FILES ARE EMPTY
    then do
      let sayStart = show wipingStartingValue
      putStrLn $ purple "YOU DELETED OUTPUT & DATA FILES"
      putStr $ gray "\nnew starting value will be set to: $"
      putStrLn $ purple $ show sayStart
      putStrLn $
        gray
          "\n { you can adjsut starting value in the '..Settings/runProgramSetting' } "
      newrunProgramSettings
        askBookP
        bidBookP
        logP
        orderBookDetailsP
        positionInfoP
        initPriceP
        posFutureP
        wipingStartingValue
    else if proceed == "n" || proceed == "N"
           then error (red "stopping program")
           else runProgram


-- checking settings, so bugs are caught before the program is runProgramning
localCheck :: IO ()
localCheck = do
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
  positionamountcheck minvolume 1 -- TODO take out this 1

warnings :: IO ()
warnings
       -- | optional warnings
 = do
  addsupto100 buyTakerProb sellTakerProb

orderBook ::
     Double
  -> StdGen
  -> StdGen
  -> IO ([(Double, Int)], [(Double, Int)], [Int], [Int], Int)
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
        (zipToTuples setupASK fullwallsASK, zipToTuples setupBID fullwallsBIDS)
             
-- | the orderbook path which should change the bid price
             -- | orderbook logc:
  return
    ( orderbook_ask
    , orderbook_bid
    , fullwallsASK
    , fullwallsBIDS
    , inittotakefromwall)
{-
            -- | calling python script (graph)
            --  TODO make this way more effective, calling the script below
             Control.Monad.when plotCharts $
               callCommand "python scripts/plot_prices.py"
-}
