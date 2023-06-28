{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Generator where

import           Data.Aeson           (encode)

-- | module of utility funcitons
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (toList)
import qualified Data.Sequence        as Seq
import           System.Random        (RandomGen (split), randomRIO)

import           Data.Sequence        (Seq, fromList, (><))

-- | internal libraries
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib
import           PosCycle
import           RunSettings
import           Util
import Data.Maybe (fromMaybe)

{-

  putStrLn "Test output: \n\n\n"
  putStrLn "Liquidations: \n\n\n"
  print writeLiqInfo
  putStrLn "Positions: \n\n\n"
  print posinfo
  putStrLn "Longs: \n\n\n"
  print longinfo
  putStrLn "Shorts: \n\n\n"
  print shortinfo
  putStrLn "Book Details: \n\n\n"
  print bookDetails
  putStrLn "Position Stats: \n\n\n"
  print  posStats



-}
-- TODO
-- convert max min limit into two seprate functions
    -- TODO reverse and take out the head which is only zeros
-- TODO think about the order of the arguments
-- | there might be a need to reverse some data so the order is correct
data GenerationOutput = GenerationOutput
  { liqInfoOutput     :: Seq (Int, String, String)
  , posInfoOutput     :: (Seq (Int, String), Seq (Int, String))
  , longInfoOutput    :: Seq (Double, Int, String)
  , shortInfoOutput   :: Seq (Double, Int, String)
  , bidBookOutput     :: SeqOrderBook
  , askBookOutput     :: SeqOrderBook
  , bookDetailsOutput :: [BookStats]
  , posStatsOutput    :: [Stats]
  }

data GenerationPass = GenerationPass
  { initLiquidationAcc1Input        :: Seq (Int, String, String)
  , initLiquidationAcc2Input        :: Seq (Int, String, String)
  , initPositioningAccInput         :: (Seq (Int, String), Seq (Int, String))
  , initAccLongFutureInput          :: Seq (Double, Int, String)
  , initAccShortFutureInput         :: Seq (Double, Int, String)
  , listofvolumesInput              :: VolumeList
  , bidBookInput                    :: SeqOrderBook
  , askBookInput                    :: SeqOrderBook
  , gen1Input                       :: Generator
  , gen2Input                       :: Generator
  , fullwallsASKInput               :: FullWall
  , fullwallsBIDSInput              :: FullWall
  , initstartingPointInput          :: StartingPoint
  , inittotakefromwallInput         :: Totakefromwall
  , initialBookDetailsListInput     :: [BookStats]
  , initStatsInput                  :: [Stats]
  }

-- ? processor part
generaterunProgram ::
     GenerationPass
  -> IO  GenerationOutput
generaterunProgram genPass
  | null (listofvolumesInput genPass) = handleBaseCase genPass
  | otherwise = handleGeneralCase genPass


-- base case do block
handleBaseCase :: GenerationPass -> IO GenerationOutput
handleBaseCase genPass@GenerationPass{} = do
  let writeLiqInfo = initLiquidationAcc2Input genPass
  let posinfo = initPositioningAccInput genPass
  let longinfo = initAccLongFutureInput genPass
  let shortinfo = initAccShortFutureInput genPass
  let bidBook = bidBookInput genPass
  let askBook = askBookInput genPass
  let bookDetails = initialBookDetailsListInput genPass
  let posStats = initStatsInput genPass
  filewrites1 $ tail (reverse bookDetails)
  let writeBidBook = Book {book = toList bidBook}
  let writeAskBook = Book {book = toList askBook}
    --let writePositionFuture = Transaction { future = longinfo ++ shortinfo }
    --let writePositionFuture' = encode writePositionFuture
    --BL.writeFile posFutureP writePositionFuture'
  BL.writeFile bidBookP (encode writeBidBook)
  BL.writeFile askBookP (encode writeAskBook)
  return 
    GenerationOutput 
    { liqInfoOutput = writeLiqInfo
      , posInfoOutput = posinfo
    , longInfoOutput = longinfo
    , shortInfoOutput = shortinfo
    , bidBookOutput = bidBook
    , askBookOutput = askBook
    , bookDetailsOutput = bookDetails
    , posStatsOutput = posStats
    }

--    print writeLiqInfo
-- general case do block



handleGeneralCase :: GenerationPass -> IO GenerationOutput
handleGeneralCase genPass@GenerationPass{} = do
  let liqinfo = initLiquidationAcc1Input genPass
  let writeLiqInfo = initLiquidationAcc2Input genPass
  let posinfo = initPositioningAccInput genPass
  let longinfo = initAccLongFutureInput genPass
  let shortinfo = initAccShortFutureInput genPass
  let x:xs = listofvolumesInput genPass
  let bidBook = bidBookInput genPass
  let askBook = askBookInput genPass
  let gen1 = gen1Input genPass
  let gen2 = gen2Input genPass
  let fullwallsASK = fullwallsASKInput genPass
  let fullwallsBIDS = fullwallsBIDSInput genPass
  let sPoint = initstartingPointInput genPass
  let takeWall = inittotakefromwallInput genPass
  let bookDetails = initialBookDetailsListInput genPass
  let posStats = initStatsInput genPass
  orderbookLoop
    ( liqinfo
    , posinfo
    , longinfo
    , shortinfo
    , x
    , bidBook
    , askBook
    , gen1
    , gen2
    , fullwallsASK
    , fullwallsBIDS
    , sPoint
    , takeWall) >>= \(newliqinfo, newWriteLiqInfo, newPosInfo, newLonginfo, newShortinfo, newBidBook, newAskBook, newBookDetails, additionalVolAcc, newPosStats) -> do
    let (newGen1, newGen2) = (fst (split gen1), fst (split gen2))
    generaterunProgram
     GenerationPass  
      { initLiquidationAcc1Input = newliqinfo
      , initLiquidationAcc2Input =writeLiqInfo >< newWriteLiqInfo
      , initPositioningAccInput =newPosInfo
      , initAccLongFutureInput =newLonginfo
      , initAccShortFutureInput= newShortinfo
      , listofvolumesInput =additionalVolAcc ++ xs -- `++` should not be a significant performance bottleneck here
      , bidBookInput =newBidBook
      , askBookInput =newAskBook
      , gen1Input= newGen1
      , gen2Input =newGen2
      , fullwallsASKInput =fullwallsASK
      , fullwallsBIDSInput =fullwallsBIDS
      , initstartingPointInput =sPoint
      , inittotakefromwallInput =takeWall
      , initialBookDetailsListInput =newBookDetails : bookDetails
      , initStatsInput =newPosStats : posStats }







-- TODO concat the liquidations
orderbookLoop ::
     ListPass
  -> IO ( Seq (Int, String, String)
        , Seq (Int, String, String)
        , (Seq (Int, String), Seq (Int, String))
        , Seq (Double, Int, String)
        , Seq (Double, Int, String)
        , SeqOrderBook 
        , SeqOrderBook 
        , BookStats
        , VolumeList
        , Stats -- additional volume accumulator in case of liquidation
         )
orderbookLoop (liqinfo, posinfo, longinfo, shortinfo, (vAmount, vSide'), bidBook, askBook, gen1, gen2, fullwallsASK, fullwallsBIDS, sPoint, takeWall) = do
  let (volumeLIQ, sideLIQ, _) =
        case toList liqinfo of
          [] -> ([], [], [])
          _  -> unzip3 $ toList liqinfo
  let wholeLIQvolume = sum volumeLIQ
  let wholeLIQside =
        if head sideLIQ == "z"
          then Buy
          else Sell

  -- make into a function null volumeLIQ
  if null volumeLIQ
            -- | NO LIQUIDATION PROCESSING
    then do nullLiq
    else do notNullLiq
    
nullLiq :: IO ( Seq (Int, String, String)
        , Seq (Int, String, String)
        , (Seq (Int, String), Seq (Int, String))
        , Seq (Double, Int, String)
        , Seq (Double, Int, String)
        , SeqOrderBook 
        , SeqOrderBook 
        , BookStats
        , VolumeList
        , Stats -- additional volume accumulator in case of liquidation
         )
nullLiq = do
      orderBookGeneration <-
        orderBookProcess
          ( liqinfo
          , posinfo
          , longinfo
          , shortinfo
          , (vAmount, vSide')
          , bidBook
          , askBook
          , gen1
          , gen2
          , fullwallsASK
          , fullwallsBIDS
          , sPoint
          , takeWall)
      let (sPrice, finalBookAsk, finalBookBid, maxMinLmt, lengchngBid', lengchngAsk', listASK', listBID') =
            orderBookGeneration
      orderBookDetails <-
        additionalBookInfo finalBookAsk finalBookBid  vSide' vAmount  sPoint
        maxMinLmt takeWall  lengchngBid' lengchngAsk' listASK' listBID' sPrice
      
      let newbookDetails = orderBookDetails
      positionGenerator <-
        positionCycle sPrice (toList liqinfo) longinfo shortinfo vAmount posinfo
      let (newLiqInfo, newPositions, localPositions, newPosFutureLong, newPosFutureShort) =
            positionGenerator
      let nullLiqInfo =
            if null newLiqInfo
              then fromList [(0, "", "")]
              else newLiqInfo

      let newStats = aggregateStats localPositions initStats

      return
        ( newLiqInfo
        , nullLiqInfo
        , newPositions
        , newPosFutureLong
        , newPosFutureShort
        , finalBookBid
        , finalBookAsk
        , newbookDetails
        , []
        , newStats)
 



 -- Make into a function 
    





      let bookProcessInput =
            ( liqinfo
            , posinfo
            , longinfo
            , shortinfo
            , (wholeLIQvolume, wholeLIQside)
            , bidBook
            , askBook
            , gen1
            , gen2
            , fullwallsASK
            , fullwallsBIDS
            , sPoint
            , takeWall)
      orderBookGenerationLiquidation <- orderBookProcess bookProcessInput
      let (sPrice, finalBookAsk, finalBookBid, maxMinLmt, lengchngBid', lengchngAsk', listASK', listBID') =
            orderBookGenerationLiquidation
 
      orderBookDetailsLiquidation <-
        additionalBookInfo
          finalBookAsk
          finalBookBid
          wholeLIQside
          wholeLIQvolume
          sPoint
          maxMinLmt
          takeWall
          lengchngBid'
          lengchngAsk'
          listASK'
          listBID'
          sPrice
      let newbookDetails = orderBookDetailsLiquidation
      positionGeneratorLiquidation <-
        positionCycle
          sPrice
          (toList liqinfo)
          longinfo
          shortinfo
          wholeLIQvolume
          posinfo
      let (newLiqInfo, newPositions, localPositions, newPosFutureLong, newPosFutureShort) =
            positionGeneratorLiquidation

      let nullLiqInfo =
            if null newLiqInfo
              then fromList [(0, "", "")]
              else newLiqInfo
      let newStats = aggregateStats localPositions initStats -- //fix pass only the positions that are truly new
      return
        ( newLiqInfo
        , nullLiqInfo
        , newPositions
        , newPosFutureLong
        , newPosFutureShort
        , finalBookBid
        , finalBookAsk
        , newbookDetails
        , [(vAmount, vSide')]
        , newStats)


-- | orderbook main processing
orderBookProcess ::
     ListPass
  -> IO ( Double
        , SeqOrderBook
        , SeqOrderBook
        , [[Int]]
        , Int
        , Int
        , SeqOrderBook
        , SeqOrderBook)

orderBookProcess (_, _, _, _, (vAmount, vSide'), bidBook, askBook, gen1, gen2, fullwallsASK, fullwallsBIDS, _, _)
            

            -- TODO implement sequencing
-- | local variables
 = do
  let (volumeBID, volumeASK) = calculateVolumes vSide' vAmount
 
  let (bidUpdateBook, askUpdateBook) = -- Sequenced
        calculateBooks volumeBID volumeASK bidBook askBook
            -- | how much volume took from certain order books
  
  let (lengchngAsk', lengchngBid') =
        lengthChanges bidUpdateBook bidBook askUpdateBook askBook
  let sPrice = startingPrices vSide' bidUpdateBook askUpdateBook
  let (askSetupInsert, bidSetupInsert) =
        calculateSetupInserts lengchngAsk' lengchngBid' sPrice gen1 gen2
  let maxMinLmt :: [[Int]] = [fullwallsASK, fullwallsBIDS]
  pricesASK <- printCustomRandomList lengchngAsk'
  pricesBID <- printRandomList' lengchngBid'
            -- | the / number is how smaller the insertion will be
  let (listASK', listBID') =
        calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID -- TODO rename the calculateListTuples
            -- //TODO, possible microoptimization with the stuff below :
            -- | let insertInAsk = if vSide == Buy then [] else listASK
            --  / let insertInBid = if vSide == Sell then [] else listBID    // --
  let (finalBookAsk, finalBookBid) =
        calculateFinalBooks
          vSide'
          askUpdateBook
          listASK'
          askBook
          bidUpdateBook
          listBID'
          bidBook
  return
    ( sPrice
    , finalBookAsk
    , finalBookBid
    , maxMinLmt
    , lengchngBid'
    , lengchngAsk'
    , listASK'
    , listBID')

--type Price = Double

-- | continuing orderbook processing, with additional data
additionalBookInfo ::
     SeqOrderBook
  -> SeqOrderBook
  -> VolumeSide
  -> Int
  -> StartingPoint
  -> [[Int]]
  -> Totakefromwall
  -> Int
  -> Int
  -> SeqOrderBook
  -> SeqOrderBook
  -> Double
  -> IO BookStats
additionalBookInfo finalBookAsk finalBookBid vSide' vAmount sPoint maxMinLmt takeWall lengchngBid' lengchngAsk' listASK' listBID' sPrice
            
-- TODO make funciton purely for this called additional book info
            
-- | ask in total in terms of count
            -- | bids in total in terms of count
 = do
  let (asktotal, bidtotal) = calculateTotalsCount finalBookAsk finalBookBid
            -- | Ratio between bids and AKS
  let bidAskR' = abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
            -- | Ratio is benefiting :
            -- | asks in total -> $$
            -- | bids in total -> $$
  let (asksTot', bidsTot') = calculateTotals finalBookAsk finalBookBid
            -- | bid ask spread
  let (firstelemASK, firstelemBID) =
        calculateFirstElements finalBookAsk finalBookBid
  let sprd' = spread' firstelemASK firstelemBID
            -- | checking if the stats above will go through with the orderbook
            -- | local check
  let check = settingcheck vSide' vAmount asksTot' bidsTot'
  check

            -- | how accumulator stores the values
  let newbookDetails =
        setupBookDetails
          ( sPoint
          , maxMinLmt
          , asksTot'
          , bidsTot'
          , takeWall
          , lengchngBid'
          , lengchngAsk'
          , listASK'
          , listBID'
          , vSide'
          , vAmount
          , sprd'
          , sPrice
          , bidAskR')
  let insertinInfo = formatAndPrintInfo newbookDetails
  insertinInfo -- THIS IS ONLY CONSOLE (pass into the console)
  return newbookDetails

-- ! add records as data types
type SeqNewPositioning = Seq NewPositioning


-- | position cycle 
-- ! does not need to recive liquidation info
positionCycle ::
     Double
  -> MarginCall
  -> Seq (Double, Int, String)
  -> Seq (Double, Int, String)
  -> Int
  -> (Seq (Int, String), Seq (Int, String))
  -> IO ( Seq (Int, String, String) -- [(Int, String, String)]
        , (Seq (Int, String), Seq (Int, String)) -- (Seq (Int, String), Seq (Int, String))
        , NewPositioning -- NewPositioning
        , Seq (Double, Int, String)
        , Seq (Double, Int, String) -- Seq (Double, Int, String), Seq (Double, Int, String))
         )-- TODO: maybe convert to record?

positionCycle sPrice liqinfo longinfo shortinfo vAmount posinfo = do
  let (_, sideLIQ, _) =
        case liqinfo of
          [] -> ([], [], [])
          _  -> unzip3 liqinfo
  let liquidationString =
        if null sideLIQ
          then ""
          else head sideLIQ
      
-- TODO make a funciton purely for this called position info
      
-- ? POSITION FUTURE
      -- | check if the future file is empty
  numTakers <- randomRIO (1, maxTakers) :: IO Int -- select how many takers
  numMakers <- randomRIO (1, maxMakers) :: IO Int -- select how many makers
  volumeSplitT <- generateVolumes numTakers vAmount -- split the volume
  volumeSplitM <- generateVolumes numMakers vAmount -- split the volume
  liquidated <- liquidationDuty longinfo shortinfo sPrice
      --   print "infos"
      
--   print longinfo
  let (liquidationIO, liquidationFuture) = liquidated
  let (longliq, shortliq) = liquidationFuture
  let newLiqInfo = liquidationIO
  runProgram <-
    normalrunProgram
      (volumeSplitT, volumeSplitM)
      (longliq, shortliq)
      sPrice
      liquidationString
  let ((newPosFutureShort, newPosFutureLong), newPositions) = runProgram
      -- | (TakerTuple,MakerTuple)
      -- TODO convert into seq and keep it until base case
  let updatedPositionAcc =
        let ((taker1, maker1), (taker2, maker2)) = (newPositions, posinfo)
            takerSeq1 = Seq.fromList taker1
            takerSeq2 = taker2
            makerSeq1 = Seq.fromList maker1
            makerSeq2 = maker2
         in (takerSeq1 Seq.>< takerSeq2, makerSeq1 Seq.>< makerSeq2)
      

  return
    ( newLiqInfo
    , updatedPositionAcc
    , newPositions
    , newPosFutureLong
    , newPosFutureShort)
