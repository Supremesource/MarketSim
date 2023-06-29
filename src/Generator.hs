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

  -- / ACTION
  -- ? OUTPUT
  writeLog  $ tail (reverse bookDetails)
  writeBook $ tail (reverse bookDetails)

  -- ? DATA DO NOT TOUCH
  -- | / rewriting orderbooks
  let writeBidBook = Book {book = toList bidBook}
  let writeAskBook = Book {book = toList askBook}
  --let writePositionFuture = Transaction { future = longinfo ++ shortinfo }
  --let writePositionFuture' = encode writePositionFuture
  --BL.writeFile posFutureP writePositionFuture'
  BL.writeFile bidBookP (encode writeBidBook)
  BL.writeFile askBookP (encode writeAskBook)
  
  

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

  
  return
    GenerationOutput
    {
        liqInfoOutput = writeLiqInfo
      , posInfoOutput = posinfo
      , longInfoOutput = longinfo
      , shortInfoOutput = shortinfo
      , bidBookOutput = bidBook
      , askBookOutput = askBook
      , bookDetailsOutput = bookDetails
      , posStatsOutput = posStats
    }

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
    ListPass
    {   liqinfoInpt          = liqinfo
      , posinfoInpt          = posinfo
      , longinfoInpt         = longinfo
      , shortinfoInpt        = shortinfo
      , volumeInpt           = x
      , bidBookInpt          = bidBook
      , askBookInpt          = askBook
      , gen1Inpt             = gen1
      , gen2Inpt             = gen2
      , fullwallsASKInpt     = fullwallsASK
      , fullwallsBIDSInpt    = fullwallsBIDS
      , sPointInpt           = sPoint
      , takeWallInpt         = takeWall } >>= \ ReturnData   
   
    {   newliqinfoOutpt      = newliqinfo
      , newWriteLiqInfoOutpt = newWriteLiqInfo
      , newPosInfoOutpt      = newPosInfo
      , newLonginfoOutpt     = newLonginfo
      , newShortinfoOutpt    = newShortinfo
      , newBidBookOutpt      = newBidBook
      , newAskBookOutpt      = newAskBook
      , newBookDetailsOutpt  = newBookDetails
      , additionalVolAccOutpt= additionalVolAcc
      , newPosStatsOutpt     = newPosStats } -> do

    let (newGen1, newGen2)   = (fst (split gen1), fst (split gen2))
    generaterunProgram

     GenerationPass
     {  initLiquidationAcc1Input = newliqinfo
      , initLiquidationAcc2Input =writeLiqInfo >< newWriteLiqInfo
      , initPositioningAccInput =newPosInfo
      , initAccLongFutureInput =newLonginfo
      , initAccShortFutureInput= newShortinfo
       -- ? `++` should not be a significant performance bottleneck here
      , listofvolumesInput = additionalVolAcc ++ xs
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

data ListPass = ListPass
    { liqinfoInpt       :: Seq (Int, String, String)
    , posinfoInpt       :: (Seq (Int, String), Seq (Int, String))
    , longinfoInpt      :: Seq (Double, Int, String)
    , shortinfoInpt     :: Seq (Double, Int, String)
    , volumeInpt        :: Volume
    , bidBookInpt       :: SeqOrderBook
    , askBookInpt       :: SeqOrderBook
    , gen1Inpt          :: Generator
    , gen2Inpt          :: Generator
    , fullwallsASKInpt  :: FullWall
    , fullwallsBIDSInpt :: FullWall
    , sPointInpt        :: StartingPoint
    , takeWallInpt      :: Totakefromwall
    } 
  
data ReturnBook = ReturnData
    { newliqinfoOutpt       :: Seq (Int, String, String)
    , newWriteLiqInfoOutpt  :: Seq (Int, String, String)
    , newPosInfoOutpt       :: (Seq (Int, String), Seq (Int, String))
    , newLonginfoOutpt      :: Seq (Double, Int, String)
    , newShortinfoOutpt     :: Seq (Double, Int, String)
    , newBidBookOutpt       :: SeqOrderBook
    , newAskBookOutpt       :: SeqOrderBook
    , newBookDetailsOutpt   :: BookStats
    , additionalVolAccOutpt :: VolumeList
    , newPosStatsOutpt      :: Stats
    }


-- TODO concat the liquidations
orderbookLoop :: ListPass -> IO ReturnBook
orderbookLoop listPass@ListPass{} = do
  let liqinfo =
        liqinfoInpt listPass

  let (volumeLIQ, sideLIQ, _) =
        case toList liqinfo of
          [] -> ([], [], [])
          _  -> unzip3 $ toList liqinfo
  let wholeLIQvolume = sum volumeLIQ
  let wholeLIQside =
        if head sideLIQ == "z"
          then Buy
          else Sell

  volLiqProcessing listPass (wholeLIQvolume,wholeLIQside)

volLiqProcessing :: ListPass -> (Int,VolumeSide) -> IO ReturnBook
volLiqProcessing listPass@ListPass{} (wholeLIQvolume, wholeLIQside)  = do
  let (liqinfo,posinfo,longinfo,shortinfo,(vAmount,vSide'),sPoint,takeWall) =
        (liqinfoInpt listPass,
        posinfoInpt listPass,
        longinfoInpt listPass,
        shortinfoInpt listPass,
        volumeInpt listPass,
        sPointInpt listPass,
        takeWallInpt listPass)

  orderBookGeneration <- orderBookProcess listPass

  let ReturnBookProcess
        { sPrice = sPriceGenerated
        , finalBookAsk = finalBookAskGenerated
        , finalBookBid = finalBookBidGenerated
        , maxMinLmt    = maxMinLmtGenerated
        , lengchngBid' = lengchngBidGenerated
        , lengchngAsk' = lengchngAskGenerated
        , listASK'     = listASKGenerated
        , listBID'     = listBIDGenerated } = orderBookGeneration
  let adjustedVolAmount= if wholeLIQvolume == 0 then vAmount else wholeLIQvolume
  let adjustedVolSide  = if wholeLIQvolume == 0 then vSide' else wholeLIQside

  orderBookDetails <- additionalBookInfo
                 AdditionData
                            { finalBookAskInput     = finalBookAskGenerated
                              ,finalBookBidInput    = finalBookBidGenerated
                              ,vSide'Input          = adjustedVolSide
                              ,vAmountInput         = adjustedVolAmount
                              ,sPointInput          = sPoint
                              ,maxMinLmtInput       = maxMinLmtGenerated
                              ,takeWallInput        = takeWall
                              ,lengchngBid'Input    = lengchngBidGenerated
                              ,lengchngAsk'Input    = lengchngAskGenerated
                              ,listASK'Input        = listASKGenerated
                              ,listBID'Input        = listBIDGenerated
                              ,sPriceInput          = sPriceGenerated }
  positionGenerator <- positionCycle
            PositionCyclePass
                            {  sPriceInputCycle     = sPriceGenerated
                              , liqinfoInputCycle   = toList liqinfo
                              , longinfoInputCycle  = longinfo
                              , shortinfoInputCycle = shortinfo
                              , vAmountInputCycle   = adjustedVolAmount
                              , posinfoInputCycle   = posinfo }
    
  let PositionCycleOutput
         { newLiqInfo        = newLiqInfoGenerated
          ,nullLiqInfo       = newPositionsGenerated
          ,newPositions      = localPositions
          ,newPosFutureLong  = newPosFutureLongGenerated
          ,newPosFutureShort = newPosFutureShortGenerated  } = positionGenerator
  let nullLiqInfoGenerated =
        if null newLiqInfoGenerated
        then fromList [(0, "", "")]
        else newLiqInfoGenerated
  let newStats = aggregateStats localPositions initStats
  let addVolAcc = ([(vAmount, vSide') | wholeLIQvolume /= 0])
  let newbookDetails = orderBookDetails

  return
               ReturnData
                        {   newliqinfoOutpt       = newLiqInfoGenerated
                          , newWriteLiqInfoOutpt  = nullLiqInfoGenerated
                          , newPosInfoOutpt       = newPositionsGenerated
                          , newLonginfoOutpt      = newPosFutureLongGenerated
                          , newShortinfoOutpt     = newPosFutureShortGenerated
                          , newBidBookOutpt       = finalBookBidGenerated
                          , newAskBookOutpt       = finalBookAskGenerated
                          , newBookDetailsOutpt   = newbookDetails
                          , additionalVolAccOutpt = addVolAcc
                          , newPosStatsOutpt      = newStats  }

data ReturnBookProcess
  = ReturnBookProcess
      { sPrice       :: Double
      , finalBookAsk :: SeqOrderBook
      , finalBookBid :: SeqOrderBook
      , maxMinLmt    :: (Int,Int)
      , lengchngBid' :: Int
      , lengchngAsk' :: Int
      , listASK'     :: SeqOrderBook
      , listBID'     :: SeqOrderBook
      }
-- | orderbook main processing
orderBookProcess ::  ListPass -> IO ReturnBookProcess
orderBookProcess listPass@ListPass{}  = do
  let ((vAmount,vSide'),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS)  =
        ( volumeInpt listPass,
        bidBookInpt listPass,
        askBookInpt listPass,
        gen1Inpt listPass,
        gen2Inpt listPass,
        fullwallsASKInpt listPass,
        fullwallsBIDSInpt listPass )  
-- | local variables
  let (volumeBID, volumeASK) = calculateVolumes vSide' vAmount
  let (bidUpdateBook, askUpdateBook) = -- Sequenced
        calculateBooks volumeBID volumeASK bidBook askBook
            -- | how much volume took from certain order books
  let (lengchngAskGenerated, lengchngBidGenerated) =
        lengthChanges bidUpdateBook bidBook askUpdateBook askBook
  let sPriceGenerated = startingPrices vSide' bidUpdateBook askUpdateBook
  let (askSetupInsert, bidSetupInsert) =
        calculateSetupInserts 
        lengchngAskGenerated 
        lengchngBidGenerated 
        sPriceGenerated 
        gen1 gen2
  let maxMinLmtGenerated :: [[Int]] = [fullwallsASK, fullwallsBIDS]
  let maxMinLmtTuple = (  fromMaybe 0 $ maxList maxMinLmtGenerated  
                        , fromMaybe 0 $ minList maxMinLmtGenerated )

  pricesASK <- printCustomRandomList lengchngAskGenerated
  pricesBID <- printRandomList' lengchngBidGenerated
            -- | the / number is how smaller the insertion will be
  let (listASKGenerated, listBIDGenerated) =
        calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID -- TODO rename the calculateListTuples
            -- //TODO, possible microoptimization with the stuff below :
            -- | let insertInAsk = if vSide == Buy then [] else listASK
            --  / let insertInBid = if vSide == Sell then [] else listBID    // --
  let (finalBookAskGenerated, finalBookBidGenerated) =
        calculateFinalBooks
          vSide'
          askUpdateBook
          listASKGenerated
          askBook
          bidUpdateBook
          listBIDGenerated
          bidBook

  return
    ReturnBookProcess
    {     sPrice       = sPriceGenerated
        , finalBookAsk = finalBookAskGenerated
        , finalBookBid = finalBookBidGenerated
        , maxMinLmt    = maxMinLmtTuple
        , lengchngBid' = lengchngBidGenerated
        , lengchngAsk' = lengchngAskGenerated
        , listASK'     = listASKGenerated
        , listBID'     = listBIDGenerated  }

data AdditionalBook = AdditionData
      {
          finalBookAskInput :: SeqOrderBook
        , finalBookBidInput :: SeqOrderBook
        , vSide'Input       :: VolumeSide
        , vAmountInput      :: Int
        , sPointInput       :: StartingPoint
        , maxMinLmtInput    :: (Int,Int)
        , takeWallInput     :: Totakefromwall
        , lengchngBid'Input :: Int
        , lengchngAsk'Input :: Int
        , listASK'Input     :: SeqOrderBook
        , listBID'Input     :: SeqOrderBook
        , sPriceInput       :: Double
      }
-- | continuing orderbook processing, with additional data
additionalBookInfo :: AdditionalBook -> IO BookStats
additionalBookInfo additionalBook@AdditionData{} = do
  let (finalBookAskGen, finalBookBidGen, vSide', vAmount, sPoint, maxMinLmtGen
       , takeWall , lengchngBidGen, lengchngAskGen, listASKGen
       , listBIDGen, sPriceGen) =
        (finalBookAskInput additionalBook,
        finalBookBidInput additionalBook,
        vSide'Input additionalBook,
        vAmountInput additionalBook,
        sPointInput additionalBook,
        maxMinLmtInput additionalBook,
        takeWallInput additionalBook,
        lengchngBid'Input additionalBook,
        lengchngAsk'Input additionalBook,
        listASK'Input additionalBook,
        listBID'Input additionalBook,
        sPriceInput additionalBook)
-- | ask in total in terms of count
            -- | bids in total in terms of count
  let (asktotal, bidtotal) = calculateTotalsCount finalBookAskGen finalBookBidGen
            -- | Ratio between bids and AKS
  let bidAskR' = abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
            -- | Ratio is benefiting :
            -- | asks in total -> $$
            -- | bids in total -> $$
  let (asksTot', bidsTot') = calculateTotals finalBookAskGen finalBookBidGen
            -- | bid ask spread
  let (firstelemASK, firstelemBID) =
        calculateFirstElements finalBookAskGen finalBookBidGen
  let sprd' = spread' firstelemASK firstelemBID
            -- | checking if the stats above will go through with the orderbook
            -- | local check
  let check = settingcheck vSide' vAmount asksTot' bidsTot'
  check
            -- | how accumulator stores the values
  let newbookDetails =
        setupBookDetails
          ( sPoint
          , maxMinLmtGen
          , asksTot'
          , bidsTot'
          , takeWall
          , lengchngBidGen
          , lengchngAskGen
          , listASKGen
          , listBIDGen
          , vSide'
          , vAmount
          , sprd'
          , sPriceGen
          , bidAskR')
  
  return newbookDetails

data PositionCyclePass = PositionCyclePass
  {   sPriceInputCycle    :: Double
    , liqinfoInputCycle   :: MarginCall
    , longinfoInputCycle  :: Seq (Double, Int, String)
    , shortinfoInputCycle :: Seq (Double, Int, String)
    , vAmountInputCycle   :: Int
    , posinfoInputCycle   :: (Seq (Int, String), Seq (Int, String))
  }

data PositionCycleOutput = PositionCycleOutput
  { newLiqInfo        :: Seq (Int, String, String)
  , nullLiqInfo       :: (Seq (Int, String), Seq (Int, String))
  , newPositions      :: NewPositioning
  , newPosFutureLong  :: Seq (Double, Int, String)
  , newPosFutureShort :: Seq (Double, Int, String)
  } deriving (Eq)

-- TODO convert into records
-- | position cycle 
-- ! does not need to recive liquidation info
positionCycle :: PositionCyclePass -> IO PositionCycleOutput
positionCycle positionCyclePass@PositionCyclePass{} = do
  let (sPricegen, liqinfo, longinfo, shortinfo, vAmount, posinfo) =
        (sPriceInputCycle positionCyclePass,
        liqinfoInputCycle positionCyclePass,
        longinfoInputCycle positionCyclePass,
        shortinfoInputCycle positionCyclePass,
        vAmountInputCycle positionCyclePass,
        posinfoInputCycle positionCyclePass)
  let (_, sideLIQ, _) =
        case liqinfo of
          [] -> ([], [], [])
          _  -> unzip3 liqinfo
  let liquidationString =
        if null sideLIQ
          then ""
          else head sideLIQ

-- ? POSITION FUTURE
      -- | check if the future file is empty
  numTakers <- randomRIO (1, maxTakers) :: IO Int   -- select how many takers
  numMakers <- randomRIO (1, maxMakers) :: IO Int   -- select how many makers
  volumeSplitT <- generateVolumes numTakers vAmount -- split the volume
  volumeSplitM <- generateVolumes numMakers vAmount -- split the volume
  liquidated <- liquidationDuty longinfo shortinfo sPricegen
      
--   print longinfo
  let (liquidationIO, liquidationFuture) = liquidated
  let (longliq, shortliq) = liquidationFuture
  let newLiqInfogen = liquidationIO
  runProgram <-
    normalrunProgram
      (volumeSplitT, volumeSplitM)
      (longliq, shortliq)
      sPricegen
      liquidationString
  let ((newPosFutureShortGen, newPosFutureLongGen), newPositionsGen) = 
          runProgram
      -- | (TakerTuple,MakerTuple)
      -- TODO convert into seq and keep it until base case
  let updatedPositionAcc =
        let ((taker1, maker1), (taker2, maker2)) = (newPositionsGen, posinfo)
            takerSeq1 = Seq.fromList taker1
            takerSeq2 = taker2
            makerSeq1 = Seq.fromList maker1
            makerSeq2 = maker2
         in (takerSeq1 Seq.>< takerSeq2, makerSeq1 Seq.>< makerSeq2)
  return
    PositionCycleOutput
    {  newLiqInfo       = newLiqInfogen
    , nullLiqInfo       = updatedPositionAcc
    , newPositions      = newPositionsGen
    , newPosFutureLong  = newPosFutureLongGen
    , newPosFutureShort = newPosFutureShortGen
    }
