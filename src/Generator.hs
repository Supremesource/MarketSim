{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE TupleSections #-}
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
module Generator
{- 
-- ! DESCRIPTION 
Heart of the whole backend:

################################
# managing funciton delegation #
################################

Establishing generating of orderbook
Managing position functions 
Managing liquidations
-- TODO use sequences for volume
-}
where
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable        (toList)
import qualified Data.Sequence        as Seq
import           System.Random        (RandomGen (split), randomRIO)
import           Data.Sequence        (Seq, fromList, (><))
import Data.Maybe (fromMaybe)
import Data.Aeson.Encode.Pretty (encodePretty)
import Debug.Trace
-- | internal libraries
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib
import           PosCycle
import           RunSettings
import           Util
import           Colours


 {-
  let liqList = initLiquidationAcc1Input genPass
  let adjustedLiqSeq = fmap (\(amt, side, _) -> 
          if side == "z" || side == "x" 
          then (amt, Buy) 
          else (amt, Sell) ) liqList
  let adjustedLiqList = toList adjustedLiqSeq
  putStrLn "adjusted liquidation list: "
  print adjustedLiqList
  let adjustedGenPass = genPass { listofvolumesInput = adjustedLiqList }  
-}

{-
-- ? processor part
generaterunProgram ::
     GenerationPass
  -> IO  GenerationOutput
generaterunProgram genPass = do

  putStrLn $ red "traced list if null: "
  let listOfVolumes = trace (show (listofvolumesInput genPass)) (listofvolumesInput genPass)

-- if the list of volumes is empty we write otherwise we accumulate
  if null listOfVolumes 
  then processTransactionBase genPass

--  else if null listOfVolumes && not (null liqList) then do
--       putStrLn $ purple "Debug.bang it is happening liquidation is the last element"
--       processTransaction adjustedGenPass
  else processTransaction genPass
-}

-- TODO
-- ! turn back on

generaterunProgram ::
     GenerationPass
  -> IO  GenerationOutput
generaterunProgram genPass

  |  null (listofvolumesInput genPass)
 -- && null (initLiquidationAcc1Input genPass >< initLiquidationAcc2Input genPass) 
  =  processTransactionBase genPass
  |  otherwise
  = processTransaction genPass

{-
-- TODO concat the liquidations
liqAdjustement :: ListPass -> IO ReturnVolumeProcess
liqAdjustement listPass@ListPass{} = do
  let liqinfo = liqinfoInpt listPass
  let (volumeLIQ, sideLIQ, _) =
        case toList liqinfo of
          [] -> ([], [], [])
          _  -> unzip3 $ toList liqinfo

  putStrLn "\n ------------------------------------ \n "
  putStrLn $ purple "bok loop liquidation info : "
  print liqinfo
 -- maybe bug
  let wholeLIQvolume = sum volumeLIQ
  putStrLn "summed liq vol "
  print wholeLIQvolume
  -- # for short liquidation buy and for long liquidation sell
  let wholeLIQside = if head sideLIQ == "z" then Buy else Sell
  volumeProcessing listPass (wholeLIQvolume,wholeLIQside)
-}


-- TODO
-- convert max min limit into two seprate functions
-- TODO reverse and take out the head which is only zeros
-- TODO think about the order of the arguments
-- | there might be a need to reverse some data so the order is correct


data GenerationPass = GenerationPass
  { initLiquidationAccInput         :: Seq (Int, String, String) -- liqudation output 2 writing accumulator
  , initPositioningAccInput         :: (Seq (Int, String), Seq (Int, String))
  , initAccLongCloseInput           :: Seq (Double, Int, String, Double, Double, Bool)
  , initAccShortCloseInput          :: Seq (Double, Int, String, Double, Double, Bool)
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
  , liquidationTagInput             :: [(Bool, String)]

  }

data GenerationOutput = GenerationOutput
  { liqInfoOutput     :: Seq (Int, String, String)
  , posInfoOutput     :: (Seq (Int, String), Seq (Int, String))
  , longInfoOutput    :: Seq (Double, Int, String, Double, Double, Bool)
  , shortInfoOutput   :: Seq (Double, Int, String, Double, Double, Bool)
  , bidBookOutput     :: SeqOrderBook
  , askBookOutput     :: SeqOrderBook
  , bookDetailsOutput :: [BookStats]
  , posStatsOutput    :: [Stats]

  
  }

data ListPass = ListPass
    { liqinfoInpt        :: Seq (Int, String, String)
    , posinfoInpt        :: (Seq (Int, String), Seq (Int, String))
    , longinfoInpt       :: Seq (Double, Int, String, Double, Double, Bool)
    , shortinfoInpt      :: Seq (Double, Int, String, Double, Double, Bool)
    , volLstInpt         :: Volume
    , volLstWholeImpt    :: VolumeList
    , bidBookInpt        :: SeqOrderBook
    , askBookInpt        :: SeqOrderBook
    , gen1Inpt           :: Generator
    , gen2Inpt           :: Generator
    , fullwallsASKInpt   :: FullWall
    , fullwallsBIDSInpt  :: FullWall
    , sPointInpt         :: StartingPoint
    , takeWallInpt       :: Totakefromwall
    , liquidationTagInpt :: [(Bool, String)]}

data ReturnVolumeProcess = ReturnData
    { newliqinfoOutpt       :: Seq (Int, String, String)
    , newPosInfoOutpt       :: (Seq (Int, String), Seq (Int, String))
    , newLonginfoOutpt      :: Seq (Double, Int, String, Double, Double, Bool)
    , newShortinfoOutpt     :: Seq (Double, Int, String, Double, Double, Bool)
    , newBidBookOutpt       :: SeqOrderBook
    , newAskBookOutpt       :: SeqOrderBook
    , newBookDetailsOutpt   :: BookStats
    -- , updatedVolumeOutpt    :: Volume
    , newPosStatsOutpt      :: Stats
    , updatedVolumeOutpt    :: VolumeList
    , liquidationTagOutpt   :: [(Bool, String)]

    }


data ReturnVolumeProcessProcess
  = ReturnVolumeProcessProcess
      { sPrice       :: Double
      , finalBookAsk :: SeqOrderBook
      , finalBookBid :: SeqOrderBook
      , maxMinLmt    :: (Int,Int)
      , lengchngBid' :: Int
      , lengchngAsk' :: Int
      , listASK'     :: SeqOrderBook
      , listBID'     :: SeqOrderBook
      }

data AdditionalBook    = AdditionData
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

data PositionCyclePass = PositionCyclePass
  {   liqinfoInputCycle   :: MarginCall
    , sPriceInputCycle    :: Double
    , longinfoInputCycle  :: Seq (Double, Int, String, Double, Double, Bool)
    , shortinfoInputCycle :: Seq (Double, Int, String, Double, Double, Bool)
    , vAmountInputCycle   :: Int
    , posinfoInputCycle   :: (Seq (Int, String), Seq (Int, String))
    , vSideInputCycle     :: VolumeSide
  }

data PositionCycleOutput = PositionCycleOutput
  { newLiqInfo        :: Seq (Int, String, String)
  , nullLiqInfo       :: (Seq (Int, String), Seq (Int, String))
  , newPositions      :: NewPositioning
  , newPosFutureLong  :: Seq (Double, Int, String, Double, Double, Bool)
  , newPosFutureShort :: Seq (Double, Int, String, Double, Double, Bool)
  , newPoslevgT              :: [Int]
  , newPoslevgM              :: [Int]
  } deriving (Eq)



-- TODO debug liquidation info being passed incorrectly at the end
-- also being passed twice 
processTransaction :: GenerationPass -> IO GenerationOutput
processTransaction genPass@GenerationPass{} = do
  let liquidationTag = liquidationTagInput genPass
  let liqinfo = initLiquidationAccInput genPass
 -- trace ("liqinfo: " ++ show liqinfo) $ return ()
 -- let writeLiqInfo = initLiquidationAcc2Input genPass
 -- trace ("writeLiqInfo: " ++ show writeLiqInfo ++ "\n\n") $ return ()
  let posinfo   = initPositioningAccInput genPass
  let longinfo  = initAccLongCloseInput genPass
  let shortinfo = initAccShortCloseInput genPass
  -- putStrLn $ "\nlong info : " ++ show longinfo
  -- putStrLn $ "\nshort info : " ++ show shortinfo
  
  let listOfVolumes = listofvolumesInput genPass
--  let (x, _) = case listOfVolumes of
--        [] -> error "list of volumes empty"
--        (y:ys) -> (y, ys)
--  let (x, xs) = case  listofvolumesInput genPass of
--        [] -> error "listofvolumesInput was empty!"
--        (y:ys) -> (y, ys)
 -- putStrLn $ green "\nhead: " ++ show x
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

-- liquidation handeling
  -- let adjustedLiqInfo = if liqinfo null then [] else Seq.Drop 1 liqinfo 

  volumeProcessing
    ListPass
    {   liqinfoInpt          = liqinfo -- case empty [0,"",""] otherwise liqinfo
      , posinfoInpt          = posinfo
      , longinfoInpt         = longinfo
      , shortinfoInpt        = shortinfo
      , volLstWholeImpt      = listOfVolumes
      , bidBookInpt          = bidBook
      , askBookInpt          = askBook
      , gen1Inpt             = gen1
      , gen2Inpt             = gen2
      , fullwallsASKInpt     = fullwallsASK
      , fullwallsBIDSInpt    = fullwallsBIDS
      , sPointInpt           = sPoint
      , takeWallInpt         = takeWall
      , liquidationTagInpt   = liquidationTag } >>= \ ReturnData

    {
        newliqinfoOutpt      = newLiqInfo'  -- cycling liquidation list (vol like)
      , newPosInfoOutpt      = newPosInfo
      , newLonginfoOutpt     = newLonginfo
      , newShortinfoOutpt    = newShortinfo
      , newBidBookOutpt      = newBidBook
      , newAskBookOutpt      = newAskBook
      , newBookDetailsOutpt  = newBookDetails
      , updatedVolumeOutpt   = newVolume
      , newPosStatsOutpt     = newPosStats
      , liquidationTagOutpt  = newLiquidationTag 
      } -> do
  
    let (newGen1, newGen2)   = (fst (split gen1), fst (split gen2))
    -- trace (orange "writeLiqInfoOLD: " ++ show writeLiqInfo ++ "\n\n") $ return ()
    -- trace (orange "writeLiqInfoNEW: " ++ show newWriteLiqInfo ++ "\n\n") $ return ()          
    generaterunProgram
     GenerationPass
     {  --initLiquidationAcc1Input = newliqinfo
        initLiquidationAccInput  = newLiqInfo'
      , initPositioningAccInput  = newPosInfo
      , initAccLongCloseInput    = newLonginfo
      , initAccShortCloseInput   = newShortinfo
       -- ? `++` should not be a significant performance bottleneck here
      , listofvolumesInput = newVolume
      , bidBookInput = newBidBook
      , askBookInput = newAskBook
      , gen1Input    = newGen1
      , gen2Input    = newGen2
      , fullwallsASKInput  = fullwallsASK
      , fullwallsBIDSInput = fullwallsBIDS
      , initstartingPointInput  = sPoint
      , inittotakefromwallInput = takeWall
      , initialBookDetailsListInput = newBookDetails : bookDetails
      , initStatsInput = newPosStats : posStats
      , liquidationTagInput = newLiquidationTag
      
      }


-- ! optimization warning
insertAt :: Int -> [a] -> [a] -> [a]
insertAt i xs ys = let (before, after) = splitAt i ys in before ++ xs ++ after

-- # biggest computational bottleneck from the whole program
volumeProcessing :: ListPass  -> IO ReturnVolumeProcess
volumeProcessing listPass@ListPass{}  = do
  let (liqinfo,posinfo,longinfo,shortinfo,vol:restOfVol,sPoint,takeWall,liqT:liqTRest) =
        ( liqinfoInpt         listPass,
          posinfoInpt         listPass,
          longinfoInpt        listPass,
          shortinfoInpt       listPass,
          volLstWholeImpt     listPass,
          sPointInpt          listPass,
          takeWallInpt        listPass,
          liquidationTagInpt  listPass )

  let (volumeAmt,volumeSide) = vol

  let adjListPass  = listPass { volLstInpt = (volumeAmt,volumeSide) }
  orderBookGeneration <- orderBookProcess adjListPass

  let ReturnVolumeProcessProcess
        { sPrice       = sPriceGenerated
        , finalBookAsk = finalBookAskGenerated
        , finalBookBid = finalBookBidGenerated
        , maxMinLmt    = maxMinLmtGenerated
        , lengchngBid' = lengchngBidGenerated
        , lengchngAsk' = lengchngAskGenerated
        , listASK'     = listASKGenerated
        , listBID'     = listBIDGenerated } = orderBookGeneration

  orderBookDetails <- additionalBookInfo
                 AdditionData
                            { finalBookAskInput     = finalBookAskGenerated
                              ,finalBookBidInput    = finalBookBidGenerated
                              ,vSide'Input          = volumeSide
                              ,vAmountInput         = volumeAmt
                              ,sPointInput          = sPoint
                              ,maxMinLmtInput       = maxMinLmtGenerated
                              ,takeWallInput        = takeWall
                              ,lengchngBid'Input    = lengchngBidGenerated
                              ,lengchngAsk'Input    = lengchngAskGenerated
                              ,listASK'Input        = listASKGenerated
                              ,listBID'Input        = listBIDGenerated
                              ,sPriceInput          = sPriceGenerated }
  let (volAmt, volSide) = vol

  let headLiqInfo = case safeHead $ toList liqinfo  of
          Nothing -> []
          Just (a,b,c) -> [(a,b,c)]

  -- // pass safe head liqinfo
  positionGenerator <- positionCycle
            PositionCyclePass
                            {  sPriceInputCycle     = sPriceGenerated
                              , liqinfoInputCycle   = headLiqInfo -- // toList liqinfo
                              , longinfoInputCycle  = longinfo
                              , shortinfoInputCycle = shortinfo
                              , vAmountInputCycle   = volAmt
                              , posinfoInputCycle   = posinfo
                              , vSideInputCycle     = volSide}

  let PositionCycleOutput
         { newLiqInfo        = newLiqInfoGenerated
          ,nullLiqInfo       = newPositionsGenerated
          ,newPositions      = localPositions
          ,newPosFutureLong  = newPosFutureLongGenerated
          ,newPosFutureShort = newPosFutureShortGenerated  
          ,newPoslevgT        = newPosLevgT 
          ,newPoslevgM     = newPosLevgM } = positionGenerator

  let liquidationString = if null newLiqInfoGenerated then [""] else (toList . fmap (\(_, _, s) -> s)) newLiqInfoGenerated
  let liquidationTag    = if null newLiqInfoGenerated then  [(False,"")] else map (True, ) liquidationString
  let maybeLiqConcat    = if null newLiqInfoGenerated then [] else map (True, ) liquidationString
  -- ! optimization warning
  let liquidationTagOut = if null liqTRest then liquidationTag else  liqTRest  ++ maybeLiqConcat


 -- ++ liquidation information 
  let newStats  = aggregateStats localPositions newLiqInfoGenerated liqT (newPosLevgT,newPosLevgM) initStats

-- in case of liquidation it will add the volume to the accumulator
  --let isVolAtOneElement = vAmount == [a]
  --let addVolAcc      = ([(vAmount, vSide') | wholeLIQvolume /= 0])

 -- let addVolAcc   = ([(wholeLIQvolume,wholeLIQside) | wholeLIQvolume /= 0])
--  let addVolAcc' = ([(vAmount, vSide') | wholeLIQvolume /= 0])
 -- putStrLn $ blue "\nadd vol acc: "
 -- print addVolAcc
  let newbookDetails = orderBookDetails

  let liquidationTOvol = if null newLiqInfoGenerated then fromList []
                          else  fmap (\(amt, side, _) ->
                            if side == "z" || side == "x"
                               then (amt, Buy)
                                  else (amt, Sell) ) newLiqInfoGenerated

  let concatVolProcess = if null liqTRest
      -- ! optimization warning
      then toList liquidationTOvol ++ restOfVol
      else insertAt (length liqTRest) (toList liquidationTOvol) restOfVol


  let newaccLiqinfo = case safeSeqTail liqinfo of
        Nothing -> newLiqInfoGenerated
        Just a -> a <> newLiqInfoGenerated
        
  -- concat newLiqInfoGenerated with old one 
  return
               ReturnData
                        {                           -- liq output of pos cycle + that 
                            newliqinfoOutpt       =  newaccLiqinfo -- // newLiqInfoGenerated
                          , newPosInfoOutpt       = newPositionsGenerated
                          , newLonginfoOutpt      = newPosFutureLongGenerated
                          , newShortinfoOutpt     = newPosFutureShortGenerated
                          , newBidBookOutpt       = finalBookBidGenerated
                          , newAskBookOutpt       = finalBookAskGenerated
                          , newBookDetailsOutpt   = newbookDetails
                          , updatedVolumeOutpt    = concatVolProcess
                          , newPosStatsOutpt      = newStats
                          , liquidationTagOutpt   = liquidationTagOut
                           }



-- | orderbook main processing
orderBookProcess ::  ListPass -> IO ReturnVolumeProcessProcess
orderBookProcess listPass@ListPass{}  = do
  let ((vAmount,vSide'),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS)  =
        ( volLstInpt listPass,
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
  let sPriceGenerated = price vSide' bidUpdateBook askUpdateBook
  let (askSetupInsert, bidSetupInsert) =
        calculateSetupInserts
        lengchngAskGenerated
        lengchngBidGenerated
        sPriceGenerated
        gen1 gen2
  let maxMinLmtGenerated :: [[Int]]   = [fullwallsASK, fullwallsBIDS]
  let maxMinLmtTuple = (  fromMaybe 0 $ maxList maxMinLmtGenerated
                        , fromMaybe 0 $ minList maxMinLmtGenerated )

  pricesASK <- printCustomRandomList lengchngAskGenerated
  pricesBID <- printRandomList' lengchngBidGenerated
            -- | the / number is how smaller the insertion will be
  let (listASKGenerated, listBIDGenerated) =
        calculateBookLists askSetupInsert bidSetupInsert pricesASK pricesBID -- TODO rename the calculateBookLists
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
    ReturnVolumeProcessProcess
    {     sPrice       = sPriceGenerated
        , finalBookAsk = finalBookAskGenerated
        , finalBookBid = finalBookBidGenerated
        , maxMinLmt    = maxMinLmtTuple
        , lengchngBid' = lengchngBidGenerated
        , lengchngAsk' = lengchngAskGenerated
        , listASK'     = listASKGenerated
        , listBID'     = listBIDGenerated  }




      -- TODO make this funciton perform better, it's not very efficient
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


-- second biggest bottle neck
-- TODO convert into records
-- | position cycle 
-- ! does not need to recive liquidation info
positionCycle :: PositionCyclePass -> IO PositionCycleOutput
positionCycle positionCyclePass@PositionCyclePass{} = do
  let (sPricegen, liqinfo, longinfo, shortinfo, vAmount, posinfo, vSide') =
        (sPriceInputCycle   positionCyclePass,
        liqinfoInputCycle   positionCyclePass,
        longinfoInputCycle  positionCyclePass,
        shortinfoInputCycle positionCyclePass,
        vAmountInputCycle   positionCyclePass,
        posinfoInputCycle   positionCyclePass,
        vSideInputCycle     positionCyclePass)

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
  -- ! get rid of !
  -- > RANDOMNESS <
  numTakers <- randomRIO (1, maxTakers) :: IO Int   -- select how many takers
  numMakers <- randomRIO (1, maxMakers) :: IO Int   -- select how many makers

  volumeSplitT <- generateVolumes numTakers vAmount -- split the volume
  volumeSplitM <- generateVolumes numMakers vAmount -- split the volume
  --let volumeSplitT = [vAmount]
  --let volumeSplitM = [vAmount]

  liquidated   <- liquidationDuty longinfo shortinfo sPricegen

--   print longinfo
  let (liquidationIO, liquidationFuture) = liquidated
  let (longliq, shortliq) = liquidationFuture
  let newLiqInfogen = liquidationIO
  runProgram <-
    normalrunProgram
      vSide'
      (volumeSplitT, volumeSplitM)
      (longliq, shortliq)
      sPricegen
      liquidationString
  let ((newPosFutureShortGen, newPosFutureLongGen), newPositionsGen, (leverageT,leverageM)) =
          runProgram
      -- | (TakerPositions,MakerPositions)
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
    , newPoslevgT       = leverageT
    , newPoslevgM       = leverageM
    }

-- adjusts leverage of positions (at the end of the cycle) to the current price
baseLeverageAdjustementAux :: Double -> (Double, Int, String, Double, Double, Bool) -> (Double, Int, String, Double, Double, Bool)
baseLeverageAdjustementAux currentP (liquidationP,a,sid,c,_,e) = case sid of _ 
                                                                                | sid == "f" && liquidationP == 0 -> (liquidationP,a,sid,c,1,e)
                                                                                | sid == "z" && liquidationP == (2 * currentP) -> (liquidationP,a,sid,c,1,e)
                                                                                | sid == "z"  -> (liquidationP,a,sid,c,currentP / (liquidationP - currentP),e) 
                                                                                | otherwise   -> (liquidationP,a,sid,c,currentP / (currentP - liquidationP),e)

baseLeverageAdjustement :: Double -> TransactionFut -> TransactionFut
baseLeverageAdjustement _ (TransactionFut []) = TransactionFut []
baseLeverageAdjustement currentP transcF = TransactionFut $ fmap (baseLeverageAdjustementAux currentP) (future transcF)

-- FUNCTION IS NOT USING UPDATED VOLUME ACCUMULATOR
-- base case do block
processTransactionBase :: GenerationPass -> IO GenerationOutput
processTransactionBase genPass = do
  let liqinfo      = initLiquidationAccInput genPass
  let posinfo      = initPositioningAccInput genPass
  let longinfo     = initAccLongCloseInput genPass
  let shortinfo    = initAccShortCloseInput genPass
  let bidBook      = bidBookInput genPass
  let askBook      = askBookInput genPass
  let currentPrice = startingprice (head (initialBookDetailsListInput genPass))
  let bookDetails  = tail $ reverse $ initialBookDetailsListInput genPass
  let posStats     = tail $ reverse $ initStatsInput genPass
--  putStrLn "the current Price is : \n"
--  print currentPrice

  --let leverageAmt  = tail $ levgInput genPass
  -- / ACTION
  -- ? DATA
  let posFuture =  TransactionFut $ toList $ longinfo >< shortinfo  
  let posFutureWithLeverageAdj = if adjustLeverage then baseLeverageAdjustement currentPrice posFuture else posFuture
  BL.writeFile posCloseDatP $ encodePretty posFutureWithLeverageAdj
  -- ? OUTPUT
  ids <- idList $ length posStats

  writeLog  bookDetails  ids
  writeBook bookDetails  ids
  writePosition posStats ids
  -- ? DATA DO NOT TOUCH
  -- | / rewriting orderbooks
  let writeBidBook = Book {book = toList bidBook}
  let writeAskBook = Book {book = toList askBook}
  --let writePositionFuture = TransactionFut { future = longinfo ++ shortinfo }
  --let writePositionFuture' = encode writePositionFuture
  --BL.writeFile posCloseDatP writePositionFuture'
  BL.writeFile bidBookP (encodePretty writeBidBook)
  BL.writeFile askBookP (encodePretty writeAskBook)

  return
    GenerationOutput
    {
        liqInfoOutput     = liqinfo
      ,  posInfoOutput    = posinfo
      , longInfoOutput    = longinfo
      , shortInfoOutput   = shortinfo
      , bidBookOutput     = bidBook
      , askBookOutput     = askBook
      , bookDetailsOutput = bookDetails
      , posStatsOutput    = posStats
   --   , levgOutput        = leverageAmt
    }
