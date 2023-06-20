{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Generator where

-- | module of utility funcitons
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import System.Random
    ( RandomGen(split), randomRIO )
import           Data.Aeson (encode)
import qualified Data.Sequence as Seq
import Data.Foldable (toList)

-- | internal libraries
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib
import           RunSettings
import           PosCycle
import           Util

-- ? processor part
recursiveList :: RecursionPass -> IO (LiquidationCall,NewPositioning,FutureInfo,FutureInfo,OrderBook, OrderBook, [BookStats])
recursiveList (liqinfo,posinfo,longinfo,shortinfo,[], bidBook, askBook, _, _, _, _, _, _, bookDetails) = do

    filewrites1 $ tail (reverse bookDetails)
    let writeBidBook = Book { book = bidBook }
    let writeAskBook = Book { book = askBook }
    let writePositionFuture = Transaction { future = longinfo ++ shortinfo }
    let writePositionFuture' = encode writePositionFuture
    BL.writeFile posFutureP writePositionFuture'
    BL.writeFile bidBookP (encode writeBidBook)
    BL.writeFile askBookP (encode writeAskBook)
    putStrLn "test liq acc: "
    print liqinfo
   -- print $ "positioning " ++ show posinfo

    return (liqinfo,posinfo,longinfo,shortinfo,bidBook, askBook, bookDetails)

recursiveList (liqinfo,posinfo,longinfo, shortinfo, x:xs, bidBook, askBook, gen1, gen2,
    fullwallsASK, fullwallsBIDS, sPoint, takeWall, bookDetails) =

 orderbookLoop (liqinfo,posinfo,longinfo, shortinfo, x, bidBook, askBook, gen1
    , gen2, fullwallsASK, fullwallsBIDS, sPoint, takeWall) >>=
    \(newliqinfo,newPosInfo,newLonginfo,newShortinfo,newBidBook, newAskBook, newBookDetails) -> do

  let (newGen1, newGen2) = (fst (split gen1), fst (split gen2))
  recursiveList (newliqinfo,newPosInfo,newLonginfo,newShortinfo,xs,newBidBook,newAskBook,newGen1,newGen2
    ,fullwallsASK,fullwallsBIDS,sPoint,takeWall,newBookDetails:bookDetails)



orderbookLoop :: ListPass
      -> IO (LiquidationCall,NewPositioning,FutureInfo,FutureInfo,OrderBook,OrderBook,BookStats)
orderbookLoop (liqinfo,posinfo,longinfo,shortinfo,(vAmount,vSide'),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS,sPoint,takeWall)= do
                      
                          -- # ----------------- FUNCTION -------------------- # 
                          -- TODO make function purely for this called orderbook  
                          -- | local variables      
                          let (volumeBID, volumeASK) =
                                calculateVolumes vSide' vAmount
                          let (bidUpdateBook, askUpdateBook) =
                                calculateBooks volumeBID volumeASK bidBook askBook
                          -- | how much volume took from certain order books
                          let (lengchngAsk', lengchngBid') =
                                lengthChanges bidUpdateBook bidBook askUpdateBook askBook
                          let sPrice  =
                               startingPrices vSide' bidUpdateBook askUpdateBook
                          let (askSetupInsert, bidSetupInsert) =
                                calculateSetupInserts lengchngAsk' lengchngBid' sPrice gen1 gen2
                          let maxMinLmt            :: [[Int]]    =
                                [fullwallsASK, fullwallsBIDS]
                          pricesASK  <- printCustomRandomList lengchngAsk'
                          pricesBID   <- printRandomList' lengchngBid'
                          -- | the / number is how smaller the insertion will be
                          let (listASK', listBID') =
                                calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID
                         -- //TODO, possible microoptimization with the stuff below :
                          -- | let insertInAsk = if vSide == Buy then [] else listASK
                          --  / let insertInBid = if vSide == Sell then [] else listBID    // --
                          let (finalBookAsk, finalBookBid) =
                                calculateFinalBooks vSide' askUpdateBook listASK' askBook bidUpdateBook listBID' bidBook
                          -- # ----------------- FUNCTION -------------------- # 
                     




                          -- # ----------------- FUNCTION -------------------- # 
                          -- TODO make funciton purely for this called additional book info
                          -- | ask in total in terms of count
                          -- | bids in total in terms of count                  
                          let (asktotal, bidtotal) =
                               calculateTotalsCount finalBookAsk finalBookBid
                          -- | Ratio between bids and AKS
                          let bidAskR' =
                               abs ((bidtotal - asktotal) / (bidtotal + asktotal)) :: Double
                          -- | Ratio is benefiting :
                          -- | asks in total -> $$
                          -- | bids in total -> $$
                          let (asksTot', bidsTot') = calculateTotals finalBookAsk finalBookBid
                          -- | bid ask spread
                          let (firstelemASK, firstelemBID) = calculateFirstElements finalBookAsk finalBookBid
                          let sprd'  = spread' firstelemASK firstelemBID
                          -- | checking if the stats above will go through with the orderbook
                          -- | local check
                          let check= settingcheck vSide' vAmount asksTot' bidsTot'
                          check
                          -- | how accumulator stores the values
                          let newbookDetails = setupBookDetails (sPoint, maxMinLmt
                                , asksTot', bidsTot', takeWall, lengchngBid', lengchngAsk' ,listASK'
                                , listBID', vSide', vAmount, sprd' ,sPrice ,bidAskR')
                          let insertinInfo = formatAndPrintInfo newbookDetails
                          insertinInfo -- THIS IS ONLY CONSOLE
                          -- # ----------------- FUNCTION -------------------- # 






                          -- # ----------------- FUNCTION -------------------- # 
                          -- TODO make a funciton purely for this called position info
                          -- ? POSITION FUTURE  
                          -- | check if the future file is empty                    
                          numTakers    <- randomRIO (1, maxTakers) :: IO Int -- select how many takers
                          numMakers    <- randomRIO (1, maxMakers) :: IO Int -- select how many makers
                          volumeSplitT <- generateVolumes numTakers vAmount  -- split the volume
                          volumeSplitM <- generateVolumes numMakers vAmount  -- split the volume
                          liquidated   <- liquidationDuty longinfo shortinfo sPrice
                          
                          let liquidationIO = fst liquidated
                          let liquidationsInfo = snd liquidated
                          let liquidationsInfo' = fst liquidated
                          let longliq =  fst liquidationsInfo
                          let shortliq = snd liquidationsInfo
                          run <- normalRun (volumeSplitT ,volumeSplitM ) (longliq, shortliq) posinfo sPrice
                         
                          let ((newPosFutureShort, newPosFutureLong), newPositions) = run
                        -- # ----------------- FUNCTION -------------------- #
                                                                     
                          -- | returning the accumulators
                          return (liqinfo,newPositions,newPosFutureLong, newPosFutureShort,finalBookBid, finalBookAsk, newbookDetails)
                       


                          -- posFuture <- positionFuture -- transaction
                          -- print isFutureEmpt
                          -- if no read the contents and move on isnotemptygenerator
                          -- if empty then write init generator consisting of only opening|+)
                          -- isemptygenerator ..
                          -- pass from isemptygenerator to positionfuture
                          -- isnotemptygenerator --> scanning for liquidation info -> paring "left over empty" with new based off of statistics
                          -- if liquidation pass as a volume
                          -- | returning the orderbook