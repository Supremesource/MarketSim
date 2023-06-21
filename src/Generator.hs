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

   {-
    putStrLn "test liq acc: \n"
    print liqinfo
    putStrLn "------\n\n"
   -- print $ "positioning " ++ show posinfo
   -}

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


      print liqinfo
      let (volumeLIQ, sideLIQ, styleLIQ) = case liqinfo of
                  [] -> ([], [], [])
                  _  -> unzip3 liqinfo

      putStrLn "\n\n LIQUIDATION INFO: \n"
      let wholeLIQvolume = sum volumeLIQ
      let wholeLIQside = if head sideLIQ == "z" then Buy else Sell
     
      print volumeLIQ
      print sideLIQ
      print styleLIQ
    
      
      orderBookGeneration <- orderBookProcess (liqinfo,posinfo,longinfo,shortinfo,(vAmount,vSide'),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS,sPoint,takeWall)
      let (sPrice, finalBookAsk, finalBookBid, maxMinLmt, lengchngBid', lengchngAsk', listASK', listBID') = orderBookGeneration
      orderBookDetails   <- additionalBookInfo finalBookAsk finalBookBid vSide' vAmount sPoint maxMinLmt takeWall lengchngBid' lengchngAsk' listASK' listBID' sPrice 
      let newbookDetails = orderBookDetails 

      orderBookGenerationLiquidation <- orderBookProcess (liqinfo,posinfo,longinfo,shortinfo,(wholeLIQvolume,wholeLIQside),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS,sPoint,takeWall)
      orderBookDetailsLiquidation    <- additionalBookInfo finalBookAsk finalBookBid wholeLIQside wholeLIQvolume sPoint maxMinLmt takeWall lengchngBid' lengchngAsk' listASK' listBID' sPrice



      positionGenerator <- positionCycle sPrice liqinfo longinfo shortinfo vAmount posinfo
      let (newLiqInfo,newPositions,newPosFutureLong, newPosFutureShort) = positionGenerator


      return (newLiqInfo,newPositions,newPosFutureLong, newPosFutureShort,finalBookBid, finalBookAsk, newbookDetails)


-- | orderbook main processing
orderBookProcess :: ListPass -> IO (Double,OrderBook,OrderBook, [[Int]], Int,Int, [(Double,Int)], [(Double,Int)])
orderBookProcess (_,_,_,_,(vAmount,vSide'),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS,_,_) = do

                      
            -- TODO make function purely for this called orderbook  
            -- | local variables      
            let (volumeBID, volumeASK) =
                  calculateVolumes vSide' vAmount
            let (bidUpdateBook, askUpdateBook) =
                  calculateBooks volumeBID volumeASK bidBook askBook
            -- | how much volume took from certain order books
            let (lengchngAsk', lengchngBid')   =
                  lengthChanges bidUpdateBook bidBook askUpdateBook askBook
            let sPrice  =
                  startingPrices vSide' bidUpdateBook askUpdateBook
            let (askSetupInsert, bidSetupInsert) =
                  calculateSetupInserts lengchngAsk' lengchngBid' sPrice gen1 gen2
            let maxMinLmt            :: [[Int]]  =
                  [fullwallsASK, fullwallsBIDS]
            pricesASK   <- printCustomRandomList lengchngAsk'
            pricesBID   <- printRandomList' lengchngBid'
            -- | the / number is how smaller the insertion will be
            let (listASK', listBID') =
                  calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID
            -- //TODO, possible microoptimization with the stuff below :
            -- | let insertInAsk = if vSide == Buy then [] else listASK
            --  / let insertInBid = if vSide == Sell then [] else listBID    // --
            let (finalBookAsk, finalBookBid) =
                  calculateFinalBooks vSide' askUpdateBook listASK' askBook bidUpdateBook listBID' bidBook 
           
            return (sPrice, finalBookAsk, finalBookBid, maxMinLmt, lengchngBid', lengchngAsk', listASK', listBID')





-- | continuing orderbook processing, with additional data
additionalBookInfo :: OrderBook -> OrderBook -> VolumeSide -> Int -> StartingPoint
                      -> [[Int]] -> Totakefromwall -> Int -> Int  
                      -> [(Double,Int)] -> [(Double,Int)] -> Double
                      -> IO BookStats                
additionalBookInfo finalBookAsk finalBookBid vSide' vAmount sPoint maxMinLmt takeWall lengchngBid' lengchngAsk' listASK' listBID' sPrice
            = do

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
            let check = settingcheck vSide' vAmount asksTot' bidsTot'
            check
            -- | how accumulator stores the values
            let newbookDetails = setupBookDetails (sPoint, maxMinLmt
                  , asksTot', bidsTot', takeWall, lengchngBid', lengchngAsk' ,listASK'
                  , listBID', vSide', vAmount, sprd' ,sPrice ,bidAskR')
            let insertinInfo = formatAndPrintInfo newbookDetails
            insertinInfo -- THIS IS ONLY CONSOLE (pass into the console)
        
            return newbookDetails


-- | position cycle
positionCycle ::  Double -> LiquidationCall -> FutureInfo -> FutureInfo -> Int -> NewPositioning 
      -> IO (LiquidationCall, NewPositioning, FutureInfo, FutureInfo)
positionCycle sPrice liqinfo longinfo shortinfo vAmount posinfo = do
    
      -- TODO make a funciton purely for this called position info
      -- ? POSITION FUTURE  
      -- | check if the future file is empty                    
      numTakers    <- randomRIO (1, maxTakers) :: IO Int -- select how many takers
      numMakers    <- randomRIO (1, maxMakers) :: IO Int -- select how many makers
      volumeSplitT <- generateVolumes numTakers vAmount  -- split the volume
      volumeSplitM <- generateVolumes numMakers vAmount  -- split the volume
      liquidated   <- liquidationDuty longinfo shortinfo sPrice
      --   print "infos"
      --   print longinfo

      let liquidationIO = fst liquidated
      putStrLn "\n liquidation IO \n"
      print liquidationIO
      let liquidationFuture = snd liquidated

      let longliq =  fst liquidationFuture
      let shortliq = snd liquidationFuture
      let newLiqInfo = liquidationIO
      run <- normalRun (volumeSplitT ,volumeSplitM ) (longliq, shortliq) posinfo sPrice


      let ((newPosFutureShort, newPosFutureLong), newPositions) = run
      --     putStrLn "liquidations \n"
      --    print longliq
      --  print shortliq

      putStrLn "\n\n\ntest  short /future/: \n"
      print newPosFutureShort
      putStrLn "test  long /future/: \n"
      print newPosFutureLong
      putStrLn "test positions: \n"
      print newPositions
      putStrLn "\n\n\n"
      return (newLiqInfo, newPositions, newPosFutureLong,  newPosFutureShort)
                        







                  