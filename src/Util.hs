{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE TupleSections #-}
module Util where
-- | module of utility funcitons
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import System.Random
    ( Random(randomRs), RandomGen(split), randomRIO )
import           Data.Aeson (encode, decode)
-- | internal libraries
import           Colours
import           DataTypes
import           Filepaths
import           InputOutput
import           Lib
import           RunSettings
import Control.Monad



-- | Store the volume result , THIS IS ACCUMULATOR
initialBookDetails :: BookStats
initialBookDetails = BookStats
  { startingPoint    = 0.0
  , maxMinLimit     = replicate 2 []
  , asksTotal       = 0
  , bidsTotal       = 0
  , totakefromwall  = 0
  , lengthchangeBID = 0
  , lengthchangeASK = 0
  , listASK         = []
  , listBID         = []
  , vSide           = Buy
  , volumeAmount    = 0
  , spread          = 0.0
  , startingprice   = 0.0
  , bidAskRatio     = 0.0
  }

setupBookDetails :: InitBookStats -> BookStats
setupBookDetails (startingP', maxMinL' ,asksTot', bidsTot', takewall', lengchngBid'
  , lengchngAsk' ,listASK', listBID', vSide', volumeA', sprd' ,sprice' ,bidAskR') =
        BookStats {     startingPoint     = startingP'
                                            , maxMinLimit       = maxMinL'
                                            , asksTotal         = asksTot'
                                            , bidsTotal         = bidsTot'
                                            , totakefromwall    = takewall'
                                            , lengthchangeBID   = lengchngBid'
                                            , lengthchangeASK    = lengchngAsk'
                                            , listASK           = listASK'
                                            , listBID           = listBID'
                                            , vSide              = vSide'
                                            , volumeAmount      = volumeA'
                                            , spread            = sprd'
                                            , startingprice     = sprice'
                                            , bidAskRatio       = bidAskR'
                                            }



-- | helper funciton for the funciton below (where is everything starting at)
initStats :: Stats
initStats = Stats 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
-- | aggregating  stats together
aggregateStats :: (TakerTuple, MakerTuple) -> Stats -> Stats
aggregateStats (taker, makers) stats  =
  Stats
          { overallOI = overallOI stats + interestorPlus taker makers - interestorMinus taker makers,
            totalVolume = totalVolume stats + foldl (\acc (x, _) -> acc + x) 0 taker,
            buyVolume =
              buyVolume stats + foldl (\acc (x, y) -> if y == "x" || y == "z" then acc + x else acc) 0 taker,
            sellVolume =
              sellVolume stats + foldl (\acc (x, y) -> if y == "y" || y == "f" then acc + x else acc) 0 taker,
            takerXc =
              takerXc stats + countElements "x" taker,
            takerYc =
              takerYc stats + countElements "y" taker,
            takerZc =
              takerZc stats + countElements "z" taker,
            takerFc =
              takerFc stats + countElements "f" taker,
            makerXc =
              makerXc stats
                + countElements "x" makers,
            makerYc = makerYc stats + countElements "y" makers ,
            makerZc =
              makerZc stats + countElements "z" makers,
            makerFc =
              makerFc stats
                +  countElements "f" makers,
            offX = offX stats + orderSize "x" taker + orderSize "x" makers,
            offY = offY stats + orderSize "y" taker + orderSize "y" makers,
            offZ = offZ stats + orderSize "z" taker + orderSize "z" makers,
            offF = offF stats + orderSize "f" taker + orderSize "f" makers,
            takerX =
              takerX stats + orderSize "x" taker,
            takerY =
              takerY stats + orderSize "y" taker,
            takerZ =
              takerZ stats + orderSize "z" taker,
            takerF =
              takerF stats + orderSize "f" taker,
            makerX =
              makerX stats + orderSize "x" makers,
            makerY =
              makerY stats + orderSize "y" makers,
            makerZ =
              makerZ stats + orderSize "z" makers,
            makerF =
              makerF stats + orderSize "f" makers
      }

settingcheck :: VolumeSide -> Int -> Int -> Int -> IO ()
settingcheck vSide' volumeA' asksTot' bidsTot' = do
            let check
                    :: String
                    | vSide' /=  Buy && vSide' /=  Sell =
                        error $ red  "wrong volume specification !"
                    | vSide' ==  Buy && volumeA' > asksTot' =
                        error $ red  "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | vSide' ==  Sell && volumeA' > bidsTot' =
                        error $ red  "THE VOLUME EXCEEDED THE ORDERBOOK CAPACITY !"
                    | otherwise = ""
            putStrLn check

calculateVolumes :: VolumeSide -> Int -> (Int, Int)
calculateVolumes vSide' volumeA' =
    (if vSide' == Sell then volumeA' else 0,
     if vSide' == Buy then volumeA'  else 0)

calculateBooks :: Int -> Int -> OrderBook -> OrderBook -> (OrderBook, OrderBook)
calculateBooks volumeBID volumeASK bidBook askBook =
    let bidUpdateBook = orderbookChange bidBook volumeBID
        askUpdateBook = orderbookChange askBook volumeASK
    in (bidUpdateBook, askUpdateBook)

calculateFinalBooks :: VolumeSide -> OrderBook -> [(Double,Int)] -> OrderBook -> OrderBook -> [(Double,Int)] -> OrderBook ->(OrderBook, OrderBook)
calculateFinalBooks vSide' askUpdateBook listASK' askBook bidUpdateBook listBID' bidBook =
    let currentbookASK = if vSide' == Buy  then askUpdateBook else listASK' ++ askBook
        currentbookBID = if vSide' == Sell then bidUpdateBook else listBID' ++ bidBook
    in (currentbookASK, currentbookBID)

lengthChanges :: OrderBook -> OrderBook -> OrderBook -> OrderBook -> (Int, Int)
lengthChanges bidUpdateBook bidBook askUpdateBook askBook =
    (lengthchange bidUpdateBook bidBook, lengthchange askUpdateBook askBook)

startingPrices :: VolumeSide -> OrderBook -> OrderBook -> Double
startingPrices vSide' bidUpdateBook askUpdateBook =
    max (if vSide' == Sell then fst (head bidUpdateBook) else 0)
        (if vSide' == Buy then fst (head askUpdateBook)  else 0)

divisionValue :: Double
divisionValue = 1.10

calculateListTuples :: [Double] -> [Double] -> [Int] -> [Int] -> ([(Double,Int)], [(Double,Int)])
calculateListTuples askSetupInsert bidSetupInsert pricesASK pricesBID  =
    let listASK' = zipToTuples askSetupInsert (map (round . ((/ divisionValue) :: Double -> Double) . fromIntegral) pricesASK)
        listBID' = zipToTuples bidSetupInsert (map (round . ((/ divisionValue) :: Double -> Double) . fromIntegral) pricesBID)
    in (listASK', listBID')

calculateFirstElements :: OrderBook -> OrderBook -> (Double, Double)
calculateFirstElements finalBookAsk finalBookBid =
    let firstelemASK = (fst . head) finalBookAsk
        firstelemBID = (fst . head) finalBookBid
    in (firstelemASK, firstelemBID)

calculateTotals :: OrderBook -> OrderBook -> (Int, Int)
calculateTotals finalBookAsk finalBookBid =
    let asksTot' = sumInts finalBookAsk
        bidsTot' = sumInts finalBookBid
    in (asksTot', bidsTot')

calculateSetupInserts :: Int -> Int -> Double -> Generator -> Generator -> ([Double], [Double])
calculateSetupInserts lengchngAsk' lengchngBid' sPrice gen1 gen2 =
    let upMovesInsert = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
        downMovesInsert = take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
        askSetupInsert = take lengchngAsk' (tail (infiniteList' sPrice gen1 upMovesInsert))
        bidSetupInsert = take lengchngBid' (tail (infiniteListDown' sPrice gen2 downMovesInsert))
    in (askSetupInsert, bidSetupInsert)

calculateTotalsCount :: OrderBook -> OrderBook -> (Double, Double)
calculateTotalsCount finalBookAsk finalBookBid =
    let asktotal = fromIntegral (length finalBookAsk)
        bidtotal = fromIntegral (length finalBookBid)
    in (asktotal, bidtotal)

-- ! Most comperhensive SUMMARY of the WHOLE program
                                                                              {-
# HOW DOES THE POSITION MANAGEMENT WORK ? 
  /function is called recursively until the list of orders is empty
  
  1. First we take a list of volumes generated from other module
  2. With this list we change the orderbook -> Price change happens
  3. The list is passed into a `spitter` that spilts the list into `transactions`
  
  Now the fun begins 
  we check if there are alredy any positions generated, if not we will have to 
  generate them in the initGenerator (i should put this into the same generator
  , cause it does not really matter where i generate it)
  then we put these new positions (X, Y where x  = long, y = short)
  into a `basket` that represent the positons that can be closed.
  
  Now this basket is managed in terms of statistically asigning leverage
 as well as liquidation price.
 Now we have a list of positions that can be closed, with their liquidation price
 We can move onto the second run through our recrusive function.

 / Run 2. 
 Now we know which positions can be closed as well as some liquidation prices
 so same as in run 1 we send our volume list into the orderbook -> change the price
 after we change the price we check if some position in that liquidation list 
 should have been liquidated, if so we take it out of our basked of positions
 that can close and send the volume as Z or F (depends if it was a long or short
 ,who got liquidated). Now this `liquidation duty` transaction has to be pared
 with a counterparty of where we generate a opposite site that can consist of
 more than one position so we split the counterside volume and assign either
 new positions as a counterparty or exiting ones from our basket.

 Now we know there is no liquidation in our basked and we will make sense
 of the volume list that came to our funciton in the first place. 
 This is done by taking the volume list splitting it (same as the initial run)
 but the difference here is that we assign z and f for all of the transaction
 participains. Now we know how many exiting positions we had. So we take them
 out of our basked. We pass the new positioning as well as the new `Position future`
 which is the basket of potenitaly exiting positions. And we repeat the process
 until we reach our base case which is `[]` -> meaning we have no more volume.

 This is the time when we take out the accumulators: 
 1. Orderbook
 2. Positioning
 3. Position Future

 And we write from the computer memory into external jsons. Then we 
 can move onto the frontend/ do another simulation run with different settings. 

                                                                              -}

-- ! POSITION FUTURE

-- ? funcitons for the position future
-- PACKAGE CLOSING CONVERSION X & Y TO F & Z
closingConversion :: (TakerTuple,MakerTuple) -> (TakerTuple, MakerTuple)
closingConversion (takers,makers )| hasBothXY takers = error "Unsupported tuple format in takers"
                                  | hasBothXY makers = error "Unsupported tuple format in makers"
                                  | otherwise =
  (closingconvert filterTakers takers, closingconvert filterMakers makers)
  where
  isValidTuple (_, side) = side == "x" || side == "y"
  filterTakers = filter isValidTuple
  filterMakers = filter isValidTuple
  closingconvert f t = map (\(x,y) -> (x, if y == "x" then "f" else "z")) (f t)
  hasBothXY xs = "x" `elem` map snd xs && "y" `elem` map snd xs

-- statisctics thing determining leverage
-- TODO move this into the stats and also make it customizable or at least point 
-- to it in the settings
takenLeverage :: IO Int
takenLeverage = do
  x <- randomRIO (1,100) :: IO Int
  return $ case x of
     _ | x >= 1  && x <= 85 -> 1
       | x >= 86 && x <= 90 -> 2
       | x >= 91 && x <= 93 -> 5
       | x >= 94 && x <= 95 -> 10
       | x == 96            -> 15
       | x == 97            -> 20
       | x == 98            -> 25
       | x == 99            -> 50
       | x == 100           -> 100
       | otherwise          -> error "something went wrong in takenLeverage"


                                                  -- (liq price, liq amount, liq side)
positionFuture :: Double -> (TakerTuple, MakerTuple) -> IO FutureInfo
positionFuture price (taker, maker)  = do
  let (takerConver, makerConvert) = closingConversion (taker, maker)
  let concatTakerMaker = takerConver ++ makerConvert
  mapM calcPosition concatTakerMaker
  where
    calcPosition (amt, side) = do
      leverage <- takenLeverage
      print leverage
      let liquidationPrice
            | leverage /= 1 && side == "z" = (price / fromIntegral leverage) + price
            | leverage /= 1 && side == "f" = price - (price / fromIntegral leverage)
            | leverage == 1 && side == "z" = 2 * price
            | otherwise = 0
      return (liquidationPrice, amt, side)


oppositeSide :: String -> String
oppositeSide side = if side == "x" then "y" else "x"

-- TODO move into settings
takerXprobability :: Int
takerXprobability = 7

randomSide :: IO String
randomSide = do
    randVal <- randomRIO (0, 9) :: IO Int
    return $ if randVal < takerXprobability then "x" else "y"


initGenerator :: [Int] -> [Int] -> IO (TakerTuple, MakerTuple)
initGenerator takerLst makerLst = do
  takerSide' <- randomSide
  let makerside = oppositeSide takerSide'
  let takerT = zip takerLst $ replicate (length takerLst) takerSide'
  let makerT = zip makerLst $ replicate (length makerLst) makerside
  return (takerT, makerT)

closingProbab :: Int
closingProbab = 8


normalGenerator :: [Int] -> [Int] -> (FutureInfo,FutureInfo) -> IO (TakerTuple, MakerTuple)
normalGenerator takerLst makerLst (toTakeFromLong, toTakeFromShort) = do
  let checker = when (closingProbab > 9 || closingProbab < 0) $ error "closingProbab must be between 0 and 9"
  checker

  let genType = if sum takerLst + sum makerLst >= sum (map (\(_,n,_) -> n) toTakeFromLong) &&
                    sum takerLst + sum makerLst >= sum (map (\(_,n,_) -> n) toTakeFromShort)
                    then openingGen else normalGen
  genType
  where
    openingGen :: IO (TakerTuple, MakerTuple)
    openingGen = do
      takerSide' <- randomSide
      let makerside = oppositeSide takerSide'
      let takerT = zip takerLst $ replicate (length takerLst) takerSide'
      let makerT = zip makerLst $ replicate (length makerLst) makerside
      return (takerT, makerT)
    normalGen :: IO (TakerTuple, MakerTuple)
    normalGen = do
      takerSide' <- randomSide
      let makerside = oppositeSide takerSide'
      let closingSideT = if takerSide' == "x" then "z" else "f"
      let closingSideM = if makerside == "x" then "z" else "f"
      takerT <- mapM (\val ->
         do
        randVal <- randomRIO (0, 9) :: IO Int
        let sideT = if randVal < closingProbab then closingSideT else takerSide'
        return (val, sideT)) takerLst
      makerT <- mapM (\val -> do
        randVal <- randomRIO (0, 9) :: IO Int
        let sideM = if randVal < closingProbab then closingSideM else makerside
        return (val, sideM)) makerLst
      return (takerT, makerT)


  -- let long tuple  = (/(_,n,s) -> (x, y) where x = map sum (n) where y = fst (s))
  -- let short tuple = (/(_,n,s) -> (x, y) where x = map sum (n) where y = fst (s))
  --takeOutOfFuture = undefined

        {-
                      secondGenerator :: [Int] -> [Int] -> (FutureInfo,FutureInfo) -> IO (TakerTuple, MakerTuple)
                      secondGenerator takerLst makerLst (toTakeFromLong,toTakeFromShort) = do
                        takerSide' <- randomSide




                        return (takerT, makerT)

        -}

-- ? helper funcitons for position management

-- | check if the future file is empty
isFutureEmpty :: IO Bool
isFutureEmpty = isFileEmpty posFutureP

readFuture :: IO Transaction
readFuture = do
  futurePos <- BL.readFile posFutureP
  let future' = decode futurePos :: Maybe Transaction
  case future' of
    Nothing -> error "Error reading future file"
    Just x  -> return x


filterFuture :: String -> Transaction -> FutureInfo
filterFuture pos transaction = filter (\(_, _, s) -> s == pos) (future transaction)

-- // end of position future

filterTuple :: String -> [(Int, String)] -> [(Int, String)]
filterTuple pos = filter (\(_, s) -> s == pos)

filterFutureAmount :: [(Int, String)] -> FutureInfo -> FutureInfo
filterFutureAmount [] futureInfo = futureInfo
filterFutureAmount _ [] = []
filterFutureAmount ((n, s) : ns) ((liq, amt, sid) : futureInfo)
    | n < amt = filterFutureAmount ns ((liq, amt - n, sid) : futureInfo)
    | otherwise = filterFutureAmount ns ((liq, amt, sid) : filterFutureAmount [(n - amt, s)] futureInfo)


filterFutureClose :: Position -> (FutureInfo, FutureInfo) -> IO (FutureInfo, FutureInfo)
filterFutureClose (closingLong,closingShort) (oldLong,oldShort) {-output (newLong,newShort-} = do
  let formatedTupleLong  = filterTuple "z" closingLong
  let formatedTupleShort = filterTuple "f" closingShort
  let filteredFutureLong  = filterFutureAmount formatedTupleLong oldLong
  let filteredFutureShort = filterFutureAmount formatedTupleShort oldShort
  return (filteredFutureLong, filteredFutureShort)

isTakerLong :: String -> Bool
isTakerLong side = side == "x"
-- | this is a helper funtion for deciding the bias of the normal Run
tuplesToSides :: (TakerTuple, MakerTuple) -> Position -- long tuple, short tuple
tuplesToSides ([], makerT) = (makerT, [])
tuplesToSides ((n,s):takerT, makerT) = if isTakerLong s
                                        then ((n,s):takerT, makerT)
                                        else (makerT,(n,s):takerT)



-- ? liquidations
randomLiquidationEvent :: IO String
randomLiquidationEvent = do
  randVal <- randomRIO (0, 9) :: IO Int
  when (stopProb > 9) $ error ("maxStop prob is 9 you have: " ++ show stopProb)
  return $ if randVal < stopProb then "stp" else "liq"

liquidationDuty :: FutureInfo -> FutureInfo -> Double
  -> IO ([(Int,String,String)]      -- info about the liquidation
        , (FutureInfo, FutureInfo)) -- updated futures
liquidationDuty futureInfoL futureInfoS price = do
    let liquidationFunction = mapM (\(p, n, s) ->
            (if (price <= p && s == "f") || (price >= p && s == "z")
             then randomLiquidationEvent >>= \event -> return (n, s, event)
             else return (0, "", "")))

    liquidationEventsL <- liquidationFunction futureInfoL
    putStrLn "liquidationEventsL \n\n"
    print liquidationEventsL
    liquidationEventsS <- liquidationFunction futureInfoS
    print liquidationEventsS
    let liquidationListL = filter (\(n, _, _) -> n /= 0) liquidationEventsL
    let liquidationListS = filter (\(n, _, _) -> n /= 0) liquidationEventsS
    putStrLn "\n\n\n"      -- TODO: remove
    print liquidationListL -- TODO: remove
    putStrLn "\n\n\n short"-- TODO: remove 
    print liquidationListS -- TODO: remove

    let updatedFutureInfoL = filter (\(p,_,_) -> price >= p) futureInfoL
    let updatedFutureInfoS = filter (\(p,_,_) -> price <= p) futureInfoS
    putStrLn "\n\n\n longsL:" -- TODO: remove
    print updatedFutureInfoL  -- TODO: remove
    putStrLn "\n\n\n shortsL:" -- TODO: remove
    print updatedFutureInfoS  -- TODO: remove
    return (liquidationListL ++ liquidationListS, (updatedFutureInfoL, updatedFutureInfoS))


-- ? Postion  Generatorts
-- | cycle management


firstRun :: ([Int],[Int]) -> Double
  -> IO ((FutureInfo, FutureInfo),NewPositioning) -- updated acc
firstRun (volumeSplitT, volumeSplitM)  sPrice = do
                  {-
                getNewPositionFuture isFutureEmpt volumeSplit sPrice info liquidated = do
                    if isFutureEmpt
                                        then firstRun volumeSplit sPrice
                                        else do
                                            undefined
                                            return (longliq, shortl6iq)
                -}
-- // DANGER EXPERIMENTAL CODE BELOW                       
                          -- only splits volume
                          putStrLn "\n\n\n"
                          --print vAmount >> print vSide'

                          putStrLn "\n\n\n"
                          putStrLn "taker"

                          -- volumeSplitT <- generateVolumes numTakers vAmount -- split the volume
                          if any (< 0) volumeSplitT  then error "volume split consists of a negative element" else print volumeSplitT

                          putStrLn "maker"

                          --  volumeSplitM <- generateVolumes numMakers vAmount -- split the volume
                          if any (< 0) volumeSplitM  then error "volume split consists of a negative element" else print volumeSplitM
                          -- main process
                          isFutureEmpt <- isFutureEmpty
                          ifempty      <- initGenerator volumeSplitT volumeSplitM
                          print ifempty

                          emptyFuture  <- positionFuture sPrice ifempty
                          putStrLn "\n\n\n\n"
                          print "xh1"
                          print emptyFuture
                          putStrLn "\n\n\n\n"
                          let emptyWrite = Transaction {future = emptyFuture}

                          Control.Monad.when isFutureEmpt $ BL.appendFile posFutureP (encode emptyWrite)

                          -- final
                          let (newLongsAcc, newShortsAcc) = (filterFuture "f" emptyWrite, filterFuture "z" emptyWrite)
                          print "hdh"
                          print newLongsAcc
                          print newShortsAcc
                          return ((newLongsAcc, newShortsAcc), ifempty)
{-
                          putStrLn $ "\nF fltr:\n" ++  show newLongsAcc
                          putStrLn $ "\nZ fltr:\n" ++  show newShortsAcc
-}


normalRun :: ([Int],[Int]) -> (FutureInfo, FutureInfo) -> Double
    -> IO
          ((FutureInfo , FutureInfo) -- updated acc
          , NewPositioning)       -- position list
normalRun (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFutur) sPrice = do
  newPositioning <- normalGenerator volumeSplitT volumeSplitM (oldLongFuture, oldShortFutur)
  posFut <- positionFuture sPrice newPositioning
  let converToTransaction = Transaction {future = posFut}
  let (filteredLongFuture,  filteredShortFuture) = (filterFuture "f" converToTransaction, filterFuture "z" converToTransaction)
  let orderedTuple = tuplesToSides newPositioning
  let (filteredLongTuple, filteredShortTuple) = orderedTuple
  let (newLongsAcc,newShortsAcc) = (filterFutureAmount filteredLongTuple filteredLongFuture, filterFutureAmount filteredShortTuple filteredShortFuture)

  -- put posFut into trasaction
  -- filter into Z and F
  -- make a funciton that filter x and y from taker tuple and a maker tuple
  -- pass long info & short into filterFutureAmount 
  -- updateFromOldFuture 
  -- filter the future
  -- filter the new positions as a new future
  -- return new positions

  return ((newLongsAcc, newShortsAcc), newPositioning)


-- ? position acccumulator
-- | accumulators for future info
futureAccLong :: FutureInfo
futureAccLong = [(0, 0, "")]

futureAccShort :: FutureInfo
futureAccShort = [(0, 0, "")]

initPositioningAcc :: NewPositioning
initPositioningAcc = ([],[])



-- ? processor part
recursiveList :: RecursionPass -> IO (NewPositioning,FutureInfo,FutureInfo,OrderBook, OrderBook, [BookStats])
recursiveList (posinfo,longinfo,shortinfo,[], bidBook, askBook, _, _, _, _, _, _, bookDetails) = do

    filewrites1 $ tail (reverse bookDetails)
    let writeBidBook = Book { book = bidBook }
    let writeAskBook = Book { book = askBook }
    let writePositionFuture = Transaction { future = longinfo ++ shortinfo }
    let writePositionFuture' = encode writePositionFuture
    BL.writeFile posFutureP writePositionFuture'
    BL.writeFile bidBookP (encode writeBidBook)
    BL.writeFile askBookP (encode writeAskBook)
    print $ "positioning " ++ show posinfo

    return (posinfo,longinfo,shortinfo,bidBook, askBook, bookDetails)

recursiveList (posinfo,longinfo, shortinfo, x:xs, bidBook, askBook, gen1, gen2,
    fullwallsASK, fullwallsBIDS, sPoint, takeWall, bookDetails) =

 orderbookLoop (posinfo,longinfo, shortinfo, x, bidBook, askBook, gen1
    , gen2, fullwallsASK, fullwallsBIDS, sPoint, takeWall) >>=
    \(newPosInfo,newLonginfo,newShortinfo,newBidBook, newAskBook, newBookDetails) -> do

  let (newGen1, newGen2) = (fst (split gen1), fst (split gen2))
  recursiveList (newPosInfo,newLonginfo,newShortinfo,xs,newBidBook,newAskBook,newGen1,newGen2
    ,fullwallsASK,fullwallsBIDS,sPoint,takeWall,newBookDetails:bookDetails)



orderbookLoop :: ListPass
      -> IO (NewPositioning,FutureInfo,FutureInfo,OrderBook,OrderBook,BookStats)
orderbookLoop (posinfo,longinfo,shortinfo,(vAmount,vSide'),bidBook,askBook,gen1,gen2,fullwallsASK,fullwallsBIDS,sPoint,takeWall)= do

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

                          print sPrice -- TODO: remove






                          -- ? position future
  -- | check if the future file is empty     

                          isFutureEmpt <- isFutureEmpty
                          numTakers    <- randomRIO (1, maxTakers) :: IO Int -- select how many takers
                          numMakers    <- randomRIO (1, maxMakers) :: IO Int -- select how many makers
                          volumeSplitT <- generateVolumes numTakers vAmount  -- split the volume
                          volumeSplitM <- generateVolumes numMakers vAmount  -- split the volume

                          liquidated   <- liquidationDuty longinfo shortinfo sPrice
                          print longinfo
                          print shortinfo
                          let liquidationIO = fst liquidated
                          putStrLn $ "\nliquidations: " ++ show liquidationIO


                          let liquidationsInfo = snd liquidated
                          let liquidationsInfo' = fst liquidated
                          putStrLn $ "\nliquidations2: " ++ show liquidationsInfo
                          putStrLn $ "\nliquidations3: " ++ show liquidationsInfo'
                          let longliq =  fst liquidationsInfo
                          let shortliq = snd liquidationsInfo


                          putStrLn "initRun\n"
                          initRun <- firstRun (volumeSplitT ,volumeSplitM ) sPrice
                          otherRun <- normalRun (volumeSplitT ,volumeSplitM ) (longliq, shortliq) sPrice




                          print $ "hj" ++ show isFutureEmpt
                          let newPositionFuture = if isFutureEmpt then fst initRun else fst otherRun

                          -- (TakerTuple,MakerTuple)
                          let newPositions :: NewPositioning =
                                            if isFutureEmpt
                                              then let ((taker1, maker1), (taker2, maker2)) = (snd initRun, posinfo)
                                                  in (taker1 ++ taker2, maker1 ++ maker2)
                                              else let ((taker1, maker1), (taker2, maker2)) = (snd otherRun, posinfo)
                                                  in (taker1 ++ taker2, maker1 ++ maker2)


                          -- posFuture <- positionFuture -- transaction
                          -- print isFutureEmpt
                          -- if no read the contents and move on isnotemptygenerator
                          -- if empty then write init generator consisting of only opening|+)
                          -- isemptygenerator ..
                          -- pass from isemptygenerator to positionfuture
                          -- isnotemptygenerator --> scanning for liquidation info -> paring "left over empty" with new based off of statistics
                          -- if liquidation pass as a volume
                          -- | returning the orderbook
                          return (newPositions,fst newPositionFuture, snd newPositionFuture,finalBookBid, finalBookAsk, newbookDetails)



