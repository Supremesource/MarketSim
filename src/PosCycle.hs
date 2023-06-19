{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module PosCycle where

-- | module of utility funcitons
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import System.Random
    ( randomRIO )
import           Data.Aeson (encode, decode)
import Control.Monad
import qualified Data.Bifunctor

-- | internal libraries
import           DataTypes
import           Filepaths
import           Lib
import           RunSettings
import           Util

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



randomSide :: IO String
randomSide = do
    randVal <- randomRIO (1, 10) :: IO Int
    unless (takerxProb >= 1 && takerxProb <= 10) $ error "takerxProb must be between 1 and 10"

    return $ if randVal < takerxProb then "x" else "y"


initGenerator :: [Int] -> [Int] -> IO (TakerTuple, MakerTuple)
initGenerator takerLst makerLst = do
  takerSide' <- randomSide
  let makerside = oppositeSide takerSide'
  let takerT = zip takerLst $ replicate (length takerLst) takerSide'
  let makerT = zip makerLst $ replicate (length makerLst) makerside
  return (takerT, makerT)



normalGenerator :: [Int] -> [Int] -> (FutureInfo,FutureInfo) -> IO (TakerTuple, MakerTuple)
normalGenerator takerLst makerLst (toTakeFromLong, toTakeFromShort) = do
  unless (closingProb >= 1 && closingProb <= 10) $ error "closingProb must be between 1 and 10"
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
        randVal <- randomRIO (1, 10) :: IO Int
        let sideT = if randVal < closingProb then closingSideT else takerSide'
        return (val, sideT)) takerLst
      makerT <- mapM (\val -> do
        randVal <- randomRIO (1, 10) :: IO Int
        let sideM = if randVal < closingProb then closingSideM else makerside
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


-- TODO : make this funciton more realistic with shuffeling emelemnts 
-- | and equal "taking out"  more than just deleting one by one
filterFutureAmount :: [(Int, String)] -- Tuple of positions to take out
                      -> FutureInfo  -- old futureInfo 
                      -> FutureInfo -- returns a new futureInfo
filterFutureAmount [] futureInfo = futureInfo
filterFutureAmount _ [] = []
filterFutureAmount ((n, s) : ns) ((liq, amt, sid) : futureInfo)
    | n < amt = filterFutureAmount ns ((liq, amt - n, sid) : futureInfo)
    | otherwise = filterFutureAmount ((n - amt, s) : ns) futureInfo


filterFutureClose :: Position -> (FutureInfo, FutureInfo) -> IO (FutureInfo, FutureInfo)
filterFutureClose (closingLong,closingShort) (oldLong,oldShort) {-output (newLong,newShort-} = do
  let formatedTupleLong  = filterTuple "z" closingLong
  let formatedTupleShort = filterTuple "f" closingShort
  let filteredFutureLong  = filterFutureAmount formatedTupleLong oldLong
  let filteredFutureShort = filterFutureAmount formatedTupleShort oldShort
  return (filteredFutureLong, filteredFutureShort)

isTakerBuying :: String -> Bool
isTakerBuying side = side == "x" || side == "z"
-- | this is a helper funtion for deciding the bias of the normal Run
tuplesToSides :: (TakerTuple, MakerTuple) -> Position -- long tuple, short tuple
tuplesToSides ([], makerT) = (makerT, [])
tuplesToSides ((n,s):takerT, makerT) = if isTakerBuying s
                                        then ((n,s):takerT, makerT)
                                        else (makerT,(n,s):takerT)



-- ? liquidations
randomLiquidationEvent :: IO String
randomLiquidationEvent = do
  randVal <- randomRIO (1, 10) :: IO Int
  unless (stopProb >= 1 && stopProb <= 10) $ error ("maxStop is 10 you have: " ++ show stopProb)
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
 
    liquidationEventsS <- liquidationFunction futureInfoS

    let liquidationListL = filter (\(n, _, _) -> n /= 0) liquidationEventsL
    let liquidationListS = filter (\(n, _, _) -> n /= 0) liquidationEventsS
    putStrLn "\n\n\n"            -- TODO: remove
   
    putStrLn "\n\n\n short"      -- TODO: remove 


    let updatedFutureInfoL = filter (\(p,_,_) -> price <= p) futureInfoL
    let updatedFutureInfoS = filter (\(p,_,_) -> price >= p) futureInfoS
    putStrLn "\n\n\n longsL:"  -- TODO: remove

    putStrLn "\n\n\n shortsL:" -- TODO: remove

    return (liquidationListL ++ liquidationListS, (updatedFutureInfoL, updatedFutureInfoS))


-- ? Postion  Generatorts
-- | cycle management

{-
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


                          -- volumeSplitT <- generateVolumes numTakers vAmount -- split the volume
                          if any (< 0) volumeSplitT  then error "volume split consists of a negative element" else print volumeSplitT

                          putStrLn "maker"

                          --  volumeSplitM <- generateVolumes numMakers vAmount -- split the volume
                          if any (< 0) volumeSplitM  then error "volume split consists of a negative element" else print volumeSplitM
                          -- main process
                          isFutureEmpt <- isFutureEmpty
                          ifempty      <- initGenerator volumeSplitT volumeSplitM
                         

                          emptyFuture  <- positionFuture sPrice ifempty
                          putStrLn "\n\n\n\n"
                      
                          putStrLn "\n\n\n\n"
                          let emptyWrite = Transaction {future = emptyFuture}

                          Control.Monad.when isFutureEmpt $ BL.appendFile posFutureP (encode emptyWrite)

                          -- final
                          let (newLongsAcc, newShortsAcc) = (filterFuture "f" emptyWrite, filterFuture "z" emptyWrite)

                          return ((newLongsAcc, newShortsAcc), ifempty)
{-
                          putStrLn $ "\nF fltr:\n" ++  show newLongsAcc
                          putStrLn $ "\nZ fltr:\n" ++  show newShortsAcc
-}
-}

-- ? PUTTING IT ALL TOGETHER

normalRun :: ([Int],[Int]) -> (FutureInfo, FutureInfo) -> Double
    -> IO
          ( (FutureInfo , FutureInfo) -- updated old acc
          , (FutureInfo , FutureInfo) -- updated new acc
          ,  NewPositioning)          -- position list
normalRun (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFuture) sPrice = do

  newPositioning <- normalGenerator volumeSplitT volumeSplitM (oldLongFuture, oldShortFuture) -- THIS works
  posFut <- positionFuture sPrice newPositioning -- THIS WORKS, this should be added to the old one

  let converToTransaction = Transaction {future = posFut}
  let (filteredLongFuture,  filteredShortFuture) = (filterFuture "f" converToTransaction, filterFuture "z" converToTransaction) -- THIS WORKS


  let orderedTuple = tuplesToSides newPositioning -- ! problem here
  let (longTuple,shortTuple) =  Data.Bifunctor.bimap (filterTuple "z") (filterTuple "f") orderedTuple
 

  -- TODO make a filtered tuple funciton -> recieves z and f mixed with x and y 
  -- TODO returns only z and f in order z first then f


  let (filteredShortTuple, filteredLongTuple) = (longTuple,shortTuple)

  let (newLongsAcc,newShortsAcc) = (filterFutureAmount filteredLongTuple oldLongFuture, filterFutureAmount filteredShortTuple oldShortFuture)

 -- let (finalNewLongsAcc,finalNewShortsAcc) = (filteredLongFuture ++ newLongsAcc , filteredShortFuture ++ newShortsAcc )
  -- let (addToAccLong,addToAccShort) = (newLongsAcc ++ filteredLongFuture, newShortsAcc ++ filteredShortFuture)
  --test <- newPositioning
  -- put posFut into trasaction
  -- filter into Z and F
  -- make a funciton that filter x and y from taker tuple and a maker tuple
  -- pass long info & short into filterFutureAmount 
  -- updateFromOldFuture 
  -- filter the future
  -- filter the new positions as a new future
  -- return new positions

  return ((newLongsAcc,newShortsAcc), (filteredLongFuture,filteredShortFuture),newPositioning)

posFutureTestEnviromentHighlyDanngerous :: IO ()
posFutureTestEnviromentHighlyDanngerous = do
  -- // position management block
            -- |
             let startingPric = 1000.0
             let testingList = ([(100,"x"),(200,"x"),(300,"x"),(150,"z")],  -- TAKER
                                [(100,"f"),(200,"f"),(300,"y"),(150,"f")])  -- MAKER

             let futureInfo2 = [(900,100000,"f"),(1200,70000,"f"),(14000,100000,"f"),(100,70000,"f")] :: FutureInfo -- FUTURE INFO LONG
             let futureInfo1 = [(900,100000,"z"),(1200,70000,"z"),(14000,100000,"z"),(100,70000,"z")] :: FutureInfo -- FUTURE INFO SHORT
             
             -- already split volumes
             let volumeList1= [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]
             let volumeList2 = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]

             -- prefiltered tuples
             let positioningTupleLong = [(10000,"f"),(90000,"f"), (50000,"f")]
             let positioningTupleShort = [(10000,"z"),(90000,"z"), (50000,"z")]

             liquidated <- liquidationDuty futureInfo1 futureInfo2 startingPric
             let liquidationsInfo = snd liquidated
             putStrLn "liquidationsInfo: "
             print liquidationsInfo
             let longliq =  fst liquidationsInfo
             let shortliq = snd liquidationsInfo
             establishRunNormal <- normalRun (volumeList1, volumeList2 )(longliq, shortliq) startingPric 
             
             let otherRunSeq = let ((firstOld, secondOld), (firstNew, secondNew), newPositioning) = establishRunNormal
                          in ((futureInfoToSeq firstOld, futureInfoToSeq secondOld), 
                              (futureInfoToSeq firstNew, futureInfoToSeq secondNew), 
                              newPositioning)

             let concatOtherRun = (\((frst, fst'), (snd',scnd ), _) -> 
                                                          (seqToFutureInfo $ frst <> scnd, 
                                                          seqToFutureInfo $ fst' <> snd')) otherRunSeq

             

             let newPositions :: NewPositioning =        -- ! problem within                                 
                                    let ((taker1, maker1), (maker2,taker2)) = ((\(_, _, laste) -> laste) establishRunNormal, testingList)
                                          in (taker1 ++ taker2, maker1 ++ maker2)
    
    
             putStrLn "\n\n\n\n establishRunNormal:"
             print  establishRunNormal

             putStrLn "\n\n\n\n otherRunSeq:" 
             print concatOtherRun

             putStrLn "\n\n\n\n Position:" 
             print newPositions
               
         


            
        