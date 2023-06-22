{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module PosCycle where

-- | module of utility funcitons
-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence as Seq
import Data.Foldable (toList)
import System.Random
    ( randomRIO )
import           Data.Aeson (decode)
import Control.Monad
import qualified Data.Bifunctor


-- | internal libraries
import           DataTypes
import           Filepaths
import           Lib
import           RunSettings
import           Util



-- ! POSITION FUTURE

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


normalGenerator :: [Int] -> [Int] -> (FutureInfo,FutureInfo) -> String -> IO (TakerTuple, MakerTuple)
normalGenerator takerLst makerLst (toTakeFromLong, toTakeFromShort) liqSide = do
  unless (closingProb >= 1 && closingProb <= 10) $ error "closingProb must be between 1 and 10"
  let genType = if sum takerLst + sum makerLst >= sum (map (\(_,n,_) -> n) toTakeFromLong) &&
                    sum takerLst + sum makerLst >= sum (map (\(_,n,_) -> n) toTakeFromShort)
                    then openingGen else normalGen
  genType
  where
    openingGen :: IO (TakerTuple, MakerTuple)
    openingGen = do
      takerSide' <- randomSide
      let finalTakerSide = if liqSide /= "" then liqSide else takerSide'

      let makerside = oppositeSide finalTakerSide
      let takerT = zip takerLst $ replicate (length takerLst) finalTakerSide
      let makerT = zip makerLst $ replicate (length makerLst) makerside
      return (takerT, makerT)
    normalGen :: IO (TakerTuple, MakerTuple)
    normalGen = do
      takerSide' <- randomSide
      let finalTakerSide = if liqSide /= "" then liqSide else takerSide'
      let makerside = oppositeSide finalTakerSide
      let closingSideT = if finalTakerSide == "x" then "z" else "f"
      let closingSideM = if makerside == "x" then "z" else "f"
      takerT <- mapM (\val ->
         do
        randVal <- randomRIO (1, 10) :: IO Int
        let sideT = if randVal < closingProb then closingSideT else finalTakerSide
        return (val, sideT)) takerLst
      makerT <- mapM (\val -> do
        randVal <- randomRIO (1, 10) :: IO Int
        let sideM = if randVal < closingProb then closingSideM else makerside
        return (val, sideM)) makerLst
      return (takerT, makerT)


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
-- ! FINE
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
    liquidationEventsS <- liquidationFunction futureInfoS

    let liquidationListL   = filter (\(n, _, _) -> n /= 0) liquidationEventsL
    let liquidationListS   = filter (\(n, _, _) -> n /= 0) liquidationEventsS
    let updatedFutureInfoL = filter (\(p,_,_) -> price <= p) futureInfoL
    let updatedFutureInfoS = filter (\(p,_,_) -> price >= p) futureInfoS

    -- filter the updatedFutureInfoL from futureInfoL
    let newFutureInfoL = filter (`notElem` updatedFutureInfoL) futureInfoL

    -- filter the updatedFutureInfoS from futureInfoS
    let newFutureInfoS = filter (`notElem` updatedFutureInfoS) futureInfoS

    return (liquidationListL ++ liquidationListS, (newFutureInfoL, newFutureInfoS))
-- ! fine

-- ? Postion  Generatorts
-- | cycle management
-- ? PUTTING IT ALL TOGETHER

-- ! bug
normalRun :: ([Int],[Int]) -> (FutureInfo, FutureInfo) -> NewPositioning -> Double -> String
    -> IO ((  FutureInfo              -- # updated accumulator long
            , FutureInfo)             -- # updated accumulator short
            , NewPositioning)         -- # updated accumulator positions

                                                                             {-
# FUNCTION EXPLINATION

input: `([Int],[Int]) -> (FutureInfo, FutureInfo) -> NewPositioning -> Double`
where: `split of volume for taker and maker -> old future (acc) -> old Positions (acc)
          current price

  PROCESS: 

  1. generate new positioning from the volume list
  , this is being done by NormalGenerator
  , note that this takes the old future accumulators as well
  , hence if there are no pre\vious positions that should close
  , we generate them, and if there are, we take them into an account.
  
  2. generate new future:
  , the future stands for positions with closing mandate 
  , (z -> close short ,  f -> close long)
  , this is being done by positionFuture, where we pass the current price
  , as well as the transaction from the step 1.
  
  3. filter out the old future
  , then we split the future into:
  , closing long & closing short
  , we split the transaction into 
  , closing long & closing short
  , we take out the closing long from the old long future 
  , and the closing short from the old short future

  4. return the new future and positioning
  , finally a transaction is returned
  , the new future coming out of the transaction is returned
  , updated old future is returned

output: ` updated accumulators for FUTURE
       -> New position accumulators      `
                                                                              -}

normalRun (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFuture) oldPositions sPrice liqSide = do

  newPositioning <- normalGenerator volumeSplitT volumeSplitM (oldLongFuture, oldShortFuture) liqSide
  posFut <- positionFuture sPrice newPositioning

  let converToTransaction = Transaction {future = posFut}
  let (filteredLongFuture,  filteredShortFuture)
        = (filterFuture "f" converToTransaction, filterFuture "z" converToTransaction)
  let orderedTupleNew = tuplesToSides newPositioning
  let unorderedTupleNew = newPositioning
  --let orderedTupleOld = tuplesToSides oldPositions
  let unorderedTupleOld = oldPositions
  let (longTuple,shortTuple) =  Data.Bifunctor.bimap (filterTuple "z") (filterTuple "f") orderedTupleNew
  let (filteredShortTuple, filteredLongTuple) = (longTuple,shortTuple)
  let (newShortsAcc,newLongsAcc) = -- ! changed order 
       (filterFutureAmount filteredLongTuple oldLongFuture, filterFutureAmount filteredShortTuple oldShortFuture)

  let otherRunSeq = let ((firstOld, secondOld), (firstNew, secondNew), newPos) =  ((newLongsAcc,newShortsAcc), (filteredLongFuture,filteredShortFuture),newPositioning) -- ! (filteredLongFuture,filteredShortFuture)
                                        in ((futureInfoToSeq firstOld, futureInfoToSeq secondOld),
                                            (futureInfoToSeq firstNew, futureInfoToSeq secondNew),
                                            newPos)
  -- | FutureInfo
  let updatedFutureAcc = (\((frst, fst'), (snd',scnd ), _) ->
                                    (seqToFutureInfo $ frst <> scnd,
                                    seqToFutureInfo $ fst' <> snd')) otherRunSeq
  -- | (TakerTuple,MakerTuple)
  let updatedPositionAcc :: NewPositioning =
        let ((taker1, maker1), (taker2,maker2)) = (unorderedTupleNew, unorderedTupleOld)
            takerSeq1 = Seq.fromList taker1
            takerSeq2 = Seq.fromList taker2
            makerSeq1 = Seq.fromList maker1
            makerSeq2 = Seq.fromList maker2
        in

       (toList $ takerSeq1 Seq.>< takerSeq2, toList $ makerSeq1 Seq.>< makerSeq2)

  return (updatedFutureAcc, updatedPositionAcc)




-- | cycle management
-- ? testing potenital bugs
-- TODO remove this whole function !
-- ! comment this funciton if you don't want to perform any tests
posFutureTestEnviromentHighlyDanngerous :: IO ()
posFutureTestEnviromentHighlyDanngerous = do
  -- // position management block

             -- DEFINE DATA
             let startingPric = 1000.0

                                --  TRY TO SWITHCH THE ORDER AND SEE IF BUGS OCCOURS
             let testingList = (
                                [(100,"f"),(200,"f"),(300,"y"),(150,"f")]    -- | TAKER | - buy taker
                                ,[(100,"x"),(200,"x"),(300,"x"),(150,"z")]   -- | MAKER | - sell taker -- ! that is the bug
                                )

             let futureInfo2 = [(900,100000,"f"),(1200,70000,"f"),(14000,100000,"f"),(100,70000,"f")] :: FutureInfo -- FUTURE INFO LONG
             let futureInfo1 = [(900,100000,"z"),(1200,70000,"z"),(14000,100000,"z"),(100,70000,"z")] :: FutureInfo -- FUTURE INFO SHORT

             -- already split volumes
             let volumeList1= [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]
             let volumeList2 = [100, 200, 300, 400, 500, 600, 700, 800, 900, 1000]

             -- prefiltered tuples
             let positioningTupleLong = [(10000,"f"),(90000,"f"), (50000,"f")]
             let positioningTupleShort = [(10000,"z"),(90000,"z"), (50000,"z")]



             liquidated <- liquidationDuty futureInfo2 futureInfo1 startingPric

             let liquidationInfo1 = fst liquidated
             let liquidationsInfo = snd liquidated
             let longliq  =  fst liquidationsInfo
             let shortliq = snd liquidationsInfo
             establishRunNormal <- normalRun (volumeList1, volumeList2 ) (longliq, shortliq) testingList startingPric ""

             -- IO        
             putStrLn "DEFINED DATA: \n"
             putStrLn "\nTESTING LIST: "
             print testingList
             putStrLn "\nFUTURE INFO LONG: "
             print futureInfo2
             putStrLn "\nFUTURE INFO SHORT: "
             print futureInfo1
             putStrLn "\nVOLUME LIST 1: "
             print volumeList1
             putStrLn "\nVOLUME LIST 2: "
             print volumeList2
             putStrLn "\nSTARTING PRICE: "
             print startingPric

             putStrLn "\n\nLIQUIDATION FREE FUTURE: \n"
             print liquidationsInfo
             putStrLn "\n\nAdditional liquidation info: \n"
             print liquidationInfo1

             putStrLn "\n\nRUN: \n"
             let ((newPosFutureShort, newPosFutureLong), (newPositionsLong, newPositionsShort)) = establishRunNormal
             putStrLn "\nnewPosFutureLong:\n"
             print newPosFutureLong
             putStrLn "\nnewPosFutureShort:\n"
             print newPosFutureShort
             putStrLn "\nnewPositions:\n"
             print newPositionsLong
             print newPositionsShort


