{-# OPTIONS_GHC -Wno-missing-export-lists #-}
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
module PosCycle
{-- ! DESCRIPTION

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
 We can move onto the second runProgram through our recrusive function.

 / runProgram 2.
 Now we know which positions can be closed as well as some liquidation prices
 so same as in runProgram 1 we send our volume list into the orderbook -> change the price
 after we change the price we check if some position in that liquidation list
 should have been liquidated, if so we take it out of our basked of positions
 that can close and send the volume as Z or F (depends if it was a long or short
 ,who got liquidated). Now this `liquidation duty` transaction has to be pared
 with a counterparty of where we generate a opposite site that can consist of
 more than one position so we split the counterside volume and assign either
 new positions as a counterparty or exiting ones from our basket.

 Now we know there is no liquidation in our basked and we will make sense
 of the volume list that came to our funciton in the first place.
 This is done by taking the volume list splitting it (same as the initial runProgram)
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
 can move onto the frontend/ do another simulation runProgram with different settings.

-}
where


-- | importing external libraries
import qualified Data.ByteString.Lazy as BL
import qualified Data.Sequence        as Seq
import           Control.Monad
import           Data.Aeson           (decode)
import qualified Data.Bifunctor
import           System.Random        (randomRIO)
import           Prelude hiding (seq)
import           Data.Monoid
import           Data.Sequence        (Seq, ViewL ((:<)), (<|), (|>), (><))
import           System.Random.Shuffle (shuffleM)
-- | internal libraries
import           DataTypes
import           Filepaths
import           Lib
import           RunSettings
import           Util
import           Statistics


-- ? funcitons for the position future
-- PACKAGE CLOSING CONVERSION X & Y TO F & Z
closingConversion :: (TakerPositions, MakerPositions) -> (TakerPositions, MakerPositions)
closingConversion (takers, makers)
  | hasBothXY takers = error "Unsupported tuple format in takers"
  | hasBothXY makers = error "Unsupported tuple format in makers"
  | otherwise =
    (closingconvert filterTakers takers, closingconvert filterMakers makers)
  where
    isValidTuple (_, side) = side == "x" || side == "y"
    filterTakers           = filter isValidTuple
    filterMakers           = filter isValidTuple
    closingconvert f t =
      map
        (\(x, y) ->
           ( x
           , if y == "x"
               then "f"
               else "z"))
        (f t)
    hasBothXY xs = "x" `elem` map snd xs && "y" `elem` map snd xs


-- TODO better leverage statistics
positionFuture :: Double -> (TakerPositions, MakerPositions) -> IO ClosePositionData
positionFuture price' (taker, maker) = do
  let (takerConver, makerConvert) = closingConversion (taker, maker)
  let concatTakerMaker = takerConver ++ makerConvert
  mapM calcPosition concatTakerMaker
  where
    calcPosition (amt, side) = do
      leverage <- takenLeverage
      let liquidationPrice
            | leverage /= 1 && side == "z" =
              (price' / fromIntegral leverage) + price'
            | leverage /= 1 && side == "f" =
              price' - (price' / fromIntegral leverage)
            | leverage == 1 && side == "z" = 2 * price'
            | otherwise                    = 0
      return (liquidationPrice, amt, side)

oppositeSide :: String -> String
oppositeSide side =
  if side == "x" || side == "z"
    then "y"
    else "x"

randomSide :: IO String
randomSide = do
  -- > RANDOMNESS <
  randVal <- randomRIO (1, 10) :: IO Int
  unless (takerxProb >= 1 && takerxProb <= 10) $
    error "takerxProb must be between 1 and 10"
  return $
    if randVal < takerxProb
      then "x"
      else "y"

initGenerator :: [Int] -> [Int] -> IO (TakerPositions, MakerPositions)
initGenerator takerLst makerLst = do
  
  takerSide' <- randomSide
  let makerside = oppositeSide takerSide'
  let takerT = zip takerLst $ replicate (length takerLst) takerSide'
  let makerT = zip makerLst $ replicate (length makerLst) makerside
  return (takerT, makerT)

type SeqClosePositionData = (Seq (Double, Int, String), Seq (Double, Int, String))

normalGenerator ::
     [Int] -> [Int] -> SeqClosePositionData -> String -> IO (TakerPositions, MakerPositions)
normalGenerator takerLst makerLst (toTakeFromLong, toTakeFromShort) liqSide = do
  unless (closingProb >= 1 && closingProb <= 10) $
    error "closingProb must be between 1 and 10"
  let genType =
        if sum takerLst + sum makerLst >=
           getSum (foldMap (\(_, n, _) -> Sum n) toTakeFromLong) &&
           sum takerLst + sum makerLst >=
           getSum (foldMap (\(_, n, _) -> Sum n) toTakeFromShort)
          then openingGen
          else normalGen
  genType
  where
    openingGen :: IO (TakerPositions, MakerPositions)
    openingGen = do
      takerSide' <- randomSide

      let finalTakerSide =
            if liqSide /= ""
              then liqSide
              else takerSide'
      let editedTakerLst =
            if liqSide /= ""
              then sumList takerLst
              else takerLst
      let makerside = oppositeSide finalTakerSide
      let takerT =
            zip editedTakerLst $ replicate (length takerLst) finalTakerSide
      let makerT = zip makerLst $ replicate (length makerLst) makerside
      return (takerT, makerT)
    normalGen :: IO (TakerPositions, MakerPositions)
    normalGen = do
      takerSide' <- randomSide

      let finalTakerSide =
            if liqSide /= ""
              then liqSide
              else takerSide'
      let editedTakerLst =
            if liqSide /= ""
              then sumList takerLst
              else takerLst
      let makerside = oppositeSide finalTakerSide
      let closingSideT
            | finalTakerSide == "x" && liqSide == "" = "z"
            | finalTakerSide == "y" && liqSide == "" = "f"
            | otherwise                              = finalTakerSide
      let closingSideM =
            if makerside == "x"
              then "z"
              else "f"
      takerT <-
        mapM
          (\val -> do
             -- > RANDOMNESS <
             randVal <- randomRIO (1, 10) :: IO Int
             let sideT =
                   if randVal < closingProb
                     then closingSideT
                     else finalTakerSide
             return (val, sideT))
          editedTakerLst
      makerT <-
        mapM
          (\val -> do
             -- > RANDOMNESS <
             randVal <- randomRIO (1, 10) :: IO Int
             let sideM =
                   if randVal < closingProb
                     then closingSideM
                     else makerside
             return (val, sideM))
          makerLst
      return (takerT, makerT)


-- ? helper funcitons for position management

-- | check if the future file is empty
isCloseDataEmpty :: IO Bool
isCloseDataEmpty = isFileEmpty posCloseDatP

readClosePos :: IO TransactionFut
readClosePos = do
  futurePos <- BL.readFile posCloseDatP
  let future' = decode futurePos :: Maybe TransactionFut
  case future' of
    Nothing -> error "Error reading future file"
    Just x  -> return x

filterClosePos :: {-String ->-} String -> TransactionFut -> ClosePositionData
filterClosePos {-liq-} pos transaction =
 {- if liq == "no" then -}filter (\(_, _, s) -> s == pos) (future transaction) --else future transaction


-- // end of position future
filterTuple :: String -> [(Int, String)] -> [(Int, String)]
filterTuple pos = filter (\(_, s) -> s == pos)

type SeqFuture = Seq (Double, Int, String)

-- | Function to get third element from 3-tuple
thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-- | helper funciton for filter future amount checking if the pass is correct
-- | meaning the future is filtered to the same element
allThirdEqual :: Seq (a, b, String) -> Bool
allThirdEqual seq = 
  case Seq.viewl seq of
    Seq.EmptyL -> True
    ((_, _, x) Seq.:< xs)
      | x == "" -> allThirdEqual xs
      | otherwise -> allOtherThirdEqual xs x
-- | helper to allThirdEqual 
allOtherThirdEqual :: Seq (a, b, String) -> String -> Bool
allOtherThirdEqual seq c = 
  case Seq.viewl seq of
    Seq.EmptyL -> True
    ((_, _, x) Seq.:< xs)
      | x == "" || x == c -> allOtherThirdEqual xs c
      | otherwise -> False

splitAmountToRandomList :: Int -> IO [Int]
splitAmountToRandomList 0 = return [0]
splitAmountToRandomList 1 = return [1]
splitAmountToRandomList n = do
     -- Change this to adjust the number of chunks
    chunkCount <- case () of  _
                                | n < 1000                -> return 5
                                | n < 5000                -> return 10
                                
                                -- > RANDOMNESS <
                                | n > 10000 && n < 20000  -> randomRIO (10,20)                                   
                                | otherwise               -> randomRIO (20, 30)
    case () of _ 
                    | n < chunkCount -> return [n]
                    | otherwise -> do
                      let (q, r) = n `divMod` chunkCount
                      -- > RANDOMNESS <
                      chunks <- replicateM (chunkCount - 1) (randomRIO (q `div` 2, q * 3 `div` 2))
                      let lastChunk = n - sum chunks
                      -- > RANDOMNESS <
                      shuffledChunks <- shuffleM (lastChunk : chunks) 
                      -- | shuffle happens only when returning (performance saver)
                      return $ filter (/= 0) shuffledChunks

filterCloseAmount ::
     [(Int, String)]     -- Tuple of positions to take out
  -> SeqFuture           -- old closePosData
  -> IO SeqFuture        -- returns a new closePosData
filterCloseAmount [] closePosData              = return closePosData
filterCloseAmount ((num, str):ns) closePosData = do    
    -- | splitting transaction volume into smaller parts such that exiting 
    --   positions are handeled better
    -- & random part
    transactionVolSplit    <- splitAmountToRandomList num 
    adjustedFutureToVolume <- filterFutureVol transactionVolSplit closePosData ns    
    filterCloseAmount ns adjustedFutureToVolume
    where        
      filterFutureVol [] closePosData' _        = return closePosData'
      filterFutureVol _ Seq.Empty     _         = return Seq.Empty        
      filterFutureVol (x:xs) closePosData' ns'  = do 
        case Seq.viewl closePosData' of
          Seq.EmptyL -> return Seq.Empty
          (liq, amt, sid) :< rest -> 
              case () of _
                          | x < amt
                              -> do
                                  let updatedFutureInfo = rest |> (liq, amt - x, sid)
                                  _ <- filterCloseAmount ns' updatedFutureInfo
                                  filterFutureVol xs updatedFutureInfo ns'               
                          | otherwise
                              -> do                         
                                  let updatedFutureInfo = rest 
                                  let newX = x - amt                              
                                  _ <- filterCloseAmount ((newX, str) : ns') updatedFutureInfo -- rest                                                           
                                  filterFutureVol (newX:xs) updatedFutureInfo ns'

-- Helper function to check if all elements of a list are equal
allEqual :: Eq a => [a] -> Bool
allEqual xs = all (== head xs) (tail xs)

filterFutureClose ::
     Position -> (SeqFuture, SeqFuture) -> IO (SeqFuture, SeqFuture)
filterFutureClose (closingLong, closingShort) (oldLong, oldShort) {-output (newLong,newShort-}
 = do
  let formatedTupleLong = filterTuple "z" closingLong
  let formatedTupleShort = filterTuple "f" closingShort
  filteredFutureLong  <- filterCloseAmount formatedTupleLong oldLong
  filteredFutureShort <- filterCloseAmount formatedTupleShort oldShort
  return (filteredFutureLong, filteredFutureShort)

isTakerBuying :: String -> Bool
isTakerBuying side = side == "x" || side == "z"

-- | this is a helper funtion for deciding the bias of the normal runProgram
tuplesToSides :: (TakerPositions, MakerPositions) -> Position -- long tuple, short tuple
tuplesToSides ([], makerT) = (makerT, [])
tuplesToSides ((n, s):takerT, makerT) =
  if isTakerBuying s
    then ((n, s) : takerT, makerT)
    else (makerT, (n, s) : takerT)


liquidationDuty ::
     SeqFuture
  -> SeqFuture
  -> Double
  -> IO ( Seq (Int, String, String) -- info about the liquidation
        , (SeqFuture, SeqFuture)    -- updated futures
         )
liquidationDuty futureInfoL futureInfoS price' = do
  let liquidationFunction =
        mapM
          (\(p, n, s) ->
             (if (price' <= p && s == "f") || (price' >= p && s == "z")
                then randomLiquidationEvent >>= \event -> return (n, s, event)
                else return (0, "", "")))
  liquidationEventsL <- liquidationFunction futureInfoL
  liquidationEventsS <- liquidationFunction futureInfoS
  let liquidationListL = Seq.filter (\(n, _, _) -> n /= 0) liquidationEventsL
  let liquidationListS = Seq.filter (\(n, _, _) -> n /= 0) liquidationEventsS
  let updatedFutureInfoL = Seq.filter (\(p, _, _) -> price' <= p) futureInfoL
  let updatedFutureInfoS = Seq.filter (\(p, _, _) -> price' >= p) futureInfoS

-- filter the updatedFutureInfoL from futureInfoL
  let newFutureInfoL = Seq.filter (`notElem` updatedFutureInfoL) futureInfoL

-- filter the updatedFutureInfoS from futureInfoS
  let newFutureInfoS = Seq.filter (`notElem` updatedFutureInfoS) futureInfoS
  return
    (liquidationListL >< liquidationListS, (newFutureInfoL, newFutureInfoS))


{-
# FUNCTION EXPLINATION

input: `([Int],[Int]) -> (ClosePositionData, ClosePositionData) -> NewPositioning -> Double`
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
-- ? PUTTING ALL FUNCTIONS ABOVE TOGETHER
normalrunProgram ::
     ([Int], [Int])
  -> SeqClosePositionData
  -> Double
  -> String
  -> IO ( SeqClosePositionData    -- # updated accumulator short # updated accumulator long
        , NewPositioning ) -- # updated accumulator positions
normalrunProgram (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFuture) -- TODO TAKE OUT oldPositions
                                               sPrice liqSide = do
  newPositioning <-
    normalGenerator
      volumeSplitT
      volumeSplitM
      (oldLongFuture, oldShortFuture)
      liqSide
  posFut <- positionFuture sPrice newPositioning
 -- let adjustedLiquidation = if liqSide == "" then "no" else "yes"
  let converToTransaction = TransactionFut {future = posFut}
  let (filteredLongFuture, filteredShortFuture) =
        ( filterClosePos {-adjustedLiquidation-}  "f" converToTransaction
        , filterClosePos {-adjustedLiquidation-}  "z" converToTransaction)
  let orderedTupleNew = tuplesToSides newPositioning
  let unorderedPosNew = newPositioning
  let (longTuple, shortTuple) =
        Data.Bifunctor.bimap (filterTuple "z") (filterTuple "f") orderedTupleNew
  let (filteredShortTuple, filteredLongTuple) = (longTuple, shortTuple)
  
  newLongsAcc <- filterCloseAmount filteredLongTuple oldLongFuture
  newShortsAcc  <- filterCloseAmount filteredShortTuple oldShortFuture   
  let futureToSeq =
        let ((firstOld, secondOld), (firstNew, secondNew), newPos) =
              ( (newShortsAcc, newLongsAcc)
              , (filteredLongFuture, filteredShortFuture)
              , newPositioning) 
         in ( (firstOld, secondOld)
            , (futureInfoToSeq firstNew, futureInfoToSeq secondNew)
            , newPos)
  -- | ClosePositionData
  let updatedFutureAcc =
        (\((frst, fst'), (snd', scnd), _) -> (frst <> scnd, fst' <> snd'))
          futureToSeq
  return
    ( updatedFutureAcc -- # already concat to new accumulator
    , unorderedPosNew -- # returning in a raw form to positionCycle
     )
