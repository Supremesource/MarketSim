{-# OPTIONS_GHC -w #-}    
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
module NRandomFunc 
{- 
--! Description
--# module converts random functions into strict (non-random) ones (for testing purpouses)
  -}
where

-- | External libraries,
import System.Random
import Test.Hspec
import Test.QuickCheck ()
import Control.Exception (evaluate)
import System.Random
import Data.Maybe
import Control.Exception (try, ErrorCall)
import Control.Monad
import Data.Sequence   (fromList, (><))
import Data.Monoid
import Prelude hiding (seq)
import qualified Data.Sequence        as Seq
import           Data.Sequence        (Seq (Empty), ViewL ((:<), EmptyL), (<|), (|>), (><))
import Data.Aeson (FromJSON, ToJSON, eitherDecode)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy as B
import qualified Data.Bifunctor
import Debug.Trace


-- | Internal libraries
import Lib
import RunSettings
import Util
import DataTypes
import PosCycle
import Statistics
import Generator
import Filepaths

closingProb = 7
stopProb = 7

type NrndSeqClosePositionData = (Seq (Double, Int, String), Seq (Double, Int, String))
type NrndSeqFuture = Seq (Double, Int, String)
type NrdClosePositionData = (Double, Int, String)

{-
-- $ HELPER FUNCITONS FOR TESTS
-- ? testsPosCycle functions 
  -- ##################################################
  -- # non-random positionFuture (leverage terms)     #
  -- ##################################################
takenLeverageNonRandom :: IO Int
takenLeverageNonRandom = do
  x <- randomRIO (1, 10) :: IO Int
  return $
    case x of
      _
        | x >= 1 && x <= 10 -> 5
        | otherwise -> error "something went wrong in non-random takenLeverage"

-- TODO better leverage statistics
positionFutureNonRandom :: Double -> (TakerPositions, MakerPositions) -> IO [NrdClosePositionData]
positionFutureNonRandom price' (taker, maker) = do
  let (takerConver, makerConvert) = closingConversion (taker, maker)
  let concatTakerMaker = takerConver ++ makerConvert
  mapM calcPosition concatTakerMaker
  where
    calcPosition (amt, side) = do
      leverage <- takenLeverageNonRandom
      let liquidationPrice
            | leverage /= 1 && side == "z" =
              (price' / fromIntegral leverage) + price'
            | leverage /= 1 && side == "f" =
              price' - (price' / fromIntegral leverage)
            | leverage == 1 && side == "z" = 2 * price'
            | otherwise                    = 0
      return (liquidationPrice, amt, side)

-- ? init generator with predetermined result
nonRandominitGenerator :: [Int] -> [Int] -> IO (TakerPositions, MakerPositions)
nonRandominitGenerator takerLst makerLst = do
  let takerSide' = "x"
  let makerside = oppositeSide takerSide'
  let takerT = zip takerLst $ replicate (length takerLst) takerSide'
  let makerT = zip makerLst $ replicate (length makerLst) makerside
  return (takerT, makerT)


nonRandomSplitAmountToRandomList :: Int -> IO [Int]
nonRandomSplitAmountToRandomList 0 = return [0]
nonRandomSplitAmountToRandomList 1 = return [1]
nonRandomSplitAmountToRandomList n = do
     -- Change this to adjust the number of chunks
    let chunkCount = 10
    case () of _
                    | n < chunkCount -> return [n]
                    | otherwise -> do
                      let (q, _) = n `divMod` chunkCount
                      -- > RANDOMNESS <
                      -- pre `set`
                      chunks <- replicateM (chunkCount - 1) (return (q `div` 2))
                      let lastChunk = n - sum chunks
                      -- | shuffle happens only when returning (performance saver)
                      return $ filter (/= 0) (lastChunk : chunks)


nonRandomFilterCloseAmount ::
     [(Int, String)]     -- Tuple of positions to take out
  -> NrndSeqFuture           -- old closePosData
  -> IO NrndSeqFuture        -- returns a new closePosData
nonRandomFilterCloseAmount [] closePosData          = return closePosData
nonRandomFilterCloseAmount ((num, _):ns) closePosData = do    
    -- | splitting transaction volume into smaller parts such that exiting 
    -- positions are handeled better
    transactionVolSplit    <- nonRandomSplitAmountToRandomList num 
    adjustedFutureToVolume <- filterFutureVol transactionVolSplit closePosData ns    
    
    nonRandomFilterCloseAmount ns adjustedFutureToVolume
    where        
      filterFutureVol [] closePosData' _      = return closePosData'
      filterFutureVol _ Seq.Empty     _        = return Seq.Empty        
      filterFutureVol (x:xs) closePosData' ns' =
        case Seq.viewl closePosData' of
            Seq.EmptyL -> return Seq.Empty
            ((liq, amt, sid) :< rest) -> do          
                case () of _
                            | x < amt -> do
                                let updatedFutureInfo = rest |> (liq, amt - x, sid)
                                filterFutureVol xs updatedFutureInfo ns'               
                            | otherwise -> do
                                let updatedFutureInfo = rest 
                                let newX = x - amt
                                filterFutureVol (newX:xs) updatedFutureInfo ns'





nonRandomizedfilterFutureClose ::
     Position -> (NrndSeqFuture, NrndSeqFuture) -> IO (NrndSeqFuture, NrndSeqFuture)
nonRandomizedfilterFutureClose (closingLong, closingShort) (oldLong, oldShort) {-output (newLong,newShort-}
  = do
  let formatedTupleLong   = filterTuple "z" closingLong
  let formatedTupleShort  = filterTuple "f" closingShort
  filteredFutureLong      <- nonRandomFilterCloseAmount formatedTupleLong oldLong
  filteredFutureShort     <- nonRandomFilterCloseAmount formatedTupleShort oldShort
  return (filteredFutureLong, filteredFutureShort)


nonRandomNormalGenerator ::
     [Int] -> [Int] ->NrndSeqClosePositionData -> String -> String -> IO (TakerPositions, MakerPositions)
nonRandomNormalGenerator takerLst makerLst (toTakeFromLong, toTakeFromShort) liqSide nonRandomSide = do
  unless (closingProbLong >= 1 && closingProbLong <= 10 && closingProbShort >= 1 && closingProbShort <= 10) $
    error "closingProb must be between 1 and 10"
  
  let genType =
        if sum takerLst + sum makerLst >=
           getSum (foldMap (\(_, n, _) -> Sum n) toTakeFromLong) &&
           sum takerLst + sum makerLst >=
           getSum (foldMap (\(_, n, _) -> Sum n) toTakeFromShort)
          then openingGen nonRandomSide
          else normalGen nonRandomSide
  genType
  where
    openingGen :: String -> IO (TakerPositions, MakerPositions)
    openingGen nonRandomSide = do
      let takerSide' = nonRandomSide
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
  
    normalGen :: String -> IO (TakerPositions, MakerPositions)
    normalGen nonRandomSide = do
      let takerSide' = nonRandomSide   
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
             randVal <- randomRIO (10, 10) :: IO Int
             let sideT =
                   if randVal < closingProb
                     then closingSideT
                     else finalTakerSide
             return (val, sideT))
          editedTakerLst
      makerT <-
        mapM
          (\val -> do
             randVal <- randomRIO (10, 10) :: IO Int
             let sideM =
                   if randVal < closingProb
                     then closingSideM
                     else makerside
             return (val, sideM))
          makerLst
      return (takerT, makerT)    

nonRandomLiquidationEvent :: IO String
nonRandomLiquidationEvent = do
  randVal <- randomRIO (1, 7) :: IO Int
  unless (stopProb >= 1 && stopProb <= 10) $
    error ("maxStop is 10 you have: " ++ show stopProb)
  return $
    if randVal < 8
      then "stp"
      else "liq"

nonRandomLiquidationDuty ::
     NrndSeqFuture
  -> NrndSeqFuture
  -> Double
  -> IO ( 
      Seq (Int, String, String)     -- info about the liquidation
        , (NrndSeqFuture, NrndSeqFuture)    -- updated futures
         )
nonRandomLiquidationDuty futureInfoL futureInfoS price' = do
  let liquidationFunction =
        mapM
          (\(p, n, s) ->
             (if (price' <= p && s == "f") || (price' >= p && s == "z")
                then nonRandomLiquidationEvent >>= \event -> return (n, s, event)
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


data TransactionFutNrd = TransactionFutNrd {
    future' :: NrdClosePositionData
  } deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via JSONConfig TransactionFut

nonRandomNormalrunProgram ::
     ([Int], [Int])
  ->NrndSeqClosePositionData
  -> Double
  -> String
  -> String 
  -> IO (NrndSeqClosePositionData    -- # updated accumulator short # updated accumulator long
        , NewPositioning )        -- # updated accumulator positions
         
nonRandomNormalrunProgram (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFuture) -- TODO TAKE OUT oldPositions
                                               sPrice liqSide nonRandomSide = do
  newPositioning <-
    nonRandomNormalGenerator
            volumeSplitT
            volumeSplitM
            (oldLongFuture, oldShortFuture)
            liqSide
            nonRandomSide

  posFut <- positionFutureNonRandom sPrice newPositioning 
  
  let converToTransaction = TransactionFutNrd {future' = posFut}
  let (filteredLongFuture, filteredShortFuture) =
        ( filterClosePos {-adjustedLiquidation-}  "f" converToTransaction
        , filterClosePos {-adjustedLiquidation-}  "z" converToTransaction)

  let orderedTupleNew   = tuplesToSides newPositioning
  let unorderedPosNew   = newPositioning
  let (longTuple, shortTuple) =
        Data.Bifunctor.bimap (filterTuple "z") (filterTuple "f") orderedTupleNew
  let (filteredShortTuple, filteredLongTuple) = (longTuple, shortTuple)
 
  newLongsAcc  <- nonRandomFilterCloseAmount filteredLongTuple oldLongFuture
  newShortsAcc <- nonRandomFilterCloseAmount filteredShortTuple oldShortFuture
  
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
    ( updatedFutureAcc  -- # already concat to new accumulator
    , unorderedPosNew   -- # returning in a raw form to positionCycle
     )
-}

readPositions :: FilePath -> IO (Either String [FileWritePosition])
readPositions filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

readBookInfo :: FilePath -> IO (Either String [FileWriteBook])
readBookInfo filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

readPosFuture :: FilePath -> IO (Either String TransactionFut)
readPosFuture filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

{-
nonBuggyVolumeSplit :: Int -> Int -> IO [Int]
generateVolumes _ 0 = return [0]
generateVolumes _ 1 = return [1]
nonBuggyVolumeSplit numPos totalVolume' = do
  Control.Monad.when (numPos < 1) $
    error $ red "Number of transactions cannot be less than 1"
  Control.Monad.when (totalVolume' < 1) $
    error $ red "Total volume cannot be less than 1"
  Control.Monad.when (totalVolume' < numPos) $
    error $ red "Total volume cannot be less than number of transactions"
  let maybeProceed = if (fromIntegral totalVolume' ) /  (fromIntegral(numPos * 2)) < 1
        then True
        else False
        
  result = if maybeProceed == True then return [totalVolume'] else 
    generateListAux [] numPos totalVolume'
      where 
        generateListAux :: [Int] -> Int -> Int -> IO [Int]
        generateListAux [] _ _ = return []
        generateListAux accList nPos totVol = do
          randomizerL <- randomRIO (1, 10) :: IO Int
          let customLow = if randomizerL =< 5 then (randomRIO (nPos, totVol `div` nPos)) else nPos
          randomizerH <- randomRIO (1, 10) :: IO Int
          let customHigh  = if ranomizerH =< 3 then round (1.5 * (fromIntegral (totVol `div` nPos))) :: Int else totVol `div` nPos
          toReplicate = if sum chunks + (nPos * 2) > totVol then False else True
          chunks <- if toReplicate /= True then replicateM (nPos - 1) (randomRIO (customLow, customHigh)) else -- return the result it has currently been replicated at
          let lastChunk = totVol - sum chunks

-}


positionAdjustement :: Int -> Int -> Int 
positionAdjustement 0 _ = 0
positionAdjustement 1 _ = 1
positionAdjustement pos vol | vol > (2 * (pos * pos)) = pos
                            | otherwise =  positionAdjustement (pos - 1) vol 
  
generateVolumesX :: Int -> Int -> IO [Int]
generateVolumesX npos ttlVol 
      | npos < 1      = error "Number of transactions cannot be less than 1"
      | ttlVol < 1    = error "Total volume cannot be less than 1"
      | ttlVol < npos = error "Total volume cannot be less than number of transactions"
      | npos == 1     = return [ttlVol]
      | otherwise     = do
          let adjustedNpos = positionAdjustement npos ttlVol
          trace ("adjustedNpos: " ++ show adjustedNpos) $ return ()
          let toReplicate  = adjustedNpos - 1
          -- layer 1
          a <- replicateM (toReplicate + 1) (randomRIO (adjustedNpos, (adjustedNpos * 2)))
          let lastChunk  = ttlVol - sum a
          -- layer 2
          b <- replicateM toReplicate (randomRIO (((lastChunk `div` toReplicate) `div` 2), (lastChunk `div` toReplicate)))
          let lastChunk2 = lastChunk - sum b
          let b' = lastChunk2 : b
          -- final
          let c = zipWith (+) a b'
          return c


{-
nonBuggyVolumeSplit :: Int -> Int -> IO [Int]
nonBuggyVolumeSplit numPos totalVolume'
  | numPos < 1 = error "Number of transactions cannot be less than 1"
  | totalVolume' < 1 = error "Total volume cannot be less than 1"
  | totalVolume' < numPos = error "Total volume cannot be less than number of transactions"
  | numPos == 1 = return [totalVolume']
  | otherwise = generateListAux [] numPos totalVolume' totalVolume'
  where
    
    generateListAux :: [Int] -> Int -> Int -> Int -> IO [Int]
    generateListAux accList 0 _ _ = return accList
    generateListAux accList nPos totVol totalVlm = do
      randomizerH <- randomRIO (1, 10) :: IO Int
      let customHigh = if randomizerH == 1 then totVol else totVol `div` nPos
      randNum <- randomRIO (1, customHigh)
      let newTotVol = totVol - randNum
      let newAccList = updateAccList accList nPos totVol totalVlm newTotVol randNum
      generateListAux newAccList (nPos - 1) newTotVol totalVlm
   
    updateAccList :: [Int] -> Int -> Int -> Int -> Int -> Int -> [Int]
    updateAccList accList nPos _ totalVlm newTotVol randNum
      | newTotVol == 0 || nPos == 1 = (totalVlm - sum accList) : accList
      | otherwise = randNum : accList
-}