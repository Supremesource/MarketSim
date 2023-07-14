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
--# modules converts random functions into exact ones (for testing purpouses)
  -}
where

-- | External libraries
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
import Data.Sequence        (Seq, ViewL ((:<)), (<|), (><))
import qualified Data.Bifunctor
-- | Internal libraries
import Lib
import RunSettings
import Util
import DataTypes
import PosCycle
import Statistics
import Generator
import Filepaths



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
positionFutureNonRandom :: Double -> (TakerPositions, MakerPositions) -> IO FutureInfo
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

nonRandomNormalGenerator ::
     [Int] -> [Int] -> SeqFutureInfo -> String -> IO (TakerPositions, MakerPositions)
nonRandomNormalGenerator takerLst makerLst (toTakeFromLong, toTakeFromShort) liqSide = do
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
      let takerSide' = "y"
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
      let takerSide' = "y"   
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
     SeqFuture
  -> SeqFuture
  -> Double
  -> IO ( 
      Seq (Int, String, String)     -- info about the liquidation
        , (SeqFuture, SeqFuture)    -- updated futures
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

nonRandomNormalrunProgram ::
     ([Int], [Int])
  -> SeqFutureInfo
  -> Double
  -> String
  -> IO ( SeqFutureInfo    -- # updated accumulator short # updated accumulator long
        , NewPositioning ) -- # updated accumulator positions
         
nonRandomNormalrunProgram (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFuture) -- TODO TAKE OUT oldPositions
                                               sPrice liqSide = do
  newPositioning <-
    nonRandomNormalGenerator
            volumeSplitT
            volumeSplitM
            (oldLongFuture, oldShortFuture)
            liqSide

  posFut <- positionFutureNonRandom sPrice newPositioning -- ~
  let adjustedLiquidation = if liqSide == "" then "no" else "yes" -- ~
  
  let converToTransaction = TransactionFut {future = posFut}
  let (filteredLongFuture, filteredShortFuture) =
        ( filterFuture {-adjustedLiquidation-}  "f" converToTransaction
        , filterFuture {-adjustedLiquidation-}  "z" converToTransaction)

  let orderedTupleNew   = tuplesToSides newPositioning
  let unorderedPosNew   = newPositioning
  let (longTuple, shortTuple) =
        Data.Bifunctor.bimap (filterTuple "z") (filterTuple "f") orderedTupleNew
  let (filteredShortTuple, filteredLongTuple) = (longTuple, shortTuple)
 
  let (newShortsAcc, newLongsAcc) 
       =
        ( filterFutureAmount filteredLongTuple oldLongFuture
        , filterFutureAmount filteredShortTuple oldShortFuture)
 
 
  let futureToSeq =
        let ((firstOld, secondOld), (firstNew, secondNew), newPos) =
              ( (newLongsAcc, newShortsAcc)
              , (filteredLongFuture, filteredShortFuture)
              , newPositioning)
         in ( (firstOld, secondOld)
            , (futureInfoToSeq firstNew, futureInfoToSeq secondNew)
            , newPos)
  
  -- | FutureInfo
  let updatedFutureAcc =
        (\((frst, fst'), (snd', scnd), _) -> (frst <> scnd, fst' <> snd'))
          futureToSeq
  
  
  
  return
    ( updatedFutureAcc  -- # already concat to new accumulator
    , unorderedPosNew   -- # returning in a raw form to positionCycle
     )