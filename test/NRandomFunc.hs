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
module NRandomFunc where

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