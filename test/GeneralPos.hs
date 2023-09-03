module GeneralPos where

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


leverageListNonRPG :: [(Int, String)] -> [Int] -> IO [Int]
leverageListNonRPG [] accL = return accL
leverageListNonRPG ((_,side):xs) accL = do
  -- leverageLong  <- takenLeverage baseLeverageLong
  -- leverageShort <- takenLeverage baseLeverageShort
  let takeLeverage  
                  | side == "x" = 100
                  | side == "y" = 1
                  | otherwise = 0
  if takeLeverage /= 0 then leverageListNonRPG xs  $ takeLeverage : accL else leverageListNonRPG xs accL



-- ? PUTTING ALL FUNCTIONS ABOVE TOGETHER
normalrunProgramNonRPG ::
        VolumeSide
  ->   ([Int], [Int])
  -> SeqClosePositionData
  -> Double
  -> String
  -> IO ( SeqClosePositionData          -- # updated accumulator short # updated accumulator long
        , NewPositioning                -- # updated accumulator positions
        , ([Int],[Int]) )               -- # taken leverage
normalrunProgramNonRPG volSide (volumeSplitT, volumeSplitM) (oldLongFuture, oldShortFuture) -- TODO TAKE OUT oldPositions
                                               sPrice liqSide = do
  newPositioning <-
    normalGenerator
      volSide
      volumeSplitT
      volumeSplitM
      (oldLongFuture, oldShortFuture)
      liqSide

  let (takerPositioning,makerPositioning) = newPositioning

  --putStrLn  "\nthe taker Positioning is:"
  --print takerPositioning
  --putStrLn "\nthe maker Positioning is : "
  --print makerPositioning
  
  leverageTaker <- leverageListNonRPG takerPositioning leverageAccumulator
  leverageMaker <- leverageListNonRPG makerPositioning leverageAccumulator
  
  -- let isLeverageZeroTaker = if any (\x -> x == "z" || x == "f") (snd <$> takerPositioning) then 0 else leverageTaker
  -- let isLeverageZeroMaker = if any (\x -> x == "z" || x == "f") (snd <$> makerPositioning) then 0 else leverageMaker

  posFut <- positionFuture (leverageTaker,leverageMaker) sPrice newPositioning
  --putStrLn "\nthe position future is : "
  --print posFut

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

-- filters out the future from the old future
  newLongsAcc   <- filterCloseAmount filteredLongTuple  oldLongFuture
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
    , (leverageTaker , leverageMaker)) -- # returning the leverage to positionCycle 