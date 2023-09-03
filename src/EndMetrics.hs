{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module EndMetrics where


import DataTypes
import Filepaths

import           Data.Aeson
import qualified Data.ByteString.Lazy as B
import Lib (readBook)
import Text.Printf
import RunSettings (wipingStartingValue)
import qualified Control.Monad
import Control.Monad (forM_)
import Data.Sequence (Seq)
import qualified Data.ByteString.Lazy as BSL
import Data.Aeson.Encode.Pretty
import Data.List.Split (chunksOf)

printMetrics :: IO ()
printMetrics = undefined

{-
-- Module for testing your thesis
printMetrics :: IO ()
printMetrics = do
    positData
    bookData

adjustTOchunk :: Bool
adjustTOchunk = True

chunkSize :: Int
chunkSize = 5

adjustData :: Int -> [FileWritePosition] -> [FileWriteBook] -> ([FileWritePosition], [FileWriteBook])
adjustData chunkSize' positionsData bookDetails =
  let chunksPositions = chunksOf chunkSize' positionsData
      combinedPositions = map combinePositionChunks chunksPositions
      chunksBooks = chunksOf chunkSize' bookDetails
      combinedBooks = map combineBookChunks chunksBooks
  in (combinedPositions, combinedBooks)

  where
    combinePositionChunks :: [FileWritePosition] -> FileWritePosition
    combinePositionChunks = foldl1 combinePositions 

    combinePositions :: FileWritePosition -> FileWritePosition -> FileWritePosition
    combinePositions p1 p2 = FileWritePosition
      { identifierPosition = identifierPosition p2
      , totalXPosAmount = totalXPosAmount p1 + totalXPosAmount p2
      , totalYPosAmount = totalYPosAmount p1 + totalYPosAmount p2
      , totalZPosAmount = totalZPosAmount p1 + totalZPosAmount p2
      , totalFPosAmount = totalFPosAmount p1 + totalFPosAmount p2
      , totalXPosCount = totalXPosCount p1 + totalXPosCount p2
      , totalYPosCount = totalYPosCount p1 + totalYPosCount p2
      , totalZPosCount = totalZPosCount p1 + totalZPosCount p2
      , totalFPosCount = totalFPosCount p1 + totalFPosCount p2
      , takerXPos = takerXPos p1 + takerXPos p2
      , takerYPos = takerYPos p1 + takerYPos p2
      , takerZPos = takerZPos p1 + takerZPos p2
      , takerFPos = takerFPos p1 + takerFPos p2
      , makerXPos = makerXPos p1 + makerXPos p2
      , makerYPos = makerYPos p1 + makerYPos p2
      , makerZPos = makerZPos p1 + makerZPos p2
      , makerFPos = makerFPos p1 + makerFPos p2
      , buyVolumePos = buyVolumePos p1 + buyVolumePos p2
      , sellVolumePos = sellVolumePos p1 + sellVolumePos p2
      , overalVolumePos = overalVolumePos p1 + overalVolumePos p2
      , overalOpenInterestPos = overalOpenInterestPos p1 + overalOpenInterestPos p2
      , activatedExitPos = activatedExitPos p2
      , isVolForcedPos = isVolForcedPos p2
 --     , leverageAmtTPos = leverageAmtTPos p1 + leverageAmtTPos p2
 --     , leverageAmtMPos = leverageAmtMPos p1 + leverageAmtMPos p2
      }

    combineBookChunks :: [FileWriteBook] -> FileWriteBook
    combineBookChunks  = foldl1 combineBooks 

    combineBooks :: FileWriteBook -> FileWriteBook -> FileWriteBook
    combineBooks b1 b2 = FileWriteBook
      { identifierBook = identifierBook b2
      , priceBook = priceBook b2
      , vSideBook = vSideBook b2
      , volumeAmountBook = volumeAmountBook b1 + volumeAmountBook b2
      , maxMinLmtBook = maxMinLmtBook b2
      , spreadBook = spreadBook b2
      , bidAskRatioBook = bidAskRatioBook b2
      , bidsTotalBook = bidsTotalBook b2
      , asksTotalBook = asksTotalBook b2
      }


bookData :: IO ()
bookData = do
    ob <- readBookInfo orderBookDetailsP
    positions <- readPositions positionInfoP 
    case (ob, positions) of
        (Right bookDetails, Right positionsData) -> do
            let (adjustedPositionsData, adjustedBookDetails) = 
                    if adjustTOchunk
                    then adjustData chunkSize positionsData bookDetails
                    else (positionsData, bookDetails)
           -- putStrLn $ "\nadjusted positions:\n " 
           -- print adjustedPositionsData
           -- putStrLn $ "\nadjusted books:\n "
          --  print adjustedBookDetails
          --  print adjustedPositionsData
          
            putStrLn $ "Total books: " ++ show (length adjustedBookDetails)
            let ((up, down), _) = upDwMovement (fromIntegral wipingStartingValue) bookDetails
            let ((_, _), moveList) = upDwMovement (fromIntegral wipingStartingValue) adjustedBookDetails
            let (downMoveClosement, upMoveClosement) = upDwCloseAmount moveList adjustedPositionsData

            putStrLn $ "Up movement: " ++ printf "%.5f" up ++ "%" ++ ", Down movement: " ++ printf "%.5f" down ++ "%"
           
            
            putStrLn $ "Down movement closement: " ++ show downMoveClosement
            putStrLn $ "Up movement closement: " ++ show upMoveClosement
            let check = downMoveClosement > upMoveClosement
            putStrLn $ "\ncheck " ++ show check
            let closementToMove = (((fromIntegral downMoveClosement)/(abs (down-up))),(fromIntegral upMoveClosement)/(abs(up-down)))
            putStrLn $ "Closement to move\n: " ++ show closementToMove
            let check2 = fst closementToMove > snd closementToMove
            putStrLn $ "\ncheck2 " ++ show check2
        _ -> error  "Failed to parse data"


readBookInfo :: FilePath -> IO (Either String [FileWriteBook])
readBookInfo filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

positData :: IO ()
positData = do
    pos <- readPositions positionInfoP 
    case pos of
        Left err -> error $ "Failed to parse metrics, POS: " ++ err
        Right positions -> finalPositioning positions

readPositions :: FilePath -> IO (Either String [FileWritePosition])
readPositions filePath = do
  jsonData <- B.readFile filePath
  return $ eitherDecode jsonData

sumPositions :: [FileWritePosition] -> (Int, Int, Int, Int)
sumPositions = foldr (\x (sumX, sumY, sumZ, sumF) ->
   (sumX + totalXPosAmount x,
    sumY +  totalYPosAmount x,
    sumZ +  totalZPosAmount x,
    sumF +  totalFPosAmount x)) (0, 0, 0, 0)

upDwMovement :: Double -> [FileWriteBook] -> ((Double, Double), [Int])
upDwMovement _ [] = ((0,0), [])
upDwMovement initPrice' xs =
    let prices = initPrice' : map priceBook xs
        res = foldl (\((up,down), bs) (y, next) -> if y < next
                                                   then ((up + next - y, down), bs ++ [1])
                                                   else ((up, down + y - next), bs ++ [0]))
                    ((0,0), []) (zip prices (tail prices))
    in res

upDwCloseAmount :: [Int] -> [FileWritePosition] -> (Int, Int)
upDwCloseAmount movements positions =
    let zipped = zip movements positions
        downMoveClosement = sum [totalZPosAmount pos + totalFPosAmount pos | (move, pos) <- zipped, move == 0]
        upMoveClosement = sum [totalZPosAmount pos + totalFPosAmount pos | (move, pos) <- zipped, move == 1]
    in (downMoveClosement, upMoveClosement)


--undeltaPriceMovement ::  [FileWriteBook] -> IO ()
--undeltaPriceMovement [] = putStrLn "No books found"



finalPositioning :: [FileWritePosition] -> IO ()
finalPositioning [] = putStrLn "No positions found"
finalPositioning xs =
    let (finalX, finalY, finalZ, finalF) = sumPositions xs
    in putStrLn $   "Final X: "   ++ show finalX ++
                    ", Final Y: " ++ show finalY ++
                    ", Final Z: " ++ show finalZ ++
                    ", Final F: " ++ show finalF ++
                    "\ncheck: " ++ show (finalF > finalZ)

                    -}