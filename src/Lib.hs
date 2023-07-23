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
module Lib
{-
-- ! DESCRIPTION 
aggregation of general functions
-}

where

import           Control.Exception.Base
-- | moudle aggregating all the functions

-- | external modules
import           Control.Monad             (replicateM, when)
import           Data.Aeson                (eitherDecode, eitherDecode')
import           Data.Aeson.Encode.Pretty  (encodePretty)
import qualified Data.ByteString.Lazy      as BL
import           Data.Char                 (toUpper)
import           Data.Foldable             (Foldable (toList))
import           Data.Ratio                ((%))
import qualified Data.Text                 as T
import qualified Data.Text.IO              as TIO
import           Data.Time.Clock.POSIX     (getPOSIXTime)
import           System.IO                 (IOMode (ReadMode), hClose,
                                            hFileSize, openFile)
import           System.Random             (Random (randomR, randomRs),
                                            RandomGen (split), StdGen, mkStdGen,
                                            newStdGen, randomRIO, setStdGen)
import           Text.Printf               (printf)
import           Data.Sequence             (Seq, (<|), viewl, ViewL(..),fromList,foldlWithIndex)
import             Debug.Trace

-- | internal libraries
import           Colours
import           DataTypes
import           RunSettings
import           Statistics
import System.Process



-- ? WALLS
randomListwalls :: IO [Int]
randomListwalls = do
  randomRs (wallminimum', wallmaximum') <$> newStdGen

-- > RANDOMESS <
takeCustomRandom :: StdGen -> Int -> [Int]
takeCustomRandom gen n = take n (customRandomRs (minimum', maximum') gen) -- 100 000

printCustomRandomList :: Int -> IO [Int]
printCustomRandomList n = do
  gen <- newStdGen
  let randomList = takeCustomRandom gen n
  return randomList

-- another version with different random generator
printRandomList' :: Int -> IO [Int]
printRandomList' n = do
  gen <- newStdGen'
  let randomList = takeCustomRandom gen n
  return randomList

-- > RANDOMNESS <
-- | price list implemented only for bids
newStdGen' :: IO StdGen
newStdGen' = do
  currentGen <- newStdGen
  let (newGen, _) = split currentGen
  setStdGen newGen
  return newGen


-- | (the maximum' is recommended as a minumum)
wallminimum' :: Int
wallminimum' = maximum'

-- | maximum wallamount
wallmaximum' :: Int
wallmaximum' = maximum' * wallAmplifier

-- | works mainly for gauging how big the walls should be
takeamount :: Int
takeamount = takeamountBID + takeamountASK

-- | telling the wall occourance
taketowalls :: Int -> Int
taketowalls 0 = 0
taketowalls n =
  if n < orderwalllikelyhood
    then 0
    else n `div` orderwalllikelyhood

randomGen :: IO StdGen
randomGen = do
  currentTime <- getPOSIXTime
  let seed = round currentTime
  return $ mkStdGen seed


-- | generating next numbers
-- Function to generate the next step/number in the upMove list
nextNumberUp :: [Double] -> Double -> StdGen -> (Double, StdGen)
nextNumberUp moves prev gen = (newNum, newGen)
  where
    listSize        = length moves
    (index, newGen) = randomR (0, listSize - 1) gen
    delta           = moves !! index
    newNum          = roundTo maxDecimal (prev + delta)


-- | Function to generate the next number in the downMove list
nextNumberDown :: [Double] -> Double -> StdGen -> (Double, StdGen)
nextNumberDown moves prev gen = (newNum', newGen)
  where
    listSize        = length moves
    (index, newGen) = randomR (0, listSize - 1) gen
    delta           = moves !! index
    newNum          = roundTo maxDecimal (prev - delta)
    newNum' =
      if newNum < 0
        then 0.001
        else newNum


-- | end of generating next numbers
-- | Generating ask list
infiniteListUpConstant :: Double -> StdGen -> [Double] -> [Double]
infiniteListUpConstant start gen moves =
  map fst $ iterate (uncurry (nextNumberUp moves)) (start, gen)


-- | Generating bid list
infiniteListDownConstant :: Double -> StdGen -> [Double] -> [Double]
infiniteListDownConstant start gen moves =
  takeWhile (> 0) . map fst $
  iterate (uncurry (nextNumberDown moves)) (start, gen)

-- | list management for the orderbook 2
-- | adding insertion grid togerger with prices
zipToTuples :: [Double] -> [Int] -> SeqOrderBook
zipToTuples xs ys = fromList $ zip xs ys


-- sums the order wall into the normal orderbook
sumAt :: Int -> Int -> [Int] -> [Int]
sumAt idx val lst =
  let (pre, post) = splitAt idx lst
   in case post of
        []       -> pre ++ [val]
        (x:rest) -> pre ++ (val + x) : rest

-- | When the list is empty, append the value.

-- |randomly inserting walls
-- randomly choosing where that sum should be inserted

-- > RANDOMNESS <
randomlyInsert :: [Int] -> [Int] -> IO [Int]
randomlyInsert [] target = return target
randomlyInsert (x:xs) target = do
  pos <- randomRIO (0, length target)
  randomlyInsert xs (sumAt pos x target)


-- | this works for bids (we need to spit the list into two halfes, the first one is going to bids)
firstPartList :: [Int] -> [Int]
firstPartList lst = take halfLen lst
  where
    halfLen = (length lst + 1) `div` 2


-- | this works for asks, it's the second half of the list
secondPartList :: [Int] -> [Int]
secondPartList lst = drop halfLen lst
  where
    halfLen = (length lst + 1) `div` 2

minList :: (Ord a, Foldable t, Functor t) => t [a] -> Maybe a
minList xs =
  if all null (toList xs)
    then Nothing
    else Just (minimum . fmap minimum $ xs)

maxList :: (Ord a, Foldable t, Functor t) => t [a] -> Maybe a
maxList xs =
  if all null (toList xs)
    then Nothing
    else Just (maximum . fmap maximum $ xs)


-- | for price changes:
orderbookChange :: Seq (Double, Int) -> Int -> Seq (Double, Int)
orderbookChange xs amount = case viewl xs of
  EmptyL -> xs
  (price, volume) :< rest
    | amount <= 0      -> xs
    | amount >= volume -> orderbookChange rest (amount - volume)
    | otherwise        -> (price, volume - amount) <| rest

-- | helper for inserting into bid and ask orderbook
bookNumChange :: Seq (Double, Int) -> Seq (Double, Int) -> Int
bookNumChange xs ys = abs (length xs - length ys)


-- | Generating ask list
infiniteListUpChange :: Double -> StdGen -> [Double] -> [Double]
infiniteListUpChange startPoint gen moves =
  map fst $ iterate (uncurry (nextNumberUp moves)) (startPoint, gen)


-- | Generating bid list (the updated one)
infiniteListDownChange :: Double -> StdGen -> [Double] -> [Double]
infiniteListDownChange startPoint gen moves =
  takeWhile (> 0) . map fst $
  iterate (uncurry (nextNumberDown moves)) (startPoint, gen)


-- ? SOME POSITION INFORMATION
sumInts :: SeqOrderBook -> Int
sumInts = foldlWithIndex (\acc _ (_, y) -> acc + y) 0

spread' :: Double -> Double -> Double
spread' askHead bidHead = roundedResult
  where
    result        = abs (askHead - bidHead)
    roundedResult = read $ printf "%.5f" result

-- > RANDOMNESS <
sideProbability :: Double -> IO Bool
sideProbability trueProbability
  | trueProbability < 0 || trueProbability > 1 =
    error $ red "Probability must be between 0 and 1"
  | otherwise = do
    randomValue <- randomRIO (0, 1)
    return (randomValue < trueProbability)

countElements :: String -> [(Int,String)] -> Int
countElements x = length . filter ((== x) . snd)


elementSize :: String -> [(Int,String)] -> Int
elementSize x = sum . map fst . filter ((== x) . snd)


-- | putting voume side and amount into a tuple
--volumecounter :: (Int,String) -> (Int,String)
--volumecounter (n,a) = if a == "x" || a == "z" then (n,"buy") else (n,"sell")

-- ? OPEN INTEREST FUNCTIONS
interestorMinus :: TakerPositions -> MakerPositions -> Int
interestorMinus [] _ = 0
interestorMinus _ [] = 0
interestorMinus ((n1, s1):takers) ((n2, s2):makers)
  | n1 == n2 =
    if s1 == "f" && s2 == "z" || s1 == "z" && s2 == "f"
      then n1 + interestorMinus takers makers
      else interestorMinus takers makers
  | n1 < n2 =
    if s1 == "f" && s2 == "z" || s1 == "z" && s2 == "f"
      then n1 + interestorMinus takers ((n2 - n1, s2) : makers)
      else interestorMinus takers ((n2 - n1, s2) : makers)
  | otherwise =
    if s1 == "f" && s2 == "z" || s1 == "z" && s2 == "f"
      then n2 + interestorMinus ((n1 - n2, s1) : takers) makers
      else interestorMinus ((n1 - n2, s1) : takers) makers

interestorPlus :: TakerPositions -> MakerPositions -> Int
interestorPlus [] _ = 0
interestorPlus _ [] = 0
interestorPlus ((n1, s1):takers) ((n2, s2):makers)
  | n1 == n2 =
    if s1 == "x" && s2 == "y" || s1 == "y" && s2 == "x"
      then n1 + interestorPlus takers makers
      else interestorPlus takers makers
  | n1 < n2 =
    if s1 == "x" && s2 == "y" || s1 == "y" && s2 == "x"
      then n1 + interestorPlus takers ((n2 - n1, s2) : makers)
      else interestorPlus takers ((n2 - n1, s2) : makers)
  | otherwise =
    if s1 == "x" && s2 == "y" || s1 == "y" && s2 == "x"
      then n2 + interestorPlus ((n1 - n2, s1) : takers) makers
      else interestorPlus ((n1 - n2, s1) : takers) makers


-- ? CHECKERS
-- | checking volume
volumechecker ::
     Int -> Int -> Int -> Int -> Int -> IO ()
volumechecker minimumV a b c d 
  | a < minimumV   ||
      b < minimumV ||
      c < minimumV ||
      d < minimumV =
     
    error
      (red
         "\n\nVolume must be greater than minimum volume specified in settings")
  | otherwise = return ()


-- | checking position amount
positionamountcheck :: Int -> Int -> IO ()
positionamountcheck a b
  | a < (b * 2) =
    error
      (red
         "\n\nPosition amount must be greater than 2 times the minimum volume specified in settings\n\n(you can fix this in the settings, 'catching potential errors)")
  | otherwise = return ()


-- | warning checker for settings
-- | helper function for probability settings (/% checker)
addsupto100 :: Int -> Int -> IO ()
addsupto100 first second
  | first + second == 100 = return ()
  | otherwise = do
    putStr "\n/RunSettings.hs "
    putStrLn $ purple "[WARNING]\n"
    putStr ("\n         |\naprx.144 |" ++ blue "  probabilites in settings do not add up to 100%"  
         ++ "\n         |" ++ blue "  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n\n")


-- ? TEMPLEATE runProgram FUNCITOINS

-- | helper function for runProgramPercenatge
toIntegralLenght :: [Options] -> Double
toIntegralLenght x = fromIntegral (length x)

runProgramPercentage :: Int -> Double -> [Int]
runProgramPercentage n listEnd =
  [round (fromIntegral n * x) | x <- [1 / listEnd,2 / listEnd .. 1]]

processlist :: [Options] -> [Int] -> Int -> Options
processlist (o:os) (p:ps) x
  | x <= p    = o
  | otherwise = processlist os ps x
processlist _ _ _ = error $ red "Invalid input to processlist"

randomhandler :: Options -> Options -> Options
randomhandler initoption randomGenlocal
  | initoption == RANDOM = randomGenlocal
  | otherwise            = initoption

optionProcessor :: Options -> Int -> Int
optionProcessor a i =
  case a of
    UP  -> templeaterunProgramBUY i a
    UPP -> templeaterunProgramBUY i a
    _   -> templeaterunProgramSELL i a



templeaterunProgramBUY :: Int -> Options -> Int
templeaterunProgramBUY a op =
  case op of
     -- increasing b vol by 200% (Amplifier)
    UP  -> round $ fromIntegral a * (1.15 :: Double)
    -- increasing b vol by 400% (Amplifier)
    UPP -> round $ fromIntegral a * (1.25 :: Double)
    -- doing nothing to downtrend (Amplifier)
    DW  -> a
    -- extreme downtrend doing nothing (Amplifier)
    DWW -> a
    CN  -> a
    _   -> error $ red "your templeate pattern matching did not go through"

templeaterunProgramSELL :: Int -> Options -> Int
templeaterunProgramSELL a op =
  case op of
    -- nothing (Amplifier)
    UP  -> a
    -- nothing (Amplifier)
    UPP -> a
     -- increasing s vol by 200% (Amplifier)
    DW  ->  round $ fromIntegral a * (1.15 :: Double)
    -- increasing s vol by 400% (Amplifier)
    DWW -> round $ fromIntegral a * (1.25 :: Double)
    CN  -> a
    _   -> error $ red "your templeate pattern matching did not go through"


-- | i == position index
processTempleaterunProgram :: Int -> Options -> (Int,Int)
processTempleaterunProgram i o = do
  let process =
        runProgramPercentage numPositions (toIntegralLenght runProgramlist)
  let currentO' = processlist runProgramlist process i
  let currentO =
        if currentO' == RANDOM
          then o
          else currentO'
  let ifprocess = currentO == UP || currentO == UPP
  let templeatedprobability =
        if ifprocess
          then optionProcessor currentO buyTakerProb
          else buyTakerProb
  let templeatedprobabilityY =
        if ifprocess
          then sellTakerProb
          else optionProcessor currentO sellTakerProb
  (templeatedprobability, templeatedprobabilityY)


-- | local variables

-- > RANDOMNESS <
randomOptionGen :: IO Options
randomOptionGen = do
  let options = [UP, UPP, CN, DWW, DW]
  idx <- randomRIO (0, length options - 1)
  return (options !! idx)


-- ? IO related functions
-- |` IO help `
-- | function to help print (helping functions in main to function correctly)
allCaps :: String -> String
allCaps = map toUpper

trim :: T.Text -> T.Text
trim = T.strip

roundToTwoDecimals :: (RealFrac a, Fractional b) => a -> b
roundToTwoDecimals x = fromRational (round (x * 100) Data.Ratio.% 100)


-- | checking if the file is empty
isFileEmpty :: FilePath -> IO Bool
isFileEmpty filePath =
  bracket
    (openFile filePath ReadMode)
    hClose
    (\handleEmpty -> do
       fileSize <- hFileSize handleEmpty
       return (fileSize == 0))


-- | reading the orderbook
readBook :: FilePath -> IO [(Double, Int)]
readBook fileName =
  bracket
    (openFile fileName ReadMode)
    hClose

    (\handle' -> do
       contents <- BL.hGetContents handle'
       case eitherDecode contents of
         Left err -> do
           putStrLn $ "Error parsing JSON: " ++ err
           return []
         Right bookData -> return $ book bookData)

-- | when wiping runProgram then -> wipe the orderbook + write starting price
newrunProgramSettings ::
     FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> FilePath
  -> Int
  -> IO ()
newrunProgramSettings askBookF bidBookF logF bookDetailF positionInfoF initPriceF posFutureF newValue = do
  let wipe = ""
  let price = InitPrice (fromIntegral newValue)
  writeFile askBookF      wipe
  writeFile bidBookF      wipe
  writeFile logF          wipe
  writeFile bookDetailF   wipe
  writeFile positionInfoF wipe
  writeFile posFutureF    wipe
  BL.writeFile initPriceF (encodePretty price)

-- | cleaning price history file
removeEmptyLines :: FilePath -> IO ()
removeEmptyLines filePath = do
  content <- TIO.readFile filePath
  let linesOfFile = T.lines content
      cleanedLines = filter (not . T.null . trim) linesOfFile
      newContent = T.unlines cleanedLines
  TIO.writeFile filePath newContent


-- | Helper function to round a number to a specific number of decimal points
roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger . round $ x * (10 ^ n)) / (10.0 ^^ n)

-- | end of decimal rounding
-- | makes sure numbers are different with each runProgram
-- | Initialize a random generator with a seed based on the current time

-- | Define the starting point, maximum and minimum upmove and downmove values, and the maximum number of decimal points
startingPointFromFile :: FilePath -> IO Double
startingPointFromFile filePath = do
  content <- BL.readFile filePath
  let result =
        either
          error
          (return . initPrice)
          (eitherDecode' content :: Either String InitPrice)
  toDouble <- result
  Control.Monad.when (toDouble < 0) $
    error $ red "Starting price cannot be negative"
  return toDouble

generateVolumes :: Int -> Int -> IO [Int]
generateVolumes numPos totalVolume' = do
  Control.Monad.when (numPos < 1) $
    error $ red "Number of transactions cannot be less than 1"
  Control.Monad.when (totalVolume' < 1) $
    error $ red "Total volume cannot be less than 1"
  Control.Monad.when (totalVolume' < numPos) $
    error $ red "Total volume cannot be less than number of transactions"
  let maxVol = totalVolume' `div` round ((fromIntegral numPos :: Double) / 1.2)

  -- > RANDOMNESS <
  volumes <- replicateM (numPos - 1) (randomRIO (1, maxVol))

  let lastVolume = totalVolume' - sum volumes
  return (volumes ++ [lastVolume])

plotGraph :: IO ()
plotGraph = do
    if plotCharts
        then callCommand "python3 /Users/janzimula/workspace/marketsim/backtestapp/strategy-back/procesGraph.py"
        else putStrLn "Plotting is turned off."