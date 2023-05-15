{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use withFile" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}
{-# HLINT ignore "Use when" #-}

-- | module name
module Orderbook.App.Functions.AAGenerator where
-- | external modules
import Control.Exception (bracket, finally)
import Data.Char (toUpper)
import Data.List (unfoldr)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.IO
  ( IOMode(ReadMode, WriteMode)
  , hClose
  , hFileSize
  , hPutStr
  , openFile
  , withFile
  )
import System.Random
  ( Random(randomR, randomRs)
  , RandomGen(split)
  , StdGen
  , mkStdGen
  , newStdGen
  , randomRIO
  , setStdGen
  )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)
-- | exporting modules
import Orderbook.App.Settings.Arunsettings 
import Orderbook.App.Data.Afilepaths ( pricePath )

-- //starting point (starting price)
-- Read the contents of the file and return the last number
getLastNumberFromFile :: FilePath -> IO Double
getLastNumberFromFile filePath = do
  content <- readFile filePath
  let numbers = map read (words content) :: [Double]
  return $ last numbers

-- Define the starting point, maximum and minimum upmove and downmove values, and the maximum number of decimal points
startingPointFromFile :: IO Double
startingPointFromFile = getLastNumberFromFile pricePath

-- list management 1
-- //price list ask
-- Custom random number generator with the desired probability distribution, probability distribution for orderbook
customRandomR :: (RandomGen g) => (Int, Int) -> g -> (Int, g)
customRandomR (low, high) gen = (num, gen3)
  where
    (p, gen1) = randomR (0, 1 :: Double) gen
    (gen2, gen3) = split gen1
    num
      | p <= 0.10 = low + round (fromIntegral (high - low) * 0.05 * r1)
      | p <= 0.30 = low + round (fromIntegral (high - low) * 0.15 * r1)
      | p <= 0.50 = low + round (fromIntegral (high - low) * 0.25 * r1)
      | p <= 0.70 = low + round (fromIntegral (high - low) * 0.35 * r1)
      | p <= 0.80 = low + round (fromIntegral (high - low) * 0.45 * r1)
      | p <= 0.85 = low + round (fromIntegral (high - low) * 0.55 * r1)
      | p <= 0.90 = low + round (fromIntegral (high - low) * 0.65 * r1)
      | p <= 0.95 = low + round (fromIntegral (high - low) * 0.75 * r1)
      | p <= 0.98 = low + round (fromIntegral (high - low) * 0.85 * r1)
      | otherwise = low + round (fromIntegral (high - low) * r1)
    (r1, _) = randomR (0, 1 :: Double) gen2

customRandomRs :: (Int, Int) -> StdGen -> [Int]
customRandomRs bounds gen = nums
  where
    nums = unfoldr (Just . customRandomR bounds) gen

-- walls
randomListwalls :: IO [Int]
randomListwalls = do
  randomRs (wallminimum', wallmaximum') <$> newStdGen

takeCustomRandom :: StdGen -> Int -> [Int]
takeCustomRandom gen n = take n (customRandomRs (minimum', maximum') gen) -- 100 000 

printCustomRandomList :: Int -> IO [Int]
printCustomRandomList n = do
  gen <- newStdGen
  let randomList = takeCustomRandom gen n
  return randomList

-- // price list implemented only for bids
newStdGen' :: IO StdGen
newStdGen' = do
  currentGen <- newStdGen
  let (newGen, _) = split currentGen
  setStdGen newGen
  return newGen

printRandomList' :: Int -> IO [Int]
printRandomList' n = do
  gen <- newStdGen'
  let randomList = takeCustomRandom gen n
  return randomList

wallminimum' :: Int
wallminimum' = maximum' -- (the maximum' is recommended as a minumum)

wallmaximum' :: Int
wallmaximum' = maximum' * wallAmplifier

takeamount :: Int
takeamount = takeamountBID + takeamountASK --works mainly for gauging how big the walls should be

taketowalls :: Int -> Int
taketowalls 0 = 0
taketowalls n =
  if n < orderwalllikelyhood
    then 0
    else n `div` orderwalllikelyhood

-- Helper function to round a number to a specific number of decimal points
roundTo :: Int -> Double -> Double
roundTo n x = (fromInteger . round $ x * (10 ^ n)) / (10.0 ^^ n)

-- //end of decimal rounding
-- // makes sure numbers are different with each run
--  Initialize a random generator with a seed based on the current time
randomGen :: IO StdGen
randomGen = do
  currentTime <- getPOSIXTime
  let seed = round currentTime
  return $ mkStdGen seed

-- // generating next numbers
-- Function to generate the next number in the upMove list
nextNumber :: [Double] -> Double -> (Double, StdGen) -> (Double, StdGen)
nextNumber moves prev (rnd, gen) = (newNum, newGen)
  where
    listSize = length moves
    (index, newGen) = randomR (0, listSize - 1) gen
    delta = moves !! index
    newNum = roundTo maxDecimal (prev + delta)

-- Function to generate the next number in the downMove list
nextNumberDown :: [Double] -> Double -> (Double, StdGen) -> (Double, StdGen)
nextNumberDown moves prev (rnd, gen) = (newNum, newGen)
  where
    listSize = length moves
    (index, newGen) = randomR (0, listSize - 1) gen
    delta = moves !! index
    newNum = roundTo maxDecimal (prev - delta)

-- // end of generating next numbers
-- Generating ask list
infiniteList :: Double -> StdGen -> [Double] -> [Double]
infiniteList start gen moves =
  map fst $ iterate (\x -> nextNumber moves (fst x) x) (start, gen)

-- Generating bid list
infiniteListDown :: Double -> StdGen -> [Double] -> [Double]
infiniteListDown start gen moves =
  takeWhile (> 0) . map fst $
  iterate (\x -> nextNumberDown moves (fst x) x) (start, gen)

-- // list management for the orderbook 2
-- adding insertion grid togerger with prices
zipToTuples :: [Double] -> [Int] -> [(Double, Int)]
zipToTuples = zip

addAt :: Int -> Int -> [Int] -> [Int]
addAt idx val lst =
  let (pre, post) = splitAt idx lst
   in case post of
        [] -> pre ++ [val] -- When the list is empty, append the value.
        (x:rest) -> pre ++ (val + x) : rest

insertRandomly :: [Int] -> [Int] -> IO [Int]
insertRandomly [] target = return target
insertRandomly (x:xs) target = do
  pos <- randomRIO (0, length target)
  insertRandomly xs (addAt pos x target)

randomlyInsert :: [Int] -> [Int] -> IO [Int]
randomlyInsert = insertRandomly

-- this works for bids (we need to spit the list into two halfes, the first one is going to bids)
firstPartList :: [Int] -> [Int]
firstPartList lst = take halfLen lst
  where
    halfLen = (length lst + 1) `div` 2

-- this works for asks, it's the second half of the list
secondPartList :: [Int] -> [Int]
secondPartList lst = drop halfLen lst
  where
    halfLen = (length lst + 1) `div` 2

-- // function to help print (helping functions in main to function correctly)
allCaps :: String -> String
allCaps = map toUpper

minimumlimit :: (Ord a, Foldable t, Functor t) => t [a] -> a
minimumlimit = minimum . fmap minimum

maximumlimit :: (Ord a, Foldable t, Functor t) => t [a] -> a
maximumlimit = maximum . fmap maximum

-- // for price changes:
orderbookChange :: [(Double, Int)] -> Int -> [(Double, Int)]
orderbookChange [] _ = []
orderbookChange ((price, volume):xs) amount
  | amount <= 0 = (price, volume) : xs
  | amount >= volume = orderbookChange xs (amount - volume)
  | otherwise = (price, volume - amount) : xs

-- HERE IS THE BUG FOR SURE
lengthchange :: [(Double, Int)] -> [(Double, Int)] -> Int
lengthchange xs ys = length xs - length ys

-- | Generating ask list (the updated one)
infiniteList' :: Double -> StdGen -> [Double] -> [Double]
infiniteList' startPoint gen moves =
  map fst $ iterate (\x -> nextNumber moves (fst x) x) (startPoint, gen)

-- | Generating bid list (the updated one)
infiniteListDown' :: Double -> StdGen -> [Double] -> [Double]
infiniteListDown' startPoint gen moves =
  map fst $ iterate (\x -> nextNumberDown moves (fst x) x) (startPoint, gen)

-- //
isFileEmpty :: FilePath -> IO Bool
isFileEmpty filePath =
  bracket
    (openFile filePath ReadMode)
    hClose
    (\handle -> do
       fileSize <- hFileSize handle
       return (fileSize == 0))

readBook :: FilePath -> IO [(Double, Int)]
readBook fileName =
  bracket
    (openFile fileName ReadMode)
    hClose
    (\handle -> do
       contents <- BS.hGetContents handle
       let contentsStr = BS.unpack contents
       return $
         case readMaybe contentsStr of
           Nothing -> []
           Just bidBook -> bidBook)

newRunSettings :: FilePath -> FilePath -> FilePath -> FilePath -> Int -> IO ()
newRunSettings logFile bidFile askFile priceFile newValue = do
  let wipe = ""
  let price = show newValue
  writeFile logFile wipe
  writeFile bidFile wipe
  writeFile askFile wipe
  writeFile priceFile price

-- cleaning price history file
removeEmptyLines :: FilePath -> IO ()
removeEmptyLines filePath = do
  content <- TIO.readFile filePath
  let linesOfFile = T.lines content
      cleanedLines = filter (not . T.null . trim) linesOfFile
      newContent = T.unlines cleanedLines
  TIO.writeFile filePath newContent

trim :: T.Text -> T.Text
trim = T.strip

sumInts :: [(Double, Int)] -> Int
sumInts lst = sum (map snd lst)

spread' :: Double -> Double -> Double
spread' askHead bidHead = abs (askHead - bidHead)

sideProbability :: Double -> IO Bool
sideProbability trueProbability
  | trueProbability < 0 || trueProbability > 1 =
    error "Probability must be between 0 and 1"
  | otherwise = do
    randomValue <- randomRIO (0, 1)
    return (randomValue < trueProbability)
