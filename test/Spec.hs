import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random
import Data.Maybe
import Control.Exception (try, ErrorCall)
import Control.Monad (unless)


import Lib
import runProgramSettings
import Util
import DataTypes

main :: IO ()
main = tests

tests :: IO ()
tests = do
  testsLib
  testsUtil

testsLib :: IO ()
testsLib = hspec $ do
  
 -- | LIBRARY TESTS:
 
  -- ORDERBOOK TESTS  
  describe "Lib.randomListwalls" $ do
    it "returns limit walls in a correct size" $ do
      walls <- randomListwalls 
      (all (\wall -> wall >= wallminimum' && wall <= wallmaximum') (take 1000 walls)) `shouldBe` True
 
  describe "Lib.customRndList" $ do
    it "returns the correct size ranom number" $ do
     customRndList <- printCustomRandomList 1000
     (all (\list -> list >= minimum' && list <= maximum') customRndList) `shouldBe` True
 
  describe "Lib.wallmximum'" $ do
    it "returns rational size limit wall maximum" $ do
      wallmaximum' < 500 * maximum'  `shouldBe`  True
 
  describe "Lib.orderwalllikelyhood" $ do
    it "returns rational amount of limit walls" $ do
      orderwalllikelyhood > 10 `shouldBe` True
  
  describe "Lib.newNumberDown" $ do
    it "returns non negative down step in the orderbook" $ do
      stGen <- newStdGen'
      let input  =  [11.5, 12.5, 1, 999] 
      let number = fst $ nextNumberDown input 11.2 stGen
      number > 0 `shouldBe` True
 
  describe "Lib.newNumberUp" $ do
    it "returns non negative up step in the orderbook" $ do
      stGen <- newStdGen'
      let input  =  [11.5, 12.5, 1, 999] 
      let number = fst $ nextNumberUp input 11.2 stGen
      number > 0 `shouldBe` True
 
  describe "Lib.infiniteListUpConstant" $ do
    it "returns non negative list from infiniteListUpConstant" $ do
      stGen <- newStdGen'
      let upMovesInsert = take takeamountASK $ randomRs (minUpMove, maxUpMove) stGen
      let list = take 1000 $ infiniteListUpConstant 1.2 stGen upMovesInsert 
      (all (>= 0) list) `shouldBe` True
 
  describe "Lib.infiniteListDownConstant" $ do
    it "returns non negative list from infiniteListDownConstant" $ do
      stGen <- newStdGen'
      let downMovesInsert = take takeamountBID $ randomRs (minDownMove, maxDownMove) stGen
      let list = take 20000 $ infiniteListDownConstant 1.2 stGen downMovesInsert
      (all (> 0) list) `shouldBe` True
 
  describe "Lib.sumAt" $ do
    it "returns sumed element at a desired position" $ do
      let list = [1,2,3,4,5]
      let element = 6
      let position = 2
      let newList = sumAt position element list
      newList `shouldBe` [1,2,9,4,5]

  describe "Lib.firstPartList,Lib.secondPartList" $ do
    it "returns list that is split in half" $ do
      let list = [1,2,3,4,5,6,7]
      let (frst,scnd) = (firstPartList list, secondPartList list)
      (frst,scnd) `shouldBe` ([1,2,3,4],[5,6,7])
  
  describe "Lib.minList,Lib.maxList" $ do
    it "returns min and max from lists" $ do
      let listOfLists = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]
      let (mn,mx) = (minList listOfLists, maxList listOfLists)
      (mn,mx) `shouldBe` (Just 1, Just 9)

  describe "Lib.orderbookChange" $ do
    it "returns orderbook with changed values" $ do
      let book = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,100000)]
      let volume = 701
      let processedBook = orderbookChange book volume
      processedBook `shouldBe` [(15,99999)]
  
  describe "Lib.bookNumChange" $ do
    it "returns the amount that the orderbook changed" $ do
      let book = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,100000)]
      let bookChange = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0)]
      let changedBook = bookNumChange book bookChange
      changedBook `shouldBe` 1

  describe "Lib.infiniteListUpChange" $ do
    it "returns non negative price grid for up list" $ do
      stGen <- newStdGen'
      let upMovesInsert = take takeamountASK $ randomRs (minUpMove, maxUpMove) stGen
      let list = take 1000 $ infiniteListUpChange 1.2 stGen upMovesInsert 
      (all (>= 0) list) `shouldBe` True
  
  describe "Lib.infiniteListDownChange" $ do
    it "returns non negative price grid for down list" $ do
      stGen <- newStdGen'
      let downMovesInsert = take takeamountBID $ randomRs (minDownMove, maxDownMove) stGen
      let list = take 20000 $ infiniteListDownChange 1.2 stGen downMovesInsert
      (all (> 0) list) `shouldBe` True
  
  -- POSITION TESTS
  describe "Lib.sumInts"  $ do
    it "returns sum of integers" $ do
      let list = [(11.2, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,1000)]
      let sum = sumInts list
      sum `shouldBe` 1700
  
  describe "Lib.spread' " $ do
    it "returns spread of best ask and best bid" $ do
      let bestAsk = 12.5 
      let bestBid = 12.499
      let spread = spread' bestAsk bestBid
      spread `shouldBe` 0.001

  describe "Lib.countElements" $ do
    it "returns the amount of a particular element in a list" $ do
      let list = [(1,"x"),(2,"x"),(3,"x"),(4,"y")]
      let amount = countElements "x" list 
      amount `shouldBe` 3
  
  describe "Lib.elementSize" $ do
    it "returns the overal size of a particular element" $ do
      let list = [(0,"x"),(2,"x"),(3,"x"),(4,"y")]
      let amount = elementSize "x" list 
      amount `shouldBe` 5

  -- OPEN INTEREST FUNCTIONS
  describe "Lib.interestorMinus" $ do
    it "returns negative open interest in a transaction" $ do
      let list1 = [(0,"z"),(2,"z"),(3,"x"),(4,"z"),(7,"x")]
      let list2 = [(0,"f"),(2,"f"),(3,"f"),(4,"y"),(7,"y")]
      let list3 = interestorMinus list1 list2
      list3 `shouldBe` 2

  describe "Lib.interestorPlus" $ do
    it "returns positive open interest in a transaction" $ do
      let list1 = [(0,"z"),(2,"z"),(3,"x"),(4,"z"),(7,"x")]
      let list2 = [(0,"f"),(2,"f"),(3,"f"),(4,"y"),(7,"y")]
      let list3 = interestorPlus list1 list2
      list3 `shouldBe` 7

-- TEMPLEATE runProgram FUNCTIONS

  describe "Lib.toIntegralLenght" $ do
    it "returns length of a list as a double" $ do
      let options = [DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW]
      let length = toIntegralLenght options
      length `shouldBe` 14

  describe "Lib.runProgramPercentage" $ do
    it "returns what positions have which runProgram periods" $ do
      let lengthEnd = 3
      let numberOfPositions = 1000
      let runProgram = runProgramPercentage numberOfPositions lengthEnd 
      runProgram `shouldBe` [333,667,1000]

  describe "Lib.processlist" $ do
    it "returns a current runProgram peroid from options" $ do
      let options = [DWW,DW,UP,UPP]
      let list =  [250,500,750,1000]
      let number = 999
      let runProgram = processlist options list number
      runProgram `shouldBe`  UPP

  describe "Lib.randomhandler" $ do
    it "returs second option passed into the function in case of `RANDOM` as first input" $ do
      let option = RANDOM
      let generatedOption = UPP
      let runProgram = randomhandler option generatedOption
      runProgram `shouldBe` UPP

  describe "Lib.optionProcessor" $ do
    it "returns a new probability for volume /UPP" $ do
      let option = UPP
      let probability = 20
      let newProbability = optionProcessor option probability
      newProbability `shouldBe` 80

  describe "Lib.optionProcessor" $ do
    it "returns a new probability for volume /DW" $ do
      let option = DW
      let probability = 20
      let newProbability = optionProcessor option probability
      newProbability `shouldBe` 40

{-   
  describe "Lib.processTempleaterunProgram" $ do
    it "returns a list of new volume probabilities" $ do
      let runProgramIndx = 99
      let option = DW
      let runProgram = processTempleaterunProgram runProgramIndx option
      runProgram `shouldBe` [0,0,0,0,0,0,0,0,0,100]
-}   
  describe "Lib.randomOptionGen" $ do
    it "returns one of 4 different options" $ do
      option <- randomOptionGen
      let withinRange = option `elem` [DWW,DW,CN,UP,UPP]
      withinRange `shouldBe` True
  
  describe "Lib.roundToTwoDecimals" $ do
    it "retunrs a double rounded to two decimals" $ do
      let number = 1.23456789
      let roundedNumber = roundToTwoDecimals number
      roundedNumber `shouldBe` 1.23
  -- FILE FUNCTIONS
  describe "Lib.isFileEmpty" $ do
    it "returns True if file is empty" $ do
      let file = "test/testfile.txt"
      isEmpty <- isFileEmpty file
      isEmpty `shouldBe` True

  describe "Lib.readBook" $ do
    it "retuns orderbook from a file" $ do
      let file = "test/testbook.json"
      book <- readBook file
      let result = [(0.9626,33587),(0.9718,7295),(0.9816,78128),(0.9907,255745)]
      book `shouldBe`  result
  

  describe "Lib.startingPointFromFile" $ do
    it "returns an error on negative starting price from json" $ do
      let file = "test/teststart.json"
      result <- startingPointFromFile file
      result `shouldBe` 0.1

  describe "Lib.generateVolumes" $ do
   it "returns volume with no negative ints" $ do
    let numTransactions = 10000000
    let totalVolume = 10000000
    volume <- generateVolumes numTransactions totalVolume
    let isNegative = any (<0) volume
    isNegative `shouldBe` False

  describe "Util.roundTo" $ do
    it "rounds double to a given number of decimals" $ do
      let number = 1.23456789
      let roundedNumber = roundTo 4 number
      roundedNumber `shouldBe` 1.2346

-- // all functions from library were tested above


testsUtil :: IO ()
testsUtil = hspec $ do

  -- | UTILITY TESTS
  describe "Util.sumList" $ do
    it "returns the sum of a list of integers inside a list" $ do
      sumList [1, 2, 3] `shouldBe` ([6] :: [Int])
  







  
  {-  
describe "Util.bookNumChange" $ do
  it "returns the amount that the orderbook changed"

-}

{-
  it "returns the first element of an *arbitrary* list" $
    property $ \x xs -> head (x:xs) == (x :: Int)

  it "throws an exception if used with an empty list" $ do
    evaluate (head []) `shouldThrow` anyException
-}