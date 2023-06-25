import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import System.Random
import Data.Maybe




import Util
import RunSettings
import Lib
import DataTypes

main :: IO ()
main = tests

tests :: IO ()
tests = hspec $ do

 -- todo move this one lower 
  describe "Util.sumList" $ do
    it "returns the sum of a list of integers inside a list" $ do
      sumList [1, 2, 3] `shouldBe` ([6] :: [Int])
  
 -- ORDERBOOK TESTS  
  describe "Util.randomListwalls" $ do
    it "returns limit walls in a correct size" $ do
      walls <- randomListwalls 
      (all (\wall -> wall >= wallminimum' && wall <= wallmaximum') (take 1000 walls)) `shouldBe` True
 
  describe "Util.customRndList" $ do
    it "returns the correct size ranom number" $ do
     customRndList <- printCustomRandomList 1000
     (all (\list -> list >= minimum' && list <= maximum') customRndList) `shouldBe` True
 
  describe "Util.wallmximum'" $ do
    it "returns rational size limit wall maximum" $ do
      wallmaximum' < 500 * maximum'  `shouldBe`  True
 
  describe "Util.orderwalllikelyhood" $ do
    it "returns rational amount of limit walls" $ do
      orderwalllikelyhood > 10 `shouldBe` True
  
  describe "Util.newNumberDown" $ do
    it "returns non negative down step in the orderbook" $ do
      stGen <- newStdGen'
      let input  =  [11.5, 12.5, 1, 999] 
      let number = fst $ nextNumberDown input 11.2 stGen
      number > 0 `shouldBe` True
 
  describe "Util.newNumberUp" $ do
    it "returns non negative up step in the orderbook" $ do
      stGen <- newStdGen'
      let input  =  [11.5, 12.5, 1, 999] 
      let number = fst $ nextNumberUp input 11.2 stGen
      number > 0 `shouldBe` True
 
  describe "Util.infiniteListUpConstant" $ do
    it "returns non negative list from infiniteListUpConstant" $ do
      stGen <- newStdGen'
      let upMovesInsert = take takeamountASK $ randomRs (minUpMove, maxUpMove) stGen
      let list = take 1000 $ infiniteListUpConstant 1.2 stGen upMovesInsert 
      (all (>= 0) list) `shouldBe` True
 
  describe "Util.infiniteListDownConstant" $ do
    it "returns non negative list from infiniteListDownConstant" $ do
      stGen <- newStdGen'
      let downMovesInsert = take takeamountBID $ randomRs (minDownMove, maxDownMove) stGen
      let list = take 20000 $ infiniteListDownConstant 1.2 stGen downMovesInsert
      (all (> 0) list) `shouldBe` True
 
  describe "Util.sumAt" $ do
    it "returns sumed element at a desired position" $ do
      let list = [1,2,3,4,5]
      let element = 6
      let position = 2
      let newList = sumAt position element list
      newList `shouldBe` [1,2,9,4,5]

  describe "Util.firstPartList,Util.secondPartList" $ do
    it "returns list that is split in half" $ do
      let list = [1,2,3,4,5,6,7]
      let (frst,scnd) = (firstPartList list, secondPartList list)
      (frst,scnd) `shouldBe` ([1,2,3,4],[5,6,7])
  
  describe "Util.minList,Util.maxList" $ do
    it "returns min and max from lists" $ do
      let listOfLists = [[1,2,3],[4,5,6],[7,8,9]] :: [[Int]]
      let (mn,mx) = (minList listOfLists, maxList listOfLists)
      (mn,mx) `shouldBe` (Just 1, Just 9)

  describe "Util.orderbookChange" $ do
    it "returns orderbook with changed values" $ do
      let book = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,100000)]
      let volume = 701
      let processedBook = orderbookChange book volume
      processedBook `shouldBe` [(15,99999)]
  
  describe "Util.bookNumChange" $ do
    it "returns the amount that the orderbook changed" $ do
      let book = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,100000)]
      let bookChange = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0)]
      let changedBook = bookNumChange book bookChange
      changedBook `shouldBe` 1

  describe "Util.infiniteListUpChange" $ do
    it "returns non negative price grid for up list" $ do
      stGen <- newStdGen'
      let upMovesInsert = take takeamountASK $ randomRs (minUpMove, maxUpMove) stGen
      let list = take 1000 $ infiniteListUpChange 1.2 stGen upMovesInsert 
      (all (>= 0) list) `shouldBe` True
  
  describe "Util.infiniteListDownChange" $ do
    it "returns non negative price grid for down list" $ do
      stGen <- newStdGen'
      let downMovesInsert = take takeamountBID $ randomRs (minDownMove, maxDownMove) stGen
      let list = take 20000 $ infiniteListDownChange 1.2 stGen downMovesInsert
      (all (> 0) list) `shouldBe` True
  
  -- POSITION TESTS
  describe "Util.sumInts"  $ do
    it "returns sum of integers" $ do
      let list = [(11.2, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,1000)]
      let sum = sumInts list
      sum `shouldBe` 1700
  
  describe "Util.spread' " $ do
    it "returns spread of best ask and best bid" $ do
      let bestAsk = 12.5 
      let bestBid = 12.499
      let spread = spread' bestAsk bestBid
      spread `shouldBe` 0.001

  describe "Util.countElements" $ do
    it "returns the amount of a particular element in a list" $ do
      let list = [(1,"x"),(2,"x"),(3,"x"),(4,"y")]
      let amount = countElements "x" list 
      amount `shouldBe` 3
  
  describe "Util.elementSize" $ do
    it "returns the overal size of a particular element" $ do
      let list = [(0,"x"),(2,"x"),(3,"x"),(4,"y")]
      let amount = elementSize "x" list 
      amount `shouldBe` 5

  -- OPEN INTEREST FUNCTIONS
    describe "Util.interestorMinus" $ do
      it "returns negative open interest in a transaction" $ do
        let list1 = [(0,"z"),(2,"z"),(3,"x"),(4,"z"),(7,"x")]
        let list2 = [(0,"f"),(2,"f"),(3,"f"),(4,"y"),(7,"y")]
        let list3 = interestorMinus list1 list2
        list3 `shouldBe` 2

    describe "Util.interestorPlus" $ do
      it "returns positive open interest in a transaction" $ do
        let list1 = [(0,"z"),(2,"z"),(3,"x"),(4,"z"),(7,"x")]
        let list2 = [(0,"f"),(2,"f"),(3,"f"),(4,"y"),(7,"y")]
        let list3 = interestorPlus list1 list2
        list3 `shouldBe` 7

  -- TEMPLEATE RUN FUNCTIONS
  
    describe "Util.toIntegralLenght" $ do
     it "returns length of a list as a double" $ do
       let options = [DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW,DWW]
       let length = toIntegralLenght options
       length `shouldBe` 14

    describe "Util.runPercentage" $ do
     it "returns what positions have which run periods" $ do
        
        let lengthEnd = 3
        let numberOfPositions = 1000
        let run = runPercentage numberOfPositions lengthEnd 
        run `shouldBe` [333,667,1000]

  
  
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