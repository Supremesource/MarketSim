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
{-
-- ! DESCRIPTION
Checking correctnes of the generated data,
crucial when it comes to backtesting data 
  ##############################
& # CONTENTS:                  #
~ # specific functions:        #
- # library    tests           #
- # utility    tests           #
~ # working functionality      #
- # poscycle   tests           #
- # generator  tests           #
- # main       tests            #
- # inputOut   tests           #
~ # statistics                 #
- # statistics tests           #
~ # live json checking         #
- # output & data tests        #
  ##############################
-}

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
import           Data.Foldable        (toList)
-- | Internal libraries
import Lib
import RunSettings
import Util
import DataTypes
import PosCycle
import Statistics
import Generator
import Filepaths
import NRandomFunc




main :: IO ()
main = tests

tests :: IO ()
tests = do
  --testsLib
  --testsUtil
  testsPosCycle

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
      let processedBook = orderbookChange (fromList book) volume
      processedBook `shouldBe` (fromList [(15,99999)])
  
  describe "Lib.bookNumChange" $ do
    it "returns the amount that the orderbook changed" $ do
      let book = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,100000)]
      let bookChange = [(11.5, 100), (12.5,200),(13.5,400),(14,0),(14.1,0)]
      let changedBook = bookNumChange (fromList book) (fromList bookChange)
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
      let list = fromList [(11.2, 100), (12.5,200),(13.5,400),(14,0),(14.1,0),(15,1000)]
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
  describe "Util.aggregateStats" $ do
    it "returns proper stats about a transaction" $ do
          let (takerSide,makerSide)=([(100,"x"),(10,"z"),(1,"x"),(0,"z")],[(90,"f"),(21,"y")])      
          let takerSide' = []
          let makerSide' = []
          let startingStats = initStats 
          aggregateStats (takerSide,makerSide) startingStats
            `shouldBe` Stats
                        { overallOI   = 11
                        , totalVolume = 111
                        , buyVolume   = 111
                        , sellVolume  = 0
                        , takerXc     = 2
                        , takerYc     = 0
                        , takerZc     = 2
                        , takerFc     = 0
                        , makerXc     = 0
                        , makerYc     = 1
                        , makerZc     = 0
                        , makerFc     = 1
                        , offX        = 101
                        , offY        = 21
                        , offF        = 90
                        , offZ        = 10
                        , takerX      = 101
                        , takerY      = 0
                        , takerZ      = 10
                        , takerF      = 0
                        , makerX      = 0
                        , makerY      = 21
                        , makerZ      = 0
                        , makerF      = 90
                        }
  
  describe "Util.calculateVolumes" $ do
    it "returns the volumes amount in order Sell, Buy" $ do
      let initVolSide = Buy
      let initVolAmount = 1000
      calculateVolumes initVolSide initVolAmount 
        `shouldBe` (0,1000)

  describe "Util.calculateBooks" $ do
    it "returns updater orderbooks based on volume" $ do
     let (bidVol, askVol) = (200,0)
     let orderBookBid     = fromList [(100.1,200),(0.1,0)]
     let orderBookAsk     = fromList [(100.1,0),(101,100),(0.1,901)] 
     calculateBooks bidVol askVol orderBookBid orderBookAsk
      `shouldBe`((fromList [(0.1,0)]),(fromList [(100.1,0),(101,100),(0.1,901)]))

  describe "Util.calculateFinalBooks" $ do
    it "returns correctly processed orderBooks" $ do
      let volS            = Sell
      let orderBookAskU   = fromList [(0.01 ,0) ,(0.02 ,1) ,(0.021 ,3)] 
      let listAsk         = fromList [(0.001,0) ,(0.002,1) ,(0.0021,3)] 
      let orderBookAskN   = fromList [(0.01 ,0) ,(0.02 ,5) ,(0.021 ,0)] 
      let orderBookBidU   = fromList [(0.05,10) ,(0.02 ,0) ,(0.01  ,3)] 
      let listBid         = fromList [(0.01,0)  ,(0.001,5) ,(0.0021,1)] 
      let orderBookBidN   = fromList [(0.01,0)  ,(0.02 ,5) ,(0.021 ,0)] 
      calculateFinalBooks volS orderBookAskU listAsk 
                          orderBookAskN orderBookBidU listBid orderBookBidN
        `shouldBe` ((listAsk >< orderBookAskN), orderBookBidU)

  describe "Util.lengthChanges" $ do
    it "returns by how much orderbook size changed" $ do
      let orderBookBid     = fromList [(100.1,200),(0.1,0)]
      let orderBookAsk     = fromList [(100.1,0),(101,100),(0.1,901)] 
      let changedBookBid   = fromList []
      let changedBookAsk   = fromList []
      lengthChanges orderBookBid changedBookBid orderBookAsk changedBookAsk
       `shouldBe` (2,3)

  describe "Util.price"  $ do
    it "returns price based on volume and orderbook" $ do 
      let volS = Sell
      let orderBookBid     = fromList [(100.1,200),(0.1,0)]
      let orderBookAsk     = fromList [(100.1,0),(101,100),(0.1,901)] 
      price volS orderBookBid orderBookAsk 
       `shouldBe` 100.1

  describe "Util.calculateBookLists" $ do
    it "returns non negative & correctly setup ask and bid list" $ do
      let askSetupGrid   = [0,0.0001,119]
      let bidSetupGrid   = [100,0000.9,0]
      let askSetupAmount = [100,0,400]
      let bidSetupAmount = [0,200,300,100] 
      let finalSetupAsk  = fromList [(0,      round $ 100 / divisionValue)
                                   , (0.0001, round $ 0   / divisionValue )
                                   , (119,    round $ 400 / divisionValue )]
      
      let finalSetupBid  = fromList [(100 , round $ 0        / divisionValue)
                                  ,(0000.9, round $ 200      / divisionValue)
                                  ,(0     , round $ 300      / divisionValue)]  
    
      calculateBookLists askSetupGrid bidSetupGrid askSetupAmount bidSetupAmount
        `shouldBe` (finalSetupAsk,finalSetupBid)

  describe "Util.calculateFirstElements" $ do
    it "returns first elements of orderbooks" $ do
      let orderBookBid     = fromList [(100.1,200),(0.1,0)]
      let orderBookAsk     = fromList [(100.1111,0),(101,100),(0.1,901)] 
      calculateFirstElements orderBookBid orderBookAsk
        `shouldBe` (100.1,100.1111)

  describe "Util.calculateTotals" $ do
   it "returns total volume in the orderbook" $ do
     let orderBookBid     = fromList [(100.1,200),(0.1,0)]
     let orderBookAsk     = fromList [(100.1111,0),(101,100),(0.1,901)] 
     calculateTotals orderBookBid orderBookAsk
      `shouldBe` (200,1001)

  describe "Util.calculateSetupInserts" $ do
   it "returns non-negative setupGrid (limit grid) of orderbook ask & bid" $ do
    let lChangeAsk = 0
    let lChangeBid = 9999
    let startingPrice = 100.01
    gen1 <- randomGen
    gen2 <- randomGen
    let (grid1,grid2) = calculateSetupInserts lChangeAsk lChangeBid startingPrice gen1 gen2
    let axiom = all (>= 0) grid1 && all (>= 0) grid2
    axiom `shouldBe` True

  describe "Util.calculateTotalsCount" $ do
    it "returns correct length of orderbooks" $ do
      let orderBookBid     = fromList []
      let orderBookAsk     = fromList [(100.1111,0),(101,100),(0.1,901)] 
      calculateTotalsCount orderBookBid orderBookAsk
        `shouldBe` (0,3)

  describe "Util.sumList" $ do
    it "returns the sum of a list of integers inside a list" $ do
      sumList [1, 2, 3] `shouldBe` ([6] :: [Int])


testsPosCycle :: IO ()
testsPosCycle = hspec $ do
  
  describe "PosCycle.closingConversion" $ do
    it "returns opened positioning into closed positioning" $ do
      let takerSide  = [(0,"x"), (0,"z"),(-1,"x"),(99999,"x")]
      let makerSide  = []
      let takerSide' = []
      let makerSide' = [(100,"f"),(-1,"y")]
      let result01   = closingConversion (takerSide  , makerSide)
      let result02   = closingConversion (takerSide' , makerSide')      
      result01 `shouldBe` ([(0,"f"),(-1,"f"),(99999,"f")], [])
      result02 `shouldBe` ([], [(-1,"z")])
  

  describe "PosCycle.positionFuture" $ do
    it "returns correctly setup position future" $ do
      let price'    = 1
      let takerSide = [(0,"x"),(-10,"z"),(10,"z"),(-15,"x")]  
      let makerSide = [(0,"y"),(-10,"f"),(10,"f"),(-15,"y")]  
      result <- positionFutureNonRandom price' (takerSide,makerSide) 
      result `shouldBe` [(0.8,0,"f"),(0.8,-15,"f"),(1.2,0,"z"),(1.2,-15,"z")]

  describe "PosCycle.oppositeSide" $ do
    it "returns opposite side" $ do
      let side = "z"
      let expected = oppositeSide side
      expected `shouldBe` "y"

  describe "PosCycle.initGenerator" $ do
    it "returns initial taker and maker side from volume" $ do
      let takerVolList  = [0,10,-9]  
      let makerVolList  = [1,0,-7]
      result <- nonRandominitGenerator takerVolList makerVolList 
      result `shouldBe` ([(0,"x"),(10,"x"),(-9,"x")],[(1,"y"),(0,"y"),(-7,"y")]) 

  describe "PosCylcle.normalGenerator" $ do
    it "returns normally (with closing positioning) generated future info" $ do
      let takerVolList  = [0,10,9]  
      let makerVolList  = [1,12,7]
      let oldPosFuture1  = fromList [(0,0,"f"),(0,0,"f"),(0,0,"f"),(0,0,"f")]
      let oldPosFuture2  = fromList [(0,0,"z"),(0,0,"z"),(0,0,"z"),(0,0,"z")]
      let oldPosinfo = (oldPosFuture1,oldPosFuture2)
      let oldPosFuture1'  = fromList [(1,900,"f"),(100,0,"f"),(0,0,"f"),(0,0,"f")]
      let oldPosFuture2'  = fromList [(0.9,900,"z"),(2.2,0,"z"),(0,0,"z"),(0,0,"z")]
      let oldPosinfo' = (oldPosFuture1',oldPosFuture2')
      let liqside1 = ""
      let liqside2 = "z"
      result1  <- nonRandomNormalGenerator takerVolList makerVolList oldPosinfo  liqside1 "y"
      result2  <- nonRandomNormalGenerator takerVolList makerVolList oldPosinfo' liqside2 "y"
      -- without liquidaiton (future does not allow for closing)
      result1 `shouldBe` ([(0,"y"),(10,"y"),(9,"y")],[(1,"x"),(12,"x"),(7,"x")])
      -- with liquidation (counterparty is set to opening)
      result2 `shouldBe` ([(19,"z")],[(1,"y"),(12,"y"),(7,"y")]) 

  describe "PosCycle.filterClosePos" $ do
    it "retruns future filtered to a specific element" $ do 
      let element      = "f"
      let element2     = "z"
      let element3     = "f" -- ~ this is the liquidation 
{-
      let liquidation  = "no"
      let liquidation2 = "yes"
      let liquidation3 = "yes" -- ~ f
-}
      let transaction  = TransactionFut [(0.0,0,""),(100,1,"f"),(1.11,20,""),(10.1,9,"f")]
      let transaction2 = TransactionFut [(0,0,"f")] 
      let transaction3 = TransactionFut 

      let result       = filterClosePos {-liquidation-} element transaction
      let result2      = filterClosePos {-liquidation2-} element2 transaction2
      
      result  `shouldBe` [(100,1,"f"),(10.1,9,"f")]
      result2 `shouldBe` [] 

  describe "PosCycle.filterTuple" $ do
    it "retruns list of tuples (positioning) filtered to a specific element" $ do
      let element       = "f"
      let element2      = "z"
      let positioining  = [(100,"z"),(1,"f"),(0,"f"),(10,"")]
      let positioining2 = [(0,"f")]
      let result        = filterTuple element positioining
      let result2       = filterTuple element2 positioining2
      result  `shouldBe` [(1,"f"),(0,"f")]
      result2 `shouldBe` []

  describe "PosCylcle.allThirdEqual" $ do
    it "return -> True if all elements of future are equal and -> False if not" $ do
      let future1 =  fromList [(0.0,0,""),(0,0,"f"),(0,0,"f"),(0,0,"f"),(0,0,"f")]
      let future2 =  fromList [(0,0,"z"),(0,0,"f"),(0,0,"f"),(0,0,"f")]
      let future3 =  fromList [(0,0,"z")]
      let (result1, result2, result3) 
                               = (allThirdEqual future1, allThirdEqual future2, allThirdEqual future3)
      result1 `shouldBe` True
      result2 `shouldBe` False
      result3 `shouldBe` True

    
  describe "PosCycle.allEqual" $ do
    it "returns True if all elements in the list are equal, False if not" $ do
      let list1 = [(100,"x"),(0,"y")]
      let list2 = [(100,"x"),(100,"x")]
      let result1 = allEqual list1
      let result2 = allEqual list2
      result1 `shouldBe` False
      result2 `shouldBe` True

  describe "PosCycle.filterFutureClose" $ do
    it "returns (non-randomized (test)) filtered future in order (futureLong,futureShort)" $ do
      let positioning1 = [(100,"x"),(0,"z"),(300,"z")]
      let positioning2 = [(100,"y"),(0,"f"),(300,"f")]
      let positioning  = (positioning1, positioning2)
      let futureL =  fromList [(0,0,"z"),(0,300,"z"),(0,200,"z"),(0,0,"z")]
      let futureS =  fromList [(0.0,200,"f"),(0,0,"f"),(0,110,"f"),(0,0,"f"),(0,0,"f")]
      let (oldfuturelong,oldfutureshort) = (futureL,futureS)
      result <- nonRandomizedfilterFutureClose positioning (futureL, futureS)
      result `shouldBe` (fromList [(0.0, 200, "z")], fromList [(0.0, 10, "f")])

  describe "PosCycle.tuplesToSides" $ do
    it "returns positioning in order (long,short)" $ do
      let takerPos = [(100,"x"),(200,"z"),(0,"x")]
      let makerPos = [(0,"y"),(0,"f")]
      let pos = (makerPos,takerPos)
      let result = tuplesToSides pos 
      result `shouldBe` (takerPos,makerPos)

  describe "PosCycle.randomLiquidationEvent" $ do
     it "returns liquidation event (stop in this case, hardcoded to test)" $ do
      result <- nonRandomLiquidationEvent
      result `shouldBe` "stp"
  
  describe "PosCycle.liquidationDuty" $ do
      it "returns future info with liquidations taken out" $ do
        let futureInfoL     = fromList [(0,100,"f"),(0.5,200,"f"),(0.45,300,"f"),(150,150,"f")]
        let futureInfoS     = fromList [(400,100,"z"),(1,200,"z"),(1,300,"z"),(0.51,150,"z")]
        let price           = 0.5
        result <- nonRandomLiquidationDuty futureInfoL futureInfoS price
        let liquidationInfo' = fromList [(200,"f","stp"), (150,"f","stp")]
        let expectedFutureL' = fromList [(0,100,"f"),(0.45,300,"f")]
        let expectedFutureS' = fromList [(400,100,"z"),(1,200,"z"),(1,300,"z"),(0.51,150,"z")]
        result `shouldBe` (liquidationInfo', (expectedFutureL', expectedFutureS'))
 
 
  describe "PosCycle.normalrunProgram" $ do
    it "returns correctly managed position management" $ do
      -- $ first
      let volList      = ( [100,200],  -- taker
                           [50 ,250])  -- maker
      let futureInfo1  = Data.Sequence.fromList [(900,100000,"f")] -- FUTURE INFO LONG
      let futureInfo2  = Data.Sequence.fromList [(900,100000,"z")] -- FUTURE INFO SHORT
      let startingPric = 1000.0  
      let liqside      = ""
      -- $ second
      let volListB     = ([300],       -- | TAKER | - buy taker
                          [40,260])    -- | MAKER | - sell taker                 
      let futureInfo1B  = Data.Sequence.fromList [(14000,100000,"f")] -- FUTURE INFO LONG
      let futureInfo2B  = Data.Sequence.fromList [(1200,70000,"z")  ] -- FUTURE INFO SHORT
      let startingPricB = 1000.0  
      let liqsideB      = "f"
      -- $ third
      let volListC     = ([300,0,400,500],      -- | TAKER | - buy taker
                          [0,1200])             -- | MAKER | - sell taker                 
      let futureInfo1C  = Data.Sequence.fromList [(0.0,100,"f"), (10,700,"f") ]    -- FUTURE INFO LONG
      let futureInfo2C  = Data.Sequence.fromList [(0.0,70000,"z") , (9999,0,"z") ] -- FUTURE INFO SHORT
      let startingPricC = 999.99  
      let liqsideC      = "z"
      -- $ fourth
      let volListD        = ([200,300,400,100], [300,700]) --  adds up to 1000
      let futureInfo1D    = Data.Sequence.fromList [(10,500,"f"), (50,900,"f"),(100,1000,"f")]
      let futureInfo2D    = Data.Sequence.fromList [(30,400,"z"), (14,500,"z"),(10,3000,"z")]
      let startingPricD   = 1000
      let liqsideD        = ""

      result  <- nonRandomNormalrunProgram volList   (futureInfo1,futureInfo2)      startingPric  liqside  "y"
      result2 <- nonRandomNormalrunProgram volListB  (futureInfo1B, futureInfo2B)   startingPricB liqsideB "y"  
      result3 <- nonRandomNormalrunProgram volListC  (futureInfo1C, futureInfo2C)   startingPricC liqsideC "y"
      result4 <- nonRandomNormalrunProgram volListD  (futureInfo1D, futureInfo2D)   startingPricD liqsideD "f"
      -- ? positioning on taker is set to be y (without liquidation)
      result  `shouldBe`  ((Data.Sequence.fromList [(900,100000,"z"), (1200,100,"z"), (1200,200,"z")], Data.Sequence.fromList [(900,100000,"f"),(800,50,"f"),(800,250,"f")]),([(100,"y"),(200,"y")],[(50,"x"),(250,"x")]))
      result2 `shouldBe`  ((Data.Sequence.fromList [(1200,70000,"z")],Data.Sequence.fromList [(14000,99700,"f"),(800.0,40,"f"),(800.0,260,"f")]),([(300,"f")],[(40,"x"),(260,"x")]))
      result3 `shouldBe`  ((Data.Sequence.fromList [(0.0,68800,"z") ,(1199.988, 0, "z"), (1199.988, 1200, "z")],Data.Sequence.fromList [(0.0,100,"f"), (10,700,"f") ]),([(1200,"z")],[(0,"y"),(1200,"y")]))
      result4 `shouldBe`  ((Data.Sequence.fromList [(30.0,400,"z"),(14.0,500,"z"),(10.0,3000,"z")],Data.Sequence.fromList [(50.0,585,"f"),(100.0,630,"f"),(10.0,185,"f"),(800.0,300,"f"),(800.0,700,"f")]),([(200,"f"),(300,"f"),(400,"f"),(100,"f")],[(300,"x"),(700,"x")]))

    
  describe "PosCycle.splitAmountToRandomList" $ do
    it "returns (non-randomized (test) + randomized) volume split into smaller parts" $ do
      result'h <- noRandomSplitAmountToRandomList 500
      result'd <- noRandomSplitAmountToRandomList 100
      result'c <- noRandomSplitAmountToRandomList 200
      result'j <- noRandomSplitAmountToRandomList 600
      resultRandom <- splitAmountToRandomList 100

      result'h `shouldBe` [275,25,25,25,25,25,25,25,25,25]
      result'd `shouldBe` [55,5,5,5,5,5,5,5,5,5]
      result'c `shouldBe` [110,10,10,10,10,10,10,10,10,10]
      result'j `shouldBe` [330,30,30,30,30,30,30,30,30,30]
      sum resultRandom `shouldBe` 100 
  


  describe "PosCycle.filterCloseAmount" $ do
    it "returns (non-randomized (test) + randomized) filtered future/close out of closing elements  " $ do
      let transactionShort  = [(500,"z"), (100,"z"),(200,"z")] 
      let oldPosCloseShort  = fromList [(0,100,"z"),(0,400,"z"),(0,900,"z"),(0,600,"z")]
     
      let transactionLong   = [(600,"f")]                        
      let oldPosCloseLong   = fromList [(0,400,"f"), (0,300,"f"),(0,200,"f")] 

      result1 <- nonRandomFilterCloseAmount transactionShort oldPosCloseShort
      -- ? result 2 goes:
      -- --------------------------------
      -- 1 | 70-300-200   -> 300-200-70 |
      -- 2 | 270-200-70   -> 200-70-270 |
      -- 3 | 170-70-270   -> 70-270-170 |
      -- 4 | 40-270-170   -> 270-170-40 |
      -- 5 | 240-170-40   -> 170-40-240 |
      -- 6 | 140-40-240   -> 40-240-140 |
      -- 7 | 10-240-140   -> 240-140-10 |
      -- 8 | 210-140-10   -> 140-10-210 |
      -- 9 | 110-10-210   -> 10-210-110 |
      -- ~  note how this result adds up to 300
      -- 10| 110 - 190                  | -- ? it reverts twice (last element)
      -- --------------------------------
      result2       <- nonRandomFilterCloseAmount transactionLong  oldPosCloseLong
      
     -- TODO:
     -- result3Random <- filterCloseAmount transactionLong oldPosCloseLong
   
   
      result1 `shouldBe`  (fromList [(0.0,105,"z"),(0.0,725,"z"),(0.0,370,"z")] )
      result2 `shouldBe`  (fromList [(0.0, 110, "f"), (0.0, 190, "f")])
      -- TODO, fix:
      -- (\[_,amt,_] -> sum amt) (toList result3Random) result2 `shouldBe` 300



-- TODO
-- decide whever to dele or keep cause of function above -- ~ mby take inspir.
{-

  describe "PosCycle.filterFutureAmount" $ do
    it "returns filtered future based on a transaction" $ do
      let transactionlong     = [(1362290,"f")]
      let oldPosFuturelong    = fromList [(0.0,0,""),(0.0,8019300,"f"),(0.0,51732996,"f"),(0.0,23747529,"f"),(0.0,44156108,"f"),(0.0,75789467,"f")]
      let transactionshort    = [(1362290,"z")]
      let oldPosFutureshort   = fromList [(185.4988,58390006,"z"),(222.4652,8833436,"z"),(222.4652,49384180,"z"),(222.4652,85475488,"z")]
      let transactionlongBig  = [(10000,"f")]
      let oldPosFuturelongBig = fromList [(0,100,"f"),(0,400,"f"),(0,900,"f"),(0,600,"f"),(0,8001,"f")]
      let result1             = filterFutureAmount transactionlong oldPosFuturelong
      let result2             = filterFutureAmount transactionshort oldPosFutureshort
      let result3             = filterFutureAmount transactionlongBig oldPosFuturelongBig
      result1                   `shouldBe` ( fromList [(0.0,6657010,"f"),(0.0,51732996,"f"),(0.0,23747529,"f"),(0.0,44156108,"f"),(0.0,75789467,"f")])
      result2                   `shouldBe` ( fromList [(185.4988,57027716,"z"),(222.4652,8833436,"z"),(222.4652,49384180,"z"),(222.4652,85475488,"z")])
      result3                   `shouldBe` ( fromList [(0,1,"f")]) 

-} 











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