{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use withFile" #-}
{-# HLINT ignore "Use hPrint" #-}
{-# HLINT ignore "Use when" #-}
-- | module name

-- | external modules
import Control.Exception (bracket, throwIO)
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8 qualified as BC
import Data.Maybe (listToMaybe)
import System.IO
import Control.Parallel.Strategies
import System.Random
import Text.Printf (printf)
import Data.List (unfoldr)

-- | export modules
import Orderbook.Orderbook.App.Functions.AAGenerator
import Orderbook.Orderbook.App.Settings.Arunsettings
import Orderbook.Orderbook.App.Data.IOOS (generateOrderBook)
import Orderbook.Orderbook.App.Data.Afilepaths
    ( askBookPath, bidBookPath, pricePath, logPath )
import Orderbook.Orderbook.App.Source.UpdatedPrice 



main :: IO ()
main = do
     -- | CHECKING IF FILES ARE EMPTY:
    isBidEmpty
            :: Bool
            <- isFileEmpty bidBookPath
    isAskEmpty
            :: Bool
            <- isFileEmpty askBookPath
-- | IO buffering
    hSetBuffering stdout LineBuffering

    -- !! WIPING RUN == TRUE
    -- | when wiping run is running the whole code is not evaluated
    -- | wiping all of the text files, and changing the starting point
    if wipingRUN
      then do
        let sayStart
                 :: String
                 = show wipingStartingValue -- the wiping run will set this value to be the starting price
        putStrLn
          "🚨 YOU JUST RAN WIPING RUN 🚨 (you can swith it to False now in WipingRun :: Bool/ Arunsettings.hs)"
        putStr "\nnew starting value will be set to: $"
        putStrLn sayStart
        newRunSettings
          logPath
          bidBookPath
          askBookPath
          pricePath
          wipingStartingValue
   
      else do
      
       
-- | random generator:
        gen1
          <- randomGen
        gen2
          <- randomGen

        -- the price we are starting at
       
        startingPoint :: Double <- startingPointFromFile

  
-- making ask move upside
        let upMoves = take takeamountASK $ randomRs (minUpMove, maxUpMove) gen1
-- making bid move downside
        let downMoves =
              take takeamountBID $ randomRs (minDownMove, maxDownMove) gen2
        
        -- liquidity definition for ask, the limit setup gradient
        let setupASK
              :: [Double]
              =
              take takeamountASK (tail (infiniteList startingPoint gen1 upMoves)) `using` parList rseq

        -- liquidity definition for ask, the limit setup gradient
        let setupBID
             :: [Double]
             =
             take takeamountBID (tail (infiniteListDown startingPoint gen2 downMoves)) `using` parList rseq


        -- generating prices for ASKS $$ amount
        amountASK :: [Int]
         <- printCustomRandomList takeamountASK
        let usdamountASK :: [Int]
             =
                  amountASK :: [Int] -- // convertion into []

        -- generating prices for BIDS $$ amount
        amountBID :: [Int]
         <- printRandomList' takeamountBID

        let usdamountBID :: [Int]
             =
                  amountBID :: [Int] -- // converting into []

        -- !! WALLS:
        -- \| Price walls (limit)
        -- generate the size of limit walls (in terms of it's occurrence)
        let totakefromwall
              :: Int
              =
              -- how much to take form ^ function above
              taketowalls $ 2 * takeamount

        -- generating walls, this is an infinite list
        pricewalls <- randomListwalls
        let pricewalllist
                  :: [Int]
                  =
                  take totakefromwall pricewalls

        -- first part of the list above going to bids
        let pricesBids1
                  :: [Int]
                  =
                  firstPartList pricewalllist

        -- second part going to asks
        let pricesAsk1
                :: [Int]
                =
                secondPartList pricewalllist

        -- full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
        fullwallsASK
          :: [Int]
          <-
          randomlyInsert pricesAsk1 (take takeamountASK usdamountASK)

        -- full wall build, the list is 2* as long tho functions below will make it usable for bids and asks
        fullwallsBIDS
          :: [Int]
           <-
          randomlyInsert pricesBids1 (take takeamountBID usdamountBID)

        -- !! ADDING DATA TOGETHER
        -- \| adding orderbook together & generating additional data
        -- zipping so that we have orderwalls in  -> orderbook is built
        -- zipping prices with $ AMOUNT
        let orderbook_ask
              :: [(Double,Int)]
              =
              zipToTuples setupASK fullwallsASK

        let orderbook_bid
              :: [(Double,Int)]
              =
              zipToTuples setupBID fullwallsBIDS

        -- the orderbook which should change the bid price
        fileBidBook <- readBook bidBookPath
        fileAskBook <- readBook askBookPath 
        
        let bidBook
              :: [(Double,Int)]
              =
              if isBidEmpty
                then orderbook_bid
                else fileBidBook
        -- the orderbook which should change the ask price

     
        let askBook
              :: [(Double,Int)]
              =
              if isAskEmpty
                then orderbook_ask
                else fileAskBook

-- | price change
        let listofvolumes = [(500000,Buy),(500000,Buy),(500000,Buy),(500000,Buy),(500000,Buy)]

        (finalBidBook,finalAskBook) -- ! THE ONLY POSSIBLE ERROR, otherwise it does not seems to be in this file
          <- recursiveList listofvolumes bidBook askBook gen1 gen2 fullwallsASK fullwallsBIDS startingPoint totakefromwall

          -- ... do something with finalBidBook and finalAskBook, e.g., print them out ...
        let finaltxtpolish = removeEmptyLines pricePath
        finaltxtpolish
   
       


-- TODO Implement a price movements
-- TODO Implement every order with a set leverage, hence a liquidation price out of that, (the liq price will get calculated out of the enter price and liq)
-- TODO test liquidation behavior
-- TODO implement average distance between limits indicator
-- TODO fix nonsense price movements (sometimes happened)
-- TODO convert everything into bytestring

-- fix spread
-- look at correct bigger spread function
-- fix psychology
-- add bytestrings everywhere
-- optimize

-- connect external volume


-- ! FIX rewriting the orderbook
