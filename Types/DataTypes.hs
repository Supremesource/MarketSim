{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module DataTypes where
import System.Random ( StdGen )  
import System.IO ( Handle )

-- | defining data typ for volume side
data VolumeSide
  = Buy
  | Sell
   deriving (Show, Eq, Ord, Enum, Bounded)

-- | maker tuple structure
type MakerTuple = [(Int, String)]

-- | taker tuple structure
type TakerTuple = [(Int, String)]

-- | stats that can be extracted
data Stats = Stats
  { overallOI   :: Int
  , totalVolume :: Int
  , buyVolume   :: Int
  , sellVolume  :: Int
  , takerXc     :: Int
  , takerYc     :: Int
  , takerZc     :: Int
  , takerFc     :: Int
  , makerXc     :: Int
  , makerYc     :: Int
  , makerZc     :: Int
  , makerFc     :: Int
  , offX        :: Int
  , offY        :: Int
  , offF        :: Int
  , offZ        :: Int
  , takerX      :: Int
  , takerY      :: Int
  , takerZ      :: Int
  , takerF      :: Int
  , makerX      :: Int
  , makerY      :: Int
  , makerZ      :: Int
  , makerF      :: Int
  } deriving Show

data Options = 
               UP | 
              UUP | 
              DW  | 
              DWW | 
              CN  |
              RANDOM
              deriving  (Eq, Show, Enum, Bounded)

type OrderBook = [(Double, Int)]
type VolumeList = [(Int, VolumeSide)]
type Generator = StdGen
type FullWall = [Int]
type StartingPoint = Double
type Totakefromwall = Int
type Volume = (Int, VolumeSide) 


type RewriteHandle3 = (Handle, Handle, Handle, Handle, Handle, Handle, Handle, Handle)

type InitBookStats = (StartingPoint , [[Int]] , Int , Int , Totakefromwall , Int , Int, [(Double,Int)],  [(Double,Int)] , VolumeSide, Int, Double, Double, Double)

data BookStats = BookStats { 
                     startingPoint :: StartingPoint
                   , maxMinLimit :: [[Int]]                   
                   , asksTotal :: Int
                   , bidsTotal :: Int
                   , totakefromwall :: Totakefromwall
                   , lengthchangeBID :: Int
                   , lengthchangeASK :: Int
                   , listASK :: [(Double, Int)]
                   , listBID :: [(Double, Int)]
                   , vSide :: VolumeSide
                   , volumeAmount :: Int
                   , spread :: Double
                   , startingprice :: Double
                   , bidAskRatio :: Double                    
                   }

type RecursionPass = (VolumeList, OrderBook, OrderBook, Generator, Generator, FullWall, FullWall, StartingPoint, Totakefromwall, [BookStats])

type ListPass = (Volume , OrderBook , OrderBook, Generator, Generator, FullWall, FullWall, StartingPoint, Totakefromwall )