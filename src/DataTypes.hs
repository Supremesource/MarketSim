
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia, DataKinds #-}

module DataTypes where
-- | external libraries
import System.Random ( StdGen )
import Deriving.Aeson


-- ? JSON Serialization
type JSONConfig a = CustomJSON '[OmitNothingFields, FieldLabelModifier '[StripPrefix "DATA", CamelToSnake]] a


data FileWritesLog = FileWritesLog
   {  identifierLOG    :: String
    , startingPointLOG :: Double -- this is pretty much an Double, data type StartingPoint
    , takeamountLOG  :: Int
    , maxUpMoveLOG :: Double
    , minUpMoveLOG :: Double
    , maxDownMoveLOG :: Double
    , minDownMoveLOG :: Double
    , minimum'LOG :: Int
    , minimumActualLOG :: Int
    , maximum'LOG :: Int
    , maximumActualLOG :: Int
    , takeamountBIDLOG :: Int
    , takeamountASKLOG :: Int
    , asksTotalLOG :: Int
    , bidsTotalLOG :: Int
    , orderwalllikelyhoodLOG :: Int
    , totakefromwallLOG :: Int -- this is pretty much an INT, data type TakeFromWall
    , wallminimum'LOG :: Int
    , wallmaximum'LOG :: Int
    , wallAmplifierLOG :: Int
    , maxDecimalLOG :: Int
    , lengthchangeBIDLOG :: Int
    , lengthchangeASKLOG :: Int
    , listASKLOG :: [(Double, Int)]
    , listBIDLOG :: [(Double, Int)]
    , vSideLOG :: String -- this is pretty much string, data type VolumeSid
    , volumeAmountLOG :: Int
    , spreadLOG :: Double
    , startingPriceLOG :: Double
    } deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig FileWritesLog


data FileWriteBook = FileWriteBook
  { startingPriceBook :: Double
  , bidAskRatioBook :: String -- formated with printf
  , bidsTotalBook :: Int
  , asksTotalBook :: Int
  } deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig FileWriteBook


data PositionData = PositionData
  {totalXPosition             :: Int
  ,totalYPosition             :: Int
  ,totalZPosition             :: Int
  ,totalFPosition             :: Int
  ,buyVolumePosition          :: Int
  ,sellVolumePosition         :: Int
  ,overalVOLUMEPosition       :: Int
  ,overalOpenInterestPosition :: Int
  }deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig PositionData

data InitPrice where
  InitPrice :: {initPrice :: Int} -> InitPrice
  deriving Generic
  deriving (FromJSON, ToJSON) via JSONConfig InitPrice

data Book = Book {
  book :: [(Double, Int)]
} deriving (Show, Generic, FromJSON, ToJSON)



initialPositionData :: [PositionData]
initialPositionData = []



-- data PositionsOutput = Output
--   { ops :: [Op]
--   , princes :: [Prices]
--   -- ...
--   }


-- ? Types

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

--type Position = ([(Int, String)], [(Int, String)])

data Transaction = Transaction {
    takerSide :: TakerTuple
  , makerSide :: MakerTuple
} deriving (Show, Generic, FromJSON, ToJSON)