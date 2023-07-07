
{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia, DataKinds #-}

module DataTypes where
-- | external libraries
import System.Random ( StdGen )
import Deriving.Aeson
import Data.Sequence (Seq)

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
  { identifierBook :: String 
  , startingPriceBook :: Double
  , bidAskRatioBook :: String -- formated with printf
  , bidsTotalBook :: Int
  , asksTotalBook :: Int
  , maxMinLmtBook :: (Int,Int)
  , vSideBook :: VolumeSide
  , volumeAmountBook :: Int
  , spreadBook :: String -- fotmate with printf
  } deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig FileWriteBook

data FileWritePosition = FileWritePosition
  {identifierPosition :: String
  ,totalXPosAmount:: Int 
  ,totalYPosAmount:: Int
  ,totalZPosAmount:: Int
  ,totalFPosAmount:: Int
  ,totalXPosCount :: Int
  ,totalYPosCount :: Int
  ,totalZPosCount :: Int
  ,totalFPosCount :: Int
  ,takerXPos :: Int
  ,takerYPos :: Int
  ,takerZPos :: Int  
  ,takerFPos :: Int
  ,makerXPos :: Int
  ,makerYPos :: Int
  ,makerZPos :: Int
  ,makerFPos :: Int
  ,buyVolumePos :: Int
  ,sellVolumePos :: Int
  ,overalVolumePos :: Int
  ,overalOpenInterestPos :: Int
  ,liquidationInfoPos :: MarginCall
  } deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig FileWritePosition

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
  InitPrice :: {initPrice :: Double} -> InitPrice
  deriving Generic
  deriving (FromJSON, ToJSON) via JSONConfig InitPrice

data Book = Book {
  book :: [(Double, Int)]
} deriving (Show, Generic, FromJSON, ToJSON)

-- | defining data typ for volume side
data VolumeSide
  = Buy
  | Sell
   deriving (Show, Eq, Ord, Enum, Bounded, Generic, FromJSON, ToJSON)

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
              UPP |
              DW  |
              DWW |
              CN  |
              RANDOM
              deriving  (Eq, Show, Enum, Bounded)

type SeqOrderBook = Seq (Double, Int)
type OrderBook = [(Double, Int)]
type VolumeList = [(Int, VolumeSide)]
type Generator = StdGen
type FullWall = [Int]
type StartingPoint = Double
type Totakefromwall = Int
type Volume = (Int, VolumeSide)


type InitBookStats = (StartingPoint , (Int,Int), Int , Int , Totakefromwall , Int , Int, SeqOrderBook,  SeqOrderBook , VolumeSide, Int, Double, Double, Double)

data BookStats = BookStats {
                     startingPoint :: StartingPoint
                   , maxMinLimit :: (Int,Int)
                   , asksTotal :: Int
                   , bidsTotal :: Int
                   , totakefromwall :: Totakefromwall
                   , lengthchangeBID :: Int
                   , lengthchangeASK :: Int
                   , listASK :: SeqOrderBook
                   , listBID :: SeqOrderBook
                   , vSide :: VolumeSide
                   , volumeAmount :: Int
                   , spread :: Double
                   , startingprice :: Double
                   , bidAskRatio :: Double
                   
                   } deriving (Show)
type SeqNewPositioning = Seq NewPositioning

type NewPositioning   =   (TakerTuple, MakerTuple)

type MarginCall       =   [(Int,String,String)] 

type FutureAcc        =   (FutureInfo, FutureInfo)

type Position         =   ([(Int, String)], [(Int, String)])

type FutureInfo       =   [(Double, Int, String)]

data Transaction = Transaction {
    future :: FutureInfo
  } deriving (Show, Generic, FromJSON, ToJSON)