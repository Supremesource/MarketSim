{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingVia, DataKinds #-}
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
module DataTypes 
{- 
-- ! DESCRIPTION
Data types that are `usually` used in more than one module are defined here
-}
where

-- | external libraries
import System.Random ( StdGen )
import Deriving.Aeson
import Data.Sequence (Seq)

-- / DATA
-- ? JSON Serialization
type JSONConfig a = 
 CustomJSON '[OmitNothingFields, FieldLabelModifier 
 '[StripPrefix "DATA", CamelToSnake]] a

data TransactionFut = TransactionFut {
    future :: ClosePositionData
  } deriving (Show, Eq, Generic)
  deriving (FromJSON, ToJSON)
  via JSONConfig TransactionFut

data VolumeStage =
                Undefined |
                   LowVol |
                MediumVol |
                HighVol   |
                SpikeVol  |
                DeadVol   deriving (Show, Eq)

data FileWritesLog = FileWritesLog
   {  identifierLOG           :: String
    , startingPointLOG        :: Double 
    , takeamountLOG           :: Int
    -- printf formated
    , maxUpMoveLOG            :: String
    , minUpMoveLOG            :: String
    , maxDownMoveLOG          :: String
    , minDownMoveLOG          :: String
    , minimum'LOG             :: Int
    , minimumActualLOG        :: Int
    , maximum'LOG             :: Int
    , maximumActualLOG        :: Int
    , takeamountBIDLOG        :: Int
    , takeamountASKLOG        :: Int
    , asksTotalLOG            :: Int
    , bidsTotalLOG            :: Int
    , orderwalllikelyhoodLOG  :: Int
    , totakefromwallLOG       :: Int 
    , wallminimum'LOG         :: Int
    , wallmaximum'LOG         :: Int
    , wallAmplifierLOG        :: Int
    , maxDecimalLOG           :: Int
    , lengthchangeBIDLOG      :: Int
    , lengthchangeASKLOG      :: Int
    , listASKLOG              :: [(Double, Int)]
    , listBIDLOG              :: [(Double, Int)]
    , vSideLOG                :: String 
    , volumeAmountLOG         :: Int
    -- formated with printf
    , spreadLOG               :: String
    , startingPriceLOG        :: Double
    } deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig FileWritesLog

data FileWriteBook = FileWriteBook
  { identifierBook    :: String 
  , priceBook :: Double
  -- formated with printf
  , bidAskRatioBook   :: String 
  , bidsTotalBook     :: Int
  , asksTotalBook     :: Int
  , maxMinLmtBook     :: (Int,Int)
  , vSideBook         :: VolumeSide
  , volumeAmountBook  :: Int
  -- fotmate with printf
  , spreadBook        :: String 
  } deriving (Generic, Show)
  deriving (FromJSON, ToJSON)
  via JSONConfig FileWriteBook

data FileWritePosition    = FileWritePosition
  {identifierPosition     :: String
  ,totalXPosAmount        :: Int 
  ,totalYPosAmount        :: Int
  ,totalZPosAmount        :: Int
  ,totalFPosAmount        :: Int
  ,totalXPosCount         :: Int
  ,totalYPosCount         :: Int
  ,totalZPosCount         :: Int
  ,totalFPosCount         :: Int
  ,takerXPos              :: Int
  ,takerYPos              :: Int
  ,takerZPos              :: Int  
  ,takerFPos              :: Int
  ,makerXPos              :: Int
  ,makerYPos              :: Int
  ,makerZPos              :: Int
  ,makerFPos              :: Int
  ,buyVolumePos           :: Int
  ,sellVolumePos          :: Int
  ,overalVolumePos        :: Int
  ,overalOpenInterestPos  :: Int
  ,activatedExitPos       :: Seq (Int,String,String)
  ,isVolForcedPos         :: (Bool,String)
  ,leverageAmtTPos         :: Int
  ,leverageAmtMPos         :: Int
  } deriving (Show, Generic)
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
  }deriving (Show, Eq) 
  deriving Generic
  deriving (FromJSON, ToJSON)
  via JSONConfig PositionData

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
  , forceCall   :: Seq (Int,String,String)
  , isVolForced :: (Bool,String)
  , leverageAmtT :: Int
  , leverageAmtM :: Int

  } deriving (Show,Eq)

data Options =
               UP |
              UPP |
              DW  |
              DWW |
              CN  |
              RANDOM
              deriving  (Eq, Show, Enum, Bounded)

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
   
-- / TYPES
-- | positining structure
type MakerPositions     =   [(Int, String)]
type TakerPositions     =   [(Int, String)]
type MarginCall         =   [(Int,String,String)] 
type FutureAcc          =   (ClosePositionData, ClosePositionData)
type Position           =   ([(Int, String)], [(Int, String)])
                      -- liquidation price, amount, side, entry price, leverage, is force Stop 
type ClosePositionData  =   [(Double, Int, String, Double, Double, Bool)] 
type NewPositioning     =   (TakerPositions, MakerPositions)
type SeqNewPositioning  =   Seq NewPositioning
-- | order-book
type SeqOrderBook       =   Seq (Double, Int)
type OrderBook          =   [(Double, Int)]
type InitBookStats      =   (StartingPoint,(Int,Int),Int,Int,Totakefromwall,Int,Int,
                  SeqOrderBook,SeqOrderBook,VolumeSide,Int,Double,Double,Double)
type FullWall           = [Int]
type StartingPoint      = Double
type Totakefromwall     = Int
-- | volumes
type VolumeList         = [(Int, VolumeSide)]
type Volume             = (Int, VolumeSide)
-- | additional
type Generator          = StdGen












