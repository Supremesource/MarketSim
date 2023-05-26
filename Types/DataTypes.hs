module DataTypes where


-- | defining data typ for volume side
data VolumeSide
  = Buy
  | Sell
   deriving Eq

instance Show VolumeSide where
  show :: VolumeSide -> String
  show Buy  = "Buy"
  show Sell = "Sell"

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