{-# LANGUAGE DeriveGeneric #-}
module Types where

import           Data.Aeson
import           GHC.Generics

data FitnessInfo = FI { milesBiked     :: Int
                      , caloriesBurned :: Int
                      }
    deriving (Generic, Show)

data FitnessStats = FS { today,week,year :: FitnessInfo }
    deriving (Generic, Show)

data MediaInfo = MediaInfo { url, artPath, mediaName :: String -- or something else?
                           }
    deriving (Generic, Show)

data Stats = Stats { games,music :: [MediaInfo]
                   }
    deriving (Generic, Show)

instance FromJSON FitnessInfo
instance ToJSON FitnessInfo
instance FromJSON FitnessStats
instance ToJSON FitnessStats
instance FromJSON MediaInfo
instance ToJSON MediaInfo
instance FromJSON Stats
instance ToJSON Stats
