{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module QuoteData where
import Data.Time (Day, parseTimeM, defaultTimeLocale)
import Data.ByteString.Char8 (unpack)
import GHC.Generics (Generic)
import Data.Csv (FromNamedRecord, FromField (..))

data QuoteData = QuoteData {
    day :: Day
  , volume :: Int
  , open :: Double
  , close :: Double
  , high :: Double
  , low :: Double
  }
