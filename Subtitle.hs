module Subtitle where

import Data.Monoid

data TimeInterval = TimeInterval
                  { low  :: Int
                  , high :: Int
                  }
                  deriving(Eq, Show)

instance Ord TimeInterval where
    compare (TimeInterval s1 e1) (TimeInterval s2 e2) = compare s1 s2 <> compare e1 e2

data Subtitle = Subtitle
              { timeInterval :: TimeInterval
              , dialog       :: String
              }
              deriving(Eq, Show)

start :: Subtitle -> Int
start = low . timeInterval

end :: Subtitle -> Int
end = high . timeInterval

overlap :: TimeInterval -> TimeInterval -> Bool
overlap (TimeInterval l1 h1) (TimeInterval l2 h2) = l1 <= h2 && l2 <= h1


instance Ord Subtitle where
    compare (Subtitle i1 d1) (Subtitle i2 d2) = compare i1 i2 <> compare d1 d2

