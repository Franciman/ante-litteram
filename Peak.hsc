module Peak where

import Foreign.Storable
import Foreign.C.Types

import Data.Int

#include <media_processor/peak.hpp>

data Peak = Peak
          { minVal :: Int16
          , maxVal :: Int16
          }
          deriving(Show)


instance Storable Peak where
    sizeOf _ = (#size struct Peak)
    alignment _ = alignment (undefined :: CShort)
    peek ptr = do
        minPeak <- (#peek struct Peak, Min) ptr :: IO CShort
        maxPeak <- (#peek struct Peak, Max) ptr :: IO CShort
        return $ Peak (fromIntegral minPeak) (fromIntegral maxPeak)

    poke ptr (Peak minPeak maxPeak) = do
        (#poke struct Peak, Min) ptr minPeak
        (#poke struct Peak, Max) ptr maxPeak
