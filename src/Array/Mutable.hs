module Array.Mutable where

import Control.Monad.ST (ST)
import qualified Data.Vector.Mutable as MV

newtype Array s a = Array (MV.MVector s a)
