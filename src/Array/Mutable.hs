module Array.Mutable where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as MV

newtype Array s a = Array (MV.MVector s a)

new :: PrimMonad m => Int -> m (Array (PrimState m) a)
new n = Array <$> MV.new n

replicate :: PrimMonad m => Int -> a -> m (Array (PrimState m) a)
replicate n val = Array <$> MV.replicate n val

size :: Array s a -> Int
size (Array vec) = MV.length vec

read :: PrimMonad m => Int -> Array (PrimState m) a -> m a
read idx (Array vec) = MV.read vec idx


write :: PrimMonad m => Int -> a -> Array (PrimState m) a -> m ()
write idx val (Array vec) = MV.write vec idx val


swap :: PrimMonad m => Int -> Int -> Array (PrimState m) a -> m ()
swap idxA idxB (Array vec) = do
    valA <- MV.read vec idxA
    valB <- MV.read vec idxB

    MV.write vec idxA valB
    MV.write vec idxB valA