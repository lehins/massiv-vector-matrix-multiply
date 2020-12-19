{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedFFITypes #-}
module Data.Massiv.Array.Numeric.Optimized where

import Control.Scheduler
import Data.Massiv.Array as A
import Data.Massiv.Array.Unsafe as A
import Data.Primitive.ByteArray
import Foreign.Ptr
import System.IO.Unsafe

multiplyVectorByMatrixS :: Vector S Float -> Matrix S Float -> Vector S Float
multiplyVectorByMatrixS vec mat
  | matRows /= vecLen = throw $ SizeMismatchException (Sz2 1 vecLen) sz
  | matRows == 0 || matCols == 0 = setComp comp empty
  | otherwise = unsafePerformIO $ do
    mutVec <- unsafeNew (Sz matCols)
    unsafeWithPtr vec $ \vecPtr ->
      unsafeWithPtr mat $ \matPtr ->
        withPtr mutVec $ \ ptrRes ->
          withScheduler_ comp $ \scheduler ->
            splitLinearlyM_ scheduler matCols $
              mult_vec_mat vecPtr matPtr matRows matCols ptrRes
    A.unsafeFreeze comp mutVec
  where
    comp = getComp mat <> getComp vec
    sz@(Sz2 matRows matCols) = size mat
    Sz1 vecLen = size vec
{-# INLINE multiplyVectorByMatrixS #-}


foreign import ccall unsafe "mult_vec_mat.c mult_vec_mat"
  mult_vec_mat ::
       Ptr Float -- ^ Pointer to a vector
    -> Ptr Float -- ^ Pointer to a matrix
    -> Int -- ^ Number of rows
    -> Int -- ^ Number of columns
    -> Ptr Float -- ^ Pointer to the resulting vector
    -> Int -- ^ Offset at which iterating resulting vector should start at
    -> Int -- ^ Offset before which iteration of resulting vector should stop
    -> IO ()


multiplyVectorByMatrixP :: Vector P Float -> Matrix P Float -> Vector P Float
multiplyVectorByMatrixP vec mat
  | matRows /= vecLen = throw $ SizeMismatchException (Sz2 1 vecLen) sz
  | matRows == 0 || matCols == 0 = setComp comp empty
  | otherwise = unsafePerformIO $ do
    mutVec <- unsafeNew (Sz matCols)
    (True, MutableByteArray vecRes#) <- toMutableByteArray mutVec
    case toByteArray vec of
      ByteArray vec# ->
        case toByteArray mat of
          ByteArray mat# ->
            withScheduler_ comp $ \scheduler ->
              splitLinearlyM_ scheduler matCols $
                mult_vec_mat_array vec# mat# matRows matCols vecRes#
    A.unsafeFreeze comp mutVec
  where
    comp = getComp mat <> getComp vec
    sz@(Sz2 matRows matCols) = size mat
    Sz1 vecLen = size vec
{-# INLINE multiplyVectorByMatrixP #-}



foreign import ccall unsafe "mult_vec_mat.c mult_vec_mat"
  mult_vec_mat_array ::
       ByteArray# -- ^ Vector with floating point values
    -> ByteArray# -- ^ Matrix with floating point values
    -> Int -- ^ Number of rows
    -> Int -- ^ Number of columns
    -> MutableByteArray# RealWorld -- ^ Resulting vector with floating point values
    -> Int -- ^ Offset at which iterating resulting vector should start at
    -> Int -- ^ Offset before which iteration of resulting vector should stop
    -> IO ()
