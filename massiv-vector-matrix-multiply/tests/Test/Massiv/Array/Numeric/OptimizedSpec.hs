{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Test.Massiv.Array.Numeric.OptimizedSpec (spec) where

import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Optimized
import Test.Massiv.Core

prop_VectorMatrixMultiply ::
     forall r e.
     ( Numeric r e
     , OuterSlice r Ix2 e
     , Mutable r Ix2 e
     , Mutable r Ix1 e
     , Show e
     , RealFloat e
     )
  => (Vector r e -> Matrix r e -> Vector r e)
  -> Fun Int e
  -> Matrix r e
  -> Property
prop_VectorMatrixMultiply multFun f mat =
  (m /= 0) ==>
  expectProp (epsilonFoldableExpect 1e-8 (delay (multFun v mat)) (v ><! mat))
  where
    Sz2 m _ = size mat
    v = makeArray Seq (Sz m) (applyFun f)

spec :: Spec
spec = do
  describe "VectorMatrixMultiply" $ do
    prop "multiplyVectorByMatrixS" $ prop_VectorMatrixMultiply multiplyVectorByMatrixS
    prop "multiplyVectorByMatrixP" $ prop_VectorMatrixMultiply multiplyVectorByMatrixP
