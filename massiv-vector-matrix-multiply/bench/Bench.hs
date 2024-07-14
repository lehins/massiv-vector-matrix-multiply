{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Criterion.Main
import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Optimized
import System.Random

randomVectorMatrix ::
     Manifest r Float
  => Sz2
  -> (Vector r Float, Matrix r Float)
randomVectorMatrix sz@(Sz2 m _) =
  case randomArrayS gen sz (randomR (0, 1)) of
    (gen', mat) ->
      case randomArrayS gen' (Sz m) (randomR (0, 1)) of
        (_, vec) -> (vec, mat)
  where
    gen = mkStdGen 2020

main :: IO ()
main = do
  let Sz2 rows cols = Sz2 19000 10000 -- matrix size
  let (vec, mat :: Matrix S Float) = randomVectorMatrix (Sz2 rows cols)
  defaultMain
    [ bgroup
        "Vector x Matrix multiplication"
        [ env (pure (vec, mat)) $ \ ~(v, m) ->
            bench "><." $
            nfIO ((computeIO =<< (v ><. m)) :: IO (Vector S Float))
        , env (pure (vec, mat)) $ \ ~(v, m) ->
            bench "><. (Par)" $
            nfIO ((computeIO =<< (v ><. setComp Par m)) :: IO (Vector S Float))
        , env
            (pure (vec, mat))
            (bench "multiplyVectorByMatrixS" .
             nf (uncurry multiplyVectorByMatrixS))
        , env
            (pure (vec, setComp Par mat))
            (bench "multiplyVectorByMatrixS (Par)" .
             nf (uncurry multiplyVectorByMatrixS))
        , env
            (pure (convert vec, convert mat))
            (bench "multiplyVectorByMatrixP" .
             nf (uncurry multiplyVectorByMatrixP))
        , env
            (pure (convert vec, convert $ setComp Par mat))
            (bench "multiplyVectorByMatrixP (Par)" .
             nf (uncurry multiplyVectorByMatrixP))
        ]
    ]
