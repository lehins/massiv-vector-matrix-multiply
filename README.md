# massiv-vector-matrix-multiply


This repo contains two specialized functions that implement very efficient parallelized
multiplication of a vector by a matrix both of which contain 32bit floating point numbers.

## Usage

Add `massiv-vector-matrix-multiply` to `package.yaml` or the cabal file and then add this
to `stack.yaml`:

```yaml
extra-deps:
- github: lehins/massiv-vector-matrix-multiply
  commit: 8afcd8feef8279e95ccc5e9a82b6f9db825349b2
  subdirs:
    - massiv-vector-matrix-multiply
```

The use it in a Haskell project with [`massiv`](https://github.com/lehins/massiv) library:

```haskell
import Data.Massiv.Array as A
import Data.Massiv.Array.Numeric.Optimized (multiplyVectorByMatrixP)

main :: IO ()
main = do
  let vec = fromList Seq [1, 2, 3]
  mat <- fromListsM Seq [[20, 30], [40, 50], [60, 70]]
  print $ multiplyVectorByMatrixP vec mat
```

Running fro ghci will yield:

```haskell
λ> :main
Array P Seq (Sz1 2)
  [ 280.0, 340.0 ]
```

## Performance

```
benchmarking Vector x Matrix multiplication/><.
time                 170.7 ms   (166.2 ms .. 176.1 ms)
                     0.999 R²   (0.997 R² .. 1.000 R²)
mean                 169.9 ms   (168.2 ms .. 172.1 ms)
std dev              2.827 ms   (2.239 ms .. 3.413 ms)
variance introduced by outliers: 12% (moderately inflated)

benchmarking Vector x Matrix multiplication/><. (Par)
time                 48.59 ms   (47.76 ms .. 49.68 ms)
                     0.998 R²   (0.995 R² .. 1.000 R²)
mean                 49.64 ms   (48.73 ms .. 51.84 ms)
std dev              2.523 ms   (727.7 μs .. 4.416 ms)
variance introduced by outliers: 14% (moderately inflated)

benchmarking Vector x Matrix multiplication/multiplyVectorByMatrixS
time                 58.69 ms   (58.34 ms .. 59.00 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 59.02 ms   (58.84 ms .. 59.25 ms)
std dev              385.6 μs   (254.8 μs .. 552.7 μs)

benchmarking Vector x Matrix multiplication/multiplyVectorByMatrixS (Par)
time                 41.80 ms   (41.14 ms .. 42.54 ms)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 42.41 ms   (42.07 ms .. 42.83 ms)
std dev              775.6 μs   (536.7 μs .. 1.080 ms)

benchmarking Vector x Matrix multiplication/multiplyVectorByMatrixP
time                 56.40 ms   (55.62 ms .. 57.01 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 56.90 ms   (56.62 ms .. 57.21 ms)
std dev              546.5 μs   (412.0 μs .. 754.0 μs)

benchmarking Vector x Matrix multiplication/multiplyVectorByMatrixP (Par)
time                 42.16 ms   (41.59 ms .. 42.76 ms)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 42.30 ms   (41.87 ms .. 43.04 ms)
std dev              1.231 ms   (537.2 μs .. 1.993 ms)
```
