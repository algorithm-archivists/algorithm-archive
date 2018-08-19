{-# LANGUAGE FlexibleContexts #-}

import Data.Array.CArray
import Data.Complex
import Data.List (intercalate, transpose)
import Math.FFT (dft, idft) -- Binding to fftw

type Vector = CArray Int (Complex Double)

data Parameters = Parameters
  { xmax :: Double
  , res :: Int
  , dt :: Double
  , timesteps :: Int
  , dx :: Double
  , x :: Vector
  , dk :: Double
  , ks :: Vector
  , imTime :: Bool
  }

defaultParameters :: Parameters
defaultParameters = makeParameters 10 512 0.01 1000 True

makeParameters :: Double -> Int -> Double -> Int -> Bool -> Parameters
makeParameters xmax res dt timesteps imTime =
  let fi = fromIntegral
      rng = (0, res - 1)
      ks = [0 .. div res 2 - 1] ++ [-div res 2 .. -1]
   in Parameters
        xmax
        res
        dt
        timesteps
        (2 * xmax / fi res)
        (listArray rng $
         map (\n -> xmax * (-1 + 2 * fi n / fi res) :+ 0) [1 .. res])
        (pi / xmax)
        (listArray rng $ map ((:+ 0) . (pi / xmax *) . fi) ks)
        imTime

data Operators = Operators
  { v :: Vector
  , rStep :: Vector
  , kStep :: Vector
  , wfc :: Vector
  }

makeOperators :: Parameters -> Complex Double -> Complex Double -> Operators
makeOperators param v0 wfc0 =
  let rng = (0, res param - 1)
      t =
        (dt param :+ 0) *
        if imTime param
          then 1 :+ 0
          else 0 :+ 1
      v = liftArray (\x -> 0.5 * (x - v0) ^ 2) (x param)
      rStep = liftArray (\x -> exp (-0.5 * t * x)) v
      kStep = liftArray (\k -> exp (-0.5 * t * k ^ 2)) (ks param)
      wfc' = liftArray (\x -> exp (-(x - wfc0) ^ 2 / 2)) (x param)
      factor = 1 / sqrt (dx param) / norm2 wfc' :+ 0
      wfc = liftArray (factor *) wfc'
   in Operators v rStep kStep wfc

evolve :: Parameters -> Operators -> [Operators]
evolve param op@(Operators _ rStep kStep _) = iterate splitop op
  where
    splitop op = op {wfc = wfc' op}
    wfc' = norm . (rStep .*) . idft . (kStep .*) . dft . (rStep .*) . wfc
    a .* b = liftArray2 (*) a b
    norm x
      | imTime param =
        let f = 1 / sqrt (dx param) / norm2 x :+ 0
         in liftArray (f *) x
      | otherwise = x

calculateEnergy :: Parameters -> Operators -> Double
calculateEnergy param ops = (* dx param) . sum . map realPart $ elems totalE
  where
    totalE = liftArray2 (+) potentialE kineticE
    potentialE = wfcConj .* v ops .* wfc ops
    kineticOp = liftArray ((/ 2) . (^ 2)) (ks param)
    kineticE = wfcConj .* idft (kineticOp .* dft (wfc ops))
    wfcConj = liftArray conjugate $ wfc ops
    a .* b = liftArray2 (*) a b

-- Use gnuplot to make an animated  GIF using ../../plot_output.plt
-- $ gnuplot -e "folder='/code/haskell'" plot_output.plt
printEvolution :: Parameters -> [Operators] -> IO ()
printEvolution param =
  mapM_ (export . (format <$>)) . zip [0 ..] . take 100 . skip
  where
    skip (x:xs) = x : skip (drop (div (timesteps param) 100 - 1) xs)
    format (Operators v _ _ wfc) =
      let density = liftArray ((^ 2) . abs) wfc
          values = map (map (show . realPart) . elems) [x param, density, v]
       in intercalate "\n" $ map (intercalate "\t") $ transpose values
    export (i, f) = writeFile ("output" ++ pad (show i) ++ ".dat") f
    pad n = replicate (5 - length n) '0' ++ n

main :: IO ()
main = do
  let p = defaultParameters
      o = makeOperators p 0 4
      evol = evolve p o
  print $ calculateEnergy p (evol !! timesteps p)
  printEvolution p evol
