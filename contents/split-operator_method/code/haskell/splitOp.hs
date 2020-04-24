import Data.Array.CArray
import Data.Complex
import Data.List (intercalate, transpose)
import Math.FFT (dft, idft)

type Vector = CArray Int (Complex Double)

(.*), (.+) :: Vector -> Vector -> Vector
a .* b = liftArray2 (*) a b
a .+ b = liftArray2 (+) a b

normalize :: Double -> Vector -> Vector
normalize dx v =
  let factor = 1 / sqrt dx / norm2 v :+ 0
   in liftArray (factor *) v

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
      time
        | imTime param = dt param :+ 0
        | otherwise = 0 :+ dt param
      v = liftArray (\x -> 0.5 * (x - v0) ^ 2) (x param)
      rStep = liftArray (\x -> exp (-0.5 * time * x)) v
      kStep = liftArray (\k -> exp (-0.5 * time * k ^ 2)) (ks param)
      wfc = liftArray (\x -> exp (-(x - wfc0) ^ 2 / 2)) (x param)
   in Operators v rStep kStep (normalize (dx param) wfc)

evolve :: Parameters -> Operators -> [Operators]
evolve param op@(Operators _ rStep kStep _) = iterate splitop op
  where
    splitop op = op {wfc = wfc' op}
    wfc' = norm . (rStep .*) . idft . (kStep .*) . dft . (rStep .*) . wfc
    norm = if imTime param then normalize (dx param) else id

calculateEnergy :: Parameters -> Operators -> Double
calculateEnergy param ops = (* dx param) . sum . map realPart $ elems totalE
  where
    totalE = potentialE .+ kineticE
    potentialE = wfcConj .* v ops .* wfc ops
    kineticOp = liftArray ((/ 2) . (^ 2)) (ks param)
    kineticE = wfcConj .* idft (kineticOp .* dft (wfc ops))
    wfcConj = liftArray conjugate $ wfc ops

-- Use gnuplot to make an animated  GIF using ../gnuplot/plot_output.plt
-- $ gnuplot -e "folder='../haskell'" plot_output.plt
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
