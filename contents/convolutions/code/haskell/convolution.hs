import Data.Array.CArray
import Data.Complex
import Data.List (tails)
import Math.FFT (dft, idft)

convolution :: (Num a) => [a] -> [a] -> [a]
convolution x = map (sum . zipWith (*) (reverse x)) . spread
  where
    spread = init . tails . (replicate (length x - 1) 0 ++)

convolutionFFT :: [Complex Double] -> [Complex Double] -> [Complex Double]
convolutionFFT x y = elems $ idft $ liftArray2 (*) (fft x) (fft y)
  where
    fft a = dft $ listArray (1, length a) a

main :: IO ()
main = do
  let x = [1, 2, 1, 2, 1]
      y = [2, 1, 2, 1, 2]
  print $ convolution x y
  print $ convolutionFFT x y
