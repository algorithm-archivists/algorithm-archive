import Data.Complex
import Data.Array
import Data.Ratio
import qualified Data.Map as M

fft :: [Complex Double] -> [Complex Double]
fft x = let n = length x
            i = 0 :+ 1
            w = M.fromList [(k%n, exp ((-2)*pi*i*(fromIntegral k)/(fromIntegral n)) ) | k<-[0..n-1]]
            arr = fft' n w (listArray (0,n-1) x)
        in [arr!k | k<-[0..n-1]]
  where
  fft' 1 _ x = x
  fft' n w x = let n2 = div n 2
                   e = fft' n2 w (listArray (0, n2-1) [x!k | k<-[0,2..n-1]])
                   o = fft' n2 w (listArray (0, n2-1) [x!k | k<-[1,3..n-1]])
               in array (0, n-1) $ concat [[(k, e!k + o!k * w M.!(k%n)),
                                            (k + n2, e!k - o!k * w M.!(k%n))]
                                                               | k <- [0..n2-1]]

main = do
  print $ fft [0,1,2,3]
