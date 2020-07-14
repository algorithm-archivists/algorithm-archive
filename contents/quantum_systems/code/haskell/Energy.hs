import Data.Array.CArray
import Data.Complex
import Math.FFT (dft, idft) -- Binding to fftw

type Vector = CArray Int (Complex Double)

calculateEnergy :: Double -> Vector -> Vector -> Vector -> Double
calculateEnergy dx kin pot wfc = (* dx) . sum . map realPart $ elems total
  where
    total = liftArray2 (+) kineticE potentialE
    potentialE = wfcConj .* pot .* wfc
    kineticE = wfcConj .* idft (kin .* dft wfc)
    wfcConj = liftArray conjugate wfc
    a .* b = liftArray2 (*) a b
