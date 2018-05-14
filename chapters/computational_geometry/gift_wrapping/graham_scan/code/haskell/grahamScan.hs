import Data.List (sortOn, minimumBy)
import Data.Function (on)

type Point = (Double, Double)

angle :: Point -> Point -> Point -> Double
angle a@(xa, ya) b@(xb, yb) c@(xc, yc)
  | a==b || c==b = 0
  | theta<0      = theta+2*pi
  | otherwise    = theta
  where thetaA = atan2 (ya-yb) (xa-xb)
        thetaC = atan2 (yc-yb) (xc-xb)
        theta = thetaC - thetaA

ccw :: Point -> Point -> Point -> Double
ccw (xa, ya) (xb, yb) (xc, yc) = (xb - xa) * (yc - ya) - (yb - ya) * (xc - xa)

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan pts = wrap sortedPts [p0]
  where p0@(x, y)= minimumBy (compare `on` snd) pts
        sortedPts = init $ sortOn (negate . angle (x, y-1)  p0) pts
        wrap [] p = p
        wrap (s:ss) [p] = wrap ss [s, p]
        wrap (s:ss) (p1:p2:ps)
          | ccw s p1 p2 < 0 = wrap (s:ss) (p2:ps)
          | otherwise       = wrap ss (s:p1:p2:ps)

main = do
  let pts = [(x,y) | x<-[-5..5], y<-[-5..5], x^2+y^2<=5^2]
  print $ grahamScan pts
