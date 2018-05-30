import Data.List (sortOn, minimumBy)
import Data.Function (on)

type Point = (Double, Double)

ccw :: Point -> Point -> Point -> Double
ccw (xa, ya) (xb, yb) (xc, yc) = (xb - xa) * (yc - ya) - (yb - ya) * (xc - xa)

grahamScan :: [Point] -> [Point]
grahamScan [] = []
grahamScan pts = wrap sortedPts [p0]
  where p0@(x, y)= minimumBy (compare `on` snd) pts
        sortedPts = sortOn (\(px, py) -> atan2 (py-y) (px-x) ) $ filter (/=p0) pts
        wrap [] ps = ps
        wrap (s:ss) [p] = wrap ss [s, p]
        wrap (s:ss) (p1:p2:ps)
          | ccw s p1 p2 > 0 = wrap (s:ss) (p2:ps)
          | otherwise       = wrap ss (s:p1:p2:ps)

main = do
  -- We build the set of points of integer coordinates within a circle of radius 5
  let pts = [(x,y) | x<-[-5..5], y<-[-5..5], x^2+y^2<=5^2]
  -- And extract the convex hull
  print $ grahamScan pts
