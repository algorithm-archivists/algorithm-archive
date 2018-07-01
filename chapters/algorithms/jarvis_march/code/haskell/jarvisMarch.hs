import Data.List (sort, maximumBy)
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

jarvisMarch :: [Point] -> [Point]
jarvisMarch [] = []
jarvisMarch pts = p0 : wrap (x, y-1) p0
  where p0@(x, y)= minimum pts
        wrap p1 p2
          | pm == p0  = []
          | otherwise = pm : wrap p2 pm
          where pm = maximumBy (compare `on` angle p1 p2) pts

main = do
  let pts = filter (\(x,y) -> x^2+y^2<=5^2) [(x,y)|x<-[-5..5], y<-[-5..5]]
  print $ jarvisMarch pts
