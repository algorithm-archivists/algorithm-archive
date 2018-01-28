-- submitted by Jie
type Position     = [Double]
type Speed        = [Double]
type Time         = Double
type Particle     = (Position, Speed, Acceleration, Time)
type Acceleration = [Double]

verletStep :: (Particle -> Acceleration)
              -> Time
              -> Particle
              -> Particle
              -> Particle
verletStep acc dt (xOld, _, aOld, _) (x, v, a, t) = (x', v', a', t+dt)
  where
  x' = zipWith3 (\xOld x a -> 2*x - xOld + a*dt^2 ) xOld x a
  v' = zipWith3 (\v a aOld -> v + 0.5*(aOld + a)*dt) v a aOld
  a' = acc (x', v', [], t+dt)

trajectory :: (Particle -> Acceleration)
              -> Time
              -> Particle
              -> [Particle]
trajectory acc dt p0@(x, v, a, t0) = t
  where
  t  = p0 : p1 : zipWith (verletStep acc dt) t (tail t)
  p1 = (x', v', acc (x', v', [], t0+dt), t0+dt)
  x' = zipWith3 (\x v a -> x + v*dt + 0.5*a*dt^2 ) x v a
  v' = zipWith (\v a -> v + a*dt) v a

freeFall :: Particle
freeFall = last $ takeWhile (\([x],_,_,_) -> x > 0) $ trajectory acc dt p0
  where
  p0    = ([5], [0], [-10], 0)
  dt    = 0.001
  acc _ = [-10]

