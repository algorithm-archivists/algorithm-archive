-- submitted by Jie
type Time = Double

type Position = Double

type Speed = Double

type Acceleration = Double

type Particle = (Position, Speed, Acceleration, Time)

type Model = Particle -> Acceleration

type Method = Model -> Time -> Particle -> Particle -> Particle

verlet :: Method
verlet acc dt (xOld, _, _, _) (x, _, a, t) = (x', v', a', t + dt)
  where
    x' = 2 * x - xOld + a * dt ^ 2
    v' = 0
    a' = acc (x', v', a, t + dt)

stormerVerlet :: Method
stormerVerlet acc dt (xOld, _, _, _) (x, _, a, t) = (x', v', a', t + dt)
  where
    x' = 2 * x - xOld + a * dt ^ 2
    v' = (x' - x) / dt
    a' = acc (x', v', a, t + dt)

velocityVerlet :: Method
velocityVerlet acc dt (xOld, _, aOld, _) (x, v, a, t) = (x', v', a', t + dt)
  where
    x' = 2 * x - xOld + a * dt ^ 2
    v' = v + 0.5 * (aOld + a) * dt
    a' = acc (x', v', a, t + dt)

trajectory :: Method -> Model -> Time -> Particle -> [Particle]
trajectory method acc dt p0@(x, v, a, t0) = traj
  where
    traj = p0 : p1 : zipWith (method acc dt) traj (tail traj)
    p1 = (x', v', acc (x', v', a, t0 + dt), t0 + dt)
    x' = x + v * dt + 0.5 * a * dt ^ 2
    v' = v + a * dt

main :: IO ()
main = do
  let p0 = (5, 0, -10, 0)
      dt = 0.001
      freefall _ = -10
      aboveGround (x, _, _, _) = x > 0
      integrate m = last $ takeWhile aboveGround $ trajectory m freefall dt p0
  print $ integrate verlet
  print $ integrate stormerVerlet
  print $ integrate velocityVerlet
