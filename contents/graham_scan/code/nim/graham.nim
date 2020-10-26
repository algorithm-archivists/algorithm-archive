from algorithm import sort, sorted
from math import arctan2
from sequtils import deduplicate, map, toSeq
import sugar

type Point[T: SomeNumber] = tuple[x, y: T]

func tup_to_point[T](t: (T, T)): Point[T] =
  (x: t[0], y: t[1])

func cross_product[T](p1, p2, p3: Point[T]): T =
  ## Form the cross product of three points. If the result is
  ## - zero, the points are collinear.
  ## - positive, the points form a counter-clockwise "left" turn.
  ## - negative, the points form a clockwise "right" turn.
  (p3.y - p1.y) * (p2.x - p1.x) - (p2.y - p1.y) * (p3.x - p1.x)

func polar_angle(reference, point: Point): float =
  ## Find the polar angle of a point relative to a reference point
  arctan2(float(point.y - reference.y), float(point.x - reference.x))

func flipped_point_cmp(pa, pb: Point): int =
  ## Compare points first by their y-coordinate, then x-coordinate.
  if (pa.y, pa.x) < (pb.y, pb.x): -1
  elif pa == pb: 0
  else: 1

func graham_scan(gift: seq[Point]): seq[Point] =
  assert(gift.len >= 3)
  var points = sorted(deduplicate(gift), flipped_point_cmp)
  let pivot = points[0]
  # Mimic sorting a sliced sequence without copying
  sort(toOpenArray(points, 1, high(points)),
       func (pa, pb: Point): int =
         if polar_angle(pivot, pa) < polar_angle(pivot, pb): -1
         else: 1)
  var
    # Hull pointer
    m = 1
    # Needed because the iteration variable from a slice is immutable
    en = toSeq(low(points) + 2..high(points))
  for i in mitems(en):
    while cross_product(points[m - 1], points[m], points[i]) <= 0:
      if m > 1:
        m -= 1
      # All points are collinear
      elif i == points.len:
        break
      else:
        i += 1
    # Counter-clockwise point found, update hull and swap
    m += 1
    swap(points[i], points[m])
  points[0..m]

when isMainModule:
  let
    test_gift = @[(-5, 2), (5, 7), (-6, -12), (-14, -14), (9, 9),
                  (-1, -1), (-10, 11), (-6, 15), (-6, -8), (15, -9),
                  (7, -7), (-2, -9), (6, -5), (0, 14), (2, 8)].map(t => tup_to_point(t))
    hull = graham_scan(test_gift)
  echo "Initial gift:"
  echo test_gift
  echo "Final hull:"
  echo hull
