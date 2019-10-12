from algorithm import sort, sorted
from math import arctan2
from sequtils import deduplicate, map, toSeq
import sugar

type Point[T: SomeNumber] = tuple[x, y: T]

proc tup_to_point[T](t: (T, T)): Point[T] =
  (x: t[0], y: t[1])

proc is_counter_clockwise(p1, p2, p3: Point): bool =
  (p3.y - p1.y) * (p2.x - p1.x) < (p2.y - p1.y) * (p3.x - p1.x)

proc polar_angle(reference, point: Point): float =
  arctan2(float(point.y - reference.y), float(point.x - reference.x))

proc flipped_point_cmp(pa, pb: Point): int =
  if (pa.y, pa.x) < (pb.y, pb.x): -1
  elif pa == pb: 0
  else: 1

proc graham_scan(gift: seq[Point]): seq[Point] =
  assert(gift.len >= 3)
  var points = sorted(deduplicate(gift), flipped_point_cmp)
  let pivot = points[0]
  sort(toOpenArray(points, 1, high(points)),
       proc (pa, pb: Point): int =
         if polar_angle(pivot, pa) < polar_angle(pivot, pb): -1
         else: 1)
  var
    m = 1
    en = toSeq(low(points) + 2..high(points))
  for i in mitems(en):
    while is_counter_clockwise(points[m - 1], points[m], points[i]):
      if m > 1:
        m -= 1
      elif i == points.len:
        break
      else:
        i += 1
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
