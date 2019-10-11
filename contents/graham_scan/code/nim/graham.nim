from algorithm import sorted
from math import arctan2
from sequtils import deduplicate, map
import sugar

type Point[T: SomeNumber] = tuple[x, y: T]

proc tup_to_point[T](t: (T, T)): Point[T] =
  (x: t[0], y: t[1])

proc is_counter_clockwise(p1, p2, p3: Point): bool =
  (p3.y - p1.y) * (p2.x - p1.x) >= (p2.y - p1.y) * (p3.x - p1.x)

proc polar_angle(reference, point: Point): float =
  arctan2(float(point.y - reference.y), float(point.x - reference.x))

proc flipped_point_cmp(pa, pb: Point): int =
  if (pa.y, pa.x) < (pb.y, pb.x): -1
  elif pa == pb: 0
  else: 1

proc graham_scan(gift: seq[Point]): seq[Point] =
  # TODO make sure input has length >= 3
  let
    gift_without_duplicates = sorted(deduplicate(gift), flipped_point_cmp)
    start = gift_without_duplicates[0]
    candidates = sorted(gift_without_duplicates[1..^1],
                        proc (pa, pb: Point): int =
                          if polar_angle(start, pa) < polar_angle(start, pb): -1
                          else: 1)
  # TODO take the approach outlined in the text where we perform rotations on
  # the candidates, rather than add to a new sequence
  var hull = @[start, candidates[0], candidates[1]]
  for candidate in candidates:
    while not is_counter_clockwise(hull[^2], hull[^1], candidate):
      discard pop(hull)
    add(hull, candidate)
  hull

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
