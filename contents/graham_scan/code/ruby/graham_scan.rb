def graham_scan(points)
  # First, sort the points so the one with the lowest y-coordinate comes first (the pivot)
  points = points.sort_by { |point| point[:y] }
  pivot = points[0]

  # Then sort all remaining points based on the angle between the pivot and itself
  hull = points.slice(1..-1).sort { |a, b| polar_angle(a, pivot) - polar_angle(b, pivot) }

  # The pivot is always on the hull
  hull.unshift(pivot)

  n = hull.length
  m = 1
  (2...n).each do |i|
    while ccw(hull[m - 1], hull[m], hull[i]) <= 0
      if m > 1
        m -= 1
      elsif m == i
        break
      else
        i += 1
      end
    end

    m += 1
    hull[i], hull[m] = [hull[m], hull[i]]
  end

  hull.slice(0...m + 1)
end

def polar_angle(a, b)
  Math.atan2(a[:y] - b[:y], a[:x] - b[:x])
end

def ccw(a, b, c)
  (b[:x] - a[:x]) * (c[:y] - a[:y]) - (c[:x] - a[:x]) * (b[:y] - a[:y])
end

points = [
  { x: 1, y: 3 },
  { x: 2, y: 4 },
  { x: 4, y: 0 },
  { x: 1, y: 0 },
  { x: 0, y: 2 },
  { x: 2, y: 2 },
  { x: 3, y: 4 },
  { x: 3, y: 1 }
]

convex_hull = graham_scan(points)
convex_hull.each { |p| puts("(#{p[:x]}, #{p[:y]})") }
