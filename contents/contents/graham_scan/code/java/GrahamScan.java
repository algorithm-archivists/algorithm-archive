import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Collectors;

public class GrahamScan {

    static class Point {
        public double x;
        public double y;

        public Point(double x, double y) {
            this.x = x;
            this.y = y;
        }

        @Override
        public boolean equals(Object o) {
            if (o == null) return false;
            if (o == this) return true;
            if (!(o instanceof Point)) return false;
            Point p = (Point)o;
            return p.x == this.x && p.y == this.y;
        }
    }

    static double ccw(Point a, Point b, Point c) {
        return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
    }

    static double polarAngle(Point origin, Point p) {
        return Math.atan2(p.y - origin.y, p.x - origin.x);
    }

    static List<Point> grahamScan(List<Point> gift) {
        gift = gift.stream()
                   .distinct()
                   .sorted(Comparator.comparingDouble(point -> -point.y))
                   .collect(Collectors.toList());

        Point pivot = gift.get(0);

        // Sort the remaining Points based on the angle between the pivot and itself
        List<Point> hull = gift.subList(1, gift.size());
        hull.sort(Comparator.comparingDouble(point -> polarAngle(point, pivot)));

        // The pivot is always on the hull
        hull.add(0, pivot);

        int n = hull.size();
        int m = 1;

        for (int i = 2; i < n; i++) {
            while (ccw(hull.get(m - 1), hull.get(m), hull.get(i)) <= 0) {
                if (m > 1) {
                    m--;
                } else if (m == 1) {
                    break;
                } else {
                    i++;
                }
            }
            m++;

            Point temp = hull.get(i);
            hull.set(i, hull.get(m));
            hull.set(m, temp);
        }
        return hull.subList(0, m + 1);
    }

    public static void main(String[] args) {
        ArrayList<Point> points = new ArrayList<>();

        points.add(new Point(-5, 2));
        points.add(new Point(5, 7));
        points.add(new Point(-6, -12));
        points.add(new Point(-14, -14));
        points.add(new Point(9, 9));
        points.add(new Point(-1, -1));
        points.add(new Point(-10, 11));
        points.add(new Point(-6, 15));
        points.add(new Point(-6, -8));
        points.add(new Point(15, -9));
        points.add(new Point(7, -7));
        points.add(new Point(-2, -9));
        points.add(new Point(6, -5));
        points.add(new Point(0, 14));
        points.add(new Point(2, 8));

        List<Point> convexHull = grahamScan(points);

        convexHull.forEach(p -> System.out.printf("% 1.0f, % 1.0f\n", p.x, p.y));
    }
}
