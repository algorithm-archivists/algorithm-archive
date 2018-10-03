import java.util.ArrayList;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;

public class GrahamScan {

	static class Point {
		public double x;
		public double y;
		
		public Point(double x, double y) {
			this.x = x;
			this.y = y;
		}		
	}
	
	static double ccw(Point a, Point b, Point c) {
		return (b.x - a.x) * (c.y - a.y) - (b.y - a.y) * (c.x - a.x);
	}
	
	static double polarAngle(Point origin, Point p) {
		return Math.atan2(p.y - origin.y,  p.x - origin.x);
	}
	
	static List<Point> grahamScan(List<Point> gift) {
		gift = new ArrayList<Point>(new HashSet<Point>(gift)); //remove duplicate points
		
		// Sort the list so the point with the lowest y-coordinate comes first
		Comparator<Point> yComparator = (a, b) -> {
			if(b.y - a.y < 0)
				return -1;
			else if(b.y - a.y > 0)
				return 1;
			else return 0;
		};
		
		gift.sort(yComparator);
		Point pivot = gift.get(0);
		
		// Sort the remaining Points based on the angle between the pivot and itself
		List<Point> hull = gift.subList(1, gift.size());
		
		Comparator<Point> angleComparator = (a, b) -> {
			double angleA = polarAngle(a, pivot);
			double angleB = polarAngle(b, pivot);
			if(angleA - angleB < 0)
				return -1;
			else if(angleA - angleB > 0)
				return 1;
			else return 0;
		};
		
		hull.sort(angleComparator);
		
		// The pivot is always on the hull
		hull.add(0, pivot);
		
		int n = hull.size();
		int m = 1;
		for(int i = 2; i < n; i++) {
			while(ccw(hull.get(m - 1), hull.get(m), hull.get(i)) <= 0) {
				if(m > 1) {
					m--;
				} else if(m == 1) {
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
		ArrayList<Point> points = new ArrayList<Point>();
		
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
		
		convexHull.stream().forEach(p -> System.out.printf("% 1.0f, % 1.0f\n", p.x, p.y));
		
	}
	
}
