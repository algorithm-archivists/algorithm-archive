import java.io.FileWriter;
import java.util.Random;

public class IFS {

    private static class Point {
        double x, y;

        public Point(double x, double y) {
            this.x = x;
            this.y = y;
        }
    }
    
    // This is a function to simulate a "chaos game"
    public static Point[] chaosGame(int n, Point[] shapePoints) {
        Random rng = new Random();

        // Initialize output vector
        Point[] outputPoints = new Point[n];

        // Choose first point randomly
        Point point = new Point(rng.nextDouble(), rng.nextDouble());

        for (int i = 0; i < n; i++) {
            outputPoints[i] = point;

            // Clone point to get a new reference
            point = new Point(point.x, point.y);

            // Retrieve random shape point
            Point temp = shapePoints[rng.nextInt(shapePoints.length)];
            // Calculate midpoint
            point.x = 0.5 * (point.x + temp.x);
            point.y = 0.5 * (point.y + temp.y);
        }

        return outputPoints;
    }

    public static void main(String[] args) throws Exception {
        // This will generate a Sierpinski triangle with a chaos game of n points for an
        // initial triangle with three points on the vertices of an equilateral triangle:
        //     A = (0.0, 0.0)
        //     B = (0.5, sqrt(0.75))
        //     C = (1.0, 0.0)
        // It will output the file sierpinski.dat, which can be plotted after
        Point[] shapePoints = new Point[]{
                new Point(0.0, 0.0),
                new Point(0.5, Math.sqrt(0.75)),
                new Point(1.0, 0.0)
        };
        Point[] outputPoints = chaosGame(10000, shapePoints);

        FileWriter fw = new FileWriter("sierpinski.dat");
        for (Point p : outputPoints)
            fw.write(p.x + "\t" + p.y + "\n");
        fw.close();
    }

}