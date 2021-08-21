import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;

public class Barnsley {

    private static class Point {
        public double x, y, z;

        public Point(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        public Point(double[] coordinates) {
            this.x = coordinates[0];
            this.y = coordinates[1];
            this.z = coordinates[2];
        }

        public Point matrixMultiplication(double[][] matrix) {
            double[] results = new double[3];
            for (int i = 0; i < 3; i++) {
                results[i] = matrix[i][0] * x + matrix[i][1] * y + matrix[i][2] * z;
            }
            return new Point(results);
        }
    }

    // This is a function that reads in the Hutchinson operator and corresponding
    //   probabilities and outputs a randomly selected transform
    // This works by choosing a random number and then iterating through all
    //   probabilities until it finds an appropriate bin
    public static double[][] selectArray(double[][][] hutchinsonOp, double[] probabilities) {
        Random rng = new Random();
        // Random number to be binned
        double rand = rng.nextDouble();

        // This checks to see if a random number is in a bin, if not, that
        // probability is subtracted from the random number and we check the
        // next bin in the list
        for (int i = 0; i < probabilities.length; i++) {
            if (rand < probabilities[i])
                return hutchinsonOp[i];
            rand -= probabilities[i];
        }
        // This return will never be reached, as the loop above ensures that at some point rand will be smaller
        // than a probability. However, Java does not know this and thus this return is needed for compilation.
        return null;
    }

    // This is a general function to simulate a chaos game
    // n is the number of iterations
    // initialLocation is the starting point of the chaos game
    // hutchinsonOp is the set of functions to iterate through
    // probabilities is the set of probabilities corresponding to the likelihood
    //   of choosing their corresponding function in hutchinsonOp
    public static Point[] chaosGame(int n, Point initialLocation, double[][][] hutchinsonOp, double[] probabilities) {
        // Initializing output points
        Point[] outputPoints = new Point[n];
        Point point = initialLocation;

        for (int i = 0; i < n; i++) {
            outputPoints[i] = point;
            point = point.matrixMultiplication(selectArray(hutchinsonOp, probabilities));
        }

        return outputPoints;
    }

    public static void main(String[] args) {
        double[][][] barnsleyHutchinson = {
                {{0.0, 0.0, 0.0},
                 {0.0, 0.16, 0.0},
                 {0.0, 0.0, 1.0}},
                {{0.85, 0.04, 0.0},
                 {-0.04, 0.85, 1.60},
                 {0.0, 0.0, 1.0}},
                {{0.20, -0.26, 0.0},
                 {0.23, 0.22, 1.60},
                 {0.0, 0.0, 1.0}},
                {{-0.15, 0.28, 0.0},
                 {0.26, 0.24, 0.44},
                 {0.0, 0.0, 1.0}}
        };
        double[] barnsleyProbabilities = new double[]{0.01, 0.85, 0.07, 0.07};
        Point[] outputPoints = chaosGame(10000, new Point(0.0, 0.0, 1.0), barnsleyHutchinson, barnsleyProbabilities);
        try (FileWriter fw = new FileWriter("barnsley.dat")) {
            for (Point p : outputPoints) {
                fw.write(p.x + "\t" + p.y + "\n");
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

}
