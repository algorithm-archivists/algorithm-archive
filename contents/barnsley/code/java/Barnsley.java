import java.io.FileWriter;
import java.io.IOException;
import java.util.Random;

public class Barnsley {

    private static class Point {
        double x, y, z;

        public Point(double x, double y, double z) {
            this.x = x;
            this.y = y;
            this.z = z;
        }

        public double[] toDoubleArray() {
            return new double[]{this.x, this.y, this.z};
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
            point = matrixMultiplication(selectArray(hutchinsonOp, probabilities), point.toDoubleArray());
        }

        return outputPoints;
    }

    public static Point matrixMultiplication(double[][] operation, double[] point) {
        double[] result = new double[3];
        for (int j = 0; j < operation[0].length; j++)
            for (int i = 0; i < operation.length; i++)
                result[j] += operation[j][i] * point[i];
        return new Point(result[0], result[1], result[2]);
    }

    public static void main(String[] args) throws IOException {
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
        FileWriter fw = new FileWriter("barnsley.dat");
        for (Point p : outputPoints)
            fw.write(p.x + "\t" + p.y + "\n");
        fw.close();
    }

}
