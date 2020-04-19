import java.util.Random;

public class MonteCarlo {

    public static void main(String[] args) {
        double piEstimation = monteCarlo(1000);
        System.out.println("Estimated pi value: " + piEstimation);
        System.out.printf("Percent error: " + 100 * Math.abs(piEstimation - Math.PI) / Math.PI);
    }

    // function to check whether point (x,y) is in unit circle
    private static boolean inCircle(double x, double y) {
        return x * x + y * y < 1;
    }

    // function to calculate estimation of pi
    public static double monteCarlo(int samples) {
        int piCount = 0;

        Random random = new Random();

        for (int i = 0; i < samples; i++) {
            double x = random.nextDouble();
            double y = random.nextDouble();
            if (inCircle(x, y)) {
                piCount++;
            }
        }

        return 4.0 * piCount / samples;
    }
}
