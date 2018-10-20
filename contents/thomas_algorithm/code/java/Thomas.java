public class Thomas {
    private static double[] thomasAlgorithm(double[] a, double[] b, double[] c, double[] x) {
        int size = a.length;
        double[] y = new double[size]; // This is needed so that we don't have to modify c
        double[] solution = new double[size];

        // Set initial elements
        y[0] = c[0] / b[0];
        solution[0] = x[0] / b[0];

        for (int i = 1; i < size; ++i) {
            // Scale factor is for c and x
            double scale = 1.0 / (b[i] - a[i] * y[i - 1]);
            y[i] = c[i] * scale;
            solution[i] = (x[i] - a[i] * solution[i - 1]) * scale;
        }

        // Back-substitution
        for (int i = size - 2; i >= 0; --i) {
            solution[i] -= y[i] * solution[i + 1];
        }

        return solution;
    }

    public static void main(String[] args) {
        double[] a = {0.0, 2.0, 3.0};
        double[] b = {1.0, 3.0, 6.0};
        double[] c = {4.0, 5.0, 0.0};
        double[] x = {7.0, 5.0, 3.0};
        double[] solution = thomasAlgorithm(a, b, c, x);

        System.out.format("The system,\n");
        System.out.format("[%.1f, %.1f, %.1f][x] = [%.1f]\n", b[0], c[0], 0f, x[0]);
        System.out.format("[%.1f, %.1f, %.1f][y] = [%.1f]\n", a[1], b[1], c[1], x[1]);
        System.out.format("[%.1f, %.1f, %.1f][z] = [%.1f]\n", 0f, a[2], b[2], x[2]);
        System.out.format("has the solution:\n");

        for (int i = 0; i < solution.length; i++) {
            System.out.format("[% .5f]\n", solution[i]);
        }
    }
}
