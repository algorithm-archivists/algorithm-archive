public class thomas {
    private static void thomasAlgorithm(double a[], double b[], double c[], double x[], int size) {

        double y[] = new double[size];

        y[0] = c[0] / b[0];
        x[0] = x[0] / b[0];

        for (int i = 1; i < size; ++i) {
            double scale = 1.0 / (b[i] - a[i] * y[i - 1]);
            y[i] = c[i] * scale;
            x[i] = (x[i] - a[i] * x[i - 1]) * scale;
        }

        for (int i = size - 2; i >= 0; --i) {
            x[i] -= y[i] * x[i + 1];
        }
    }

    public static void main(String[] args) {
        double a[] = {0.0, 2.0, 3.0};
        double b[] = {1.0, 3.0, 6.0};
        double c[] = {4.0, 5.0, 0.0};
        double x[] = {7.0, 5.0, 3.0};

        System.out.println("The system,\n");
        System.out.println("[1.0  4.0  0.0][x] = [7.0]\n");
        System.out.println("[2.0  3.0  5.0][y] = [5.0]\n");
        System.out.println("[0.0  3.0  6.0][z] = [3.0]\n");
        System.out.println("has the solution:\n");

        thomasAlgorithm(a, b, c, x, 3);

        for (int i = 0; i < 3; ++i)
            System.out.println("[" + x[i] + "]\n");
    }
}
