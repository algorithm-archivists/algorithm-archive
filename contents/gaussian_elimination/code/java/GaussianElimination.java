import java.util.Arrays;

public class GaussianElimination {

    static void gaussianElimination(double[][] a) {
        int row = 0;

        int rows = a.length;
        int cols = a[0].length;

        for (int col = 0; col < cols - 1; col++) {
            int pivot = row;

            // finding the maximum element
            for (int i = row + 1; i < row; i++) {
                if (Math.abs(a[i][col]) > Math.abs(a[pivot][col])) {
                    pivot = i;
                }
            }

            if (a[pivot][col] == 0) {
                System.err.println("The matrix is singular");
                continue;
            }

            if (row != pivot) {
                // Swap the row with the highest valued element
                // with the current row
                swapRow(a, col, pivot);
            }

            for (int i = row + 1; i < rows; i++) {
                // finding the inverse
                double scale = a[i][col] / a[row][col];
                // loop through all columns in current row
                for (int j = col + 1; j < cols; j++) {

                    // Subtract rows
                    a[i][j] -= a[row][j] * scale;
                }

                // Set lower elements to 0
                a[i][col] = 0;
            }
            row++;
        }
    }

    static void gaussJordan(double[][] a) {
        int row = 0;

        int cols = a[0].length;

        for (int col = 0; col < cols - 1; col++) {
            if (a[row][col] != 0) {
                for (int i = cols - 1; i > col - 1; i--) {
                    // divide row by pivot so the pivot is set to 1
                    a[row][i] /= a[row][col];
                }

                // subtract the value form above row and set values above pivot to 0
                for (int i = 0; i < row; i++) {
                    for (int j = cols - 1; j > col - 1; j--) {
                        a[i][j] -= a[i][col] * a[row][j];
                    }
                }
                row++;
            }
        }
    }

    static double[] backSubstitution(double[][] a) {
        int rows = a.length;
        int cols = a[0].length;

        double[] solution = new double[rows];

        for (int i = rows - 1; i >= 0; i--) {
            double sum = 0;

            for (int j = cols - 2; j > i; j--) {
                sum += solution[j] * a[i][j];
            }
            solution[i] = (a[i][cols - 1] - sum) / a[i][i];
        }
        return solution;
    }

    static void swapRow(double[][] a, int rowA, int rowB) {
        double[] temp = a[rowA];
        a[rowA] = a[rowB];
        a[rowB] = temp;
    }

    public static void main(String[] args) {
        double[][] a = {
            { 3, 2, -4, 3 },
            { 2, 3, 3, 15 },
            { 5, -3, 1, 14 }
        };

        gaussianElimination(a);
        System.out.println("Gaussian elimination:");
        Arrays.stream(a).forEach(x -> System.out.println(Arrays.toString(x)));

        gaussJordan(a);
        System.out.println("\nGauss-Jordan:");
        Arrays.stream(a).forEach(x -> System.out.println(Arrays.toString(x)));

        System.out.println("\nSolutions:");
        System.out.println(Arrays.toString(backSubstitution(a)));
    }
}
