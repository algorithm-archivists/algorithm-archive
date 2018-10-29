public class ForwardEuler {
    private static double[] solveEuler(double timestep, int n) {
        double[] eulerResult = new double[n];

        //Setting the initial condition
        eulerResult[0] = 1;
        for(int i = 1; i < eulerResult.length; i++) {
            eulerResult[i] = eulerResult[i - 1] - (3 * eulerResult[i - 1] * timestep);
        }
        return eulerResult;
    }

    private static boolean checkResult(double[] eulerResult, double threshold, double timestep) {
        boolean isApprox = true;

        for(int i = 0; i < eulerResult.length; i++) {
            double time = i * timestep;
            double solution = Math.exp(-3 * time);
            if(Math.abs(eulerResult[i] - solution) > threshold) {
                System.out.println(eulerResult[i] + " " + solution);
                isApprox = false;
            }
        }

        return isApprox;
    }

    public static void main(String[] args) {
        double timestep = 0.1;
        int n = 100;
        double threshold = 0.1;

        double[] eulerResult = solveEuler(timestep, n);
        boolean isApprox = checkResult(eulerResult, threshold, timestep);

        System.out.println(isApprox);
    }
}
