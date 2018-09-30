// Submitted by William103

public class ForwardEuler {
    public static void main(String[] args) {
		int n = 100;
		double dt = 0.01;
		double initial = 1;
		double threshold = 0.01;
        double[] result = forwardEuler(n, dt, initial);
		if (check(result, dt, threshold)) {
			System.out.println("All values below threshold!");
		} else {
			System.out.println("Value(s) outside of threshold!");
		}
    }

	static double[] forwardEuler(int n, double dt, double initial) {
		double[] result = new double[n];
		result[0] = initial;
		for (int i = 1; i < n; i++) {
			result[i] = result[i-1] - result[i-1] * 3 * dt;
		}
		return result;
	}

	static boolean check(double[] result, double dt, double threshold) {
		double solution;
		for (int i = 0; i < result.length; i++) {
			solution = Math.exp(-3 * i * dt);
			if (Math.abs(result[i] - solution) > threshold) {
				return false;
			}
		}
		return true;
	}
}
