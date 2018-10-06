// Submitted by William103

class ForwardEuler {
	static int n = 100;
	static double dt = 0.01;
	static double initial = 1;
    static double threshold = 0.01;
    public static void main(String[] args) {
        double[] result = forwardEuler();
		    if (check(result)) {
			    System.out.println("All values below threshold!");
		    } else {
			    System.out.println("Value(s) outside of threshold!");
		    }
    }

	static double[] forwardEuler() {
		double[] result = new double[n];
		result[0] = initial;
		for (int i = 1; i < n; i++) {
			result[i] = result[i-1] - result[i-1] * 3 * dt;
		}
		return result;
	}

	static boolean check(double[] result) {
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
