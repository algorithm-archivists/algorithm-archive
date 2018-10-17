public class ForwardEuler {
  
  public static double[] append(double[] oldArray, double newItem) {
    double[] newArray = new double[oldArray.length + 1];

    for(int i = 0; i < oldArray.length; i++) {
        newArray[i] = oldArray[i];
    }
    newArray[newArray.length - 1] = newItem;

    return newArray;
}
  
  public static double[] solveEuler(double timeStep, int n) {
    double[] result = {1};

    for(int i = 0; i < n; i++) {
      result = append(result, result[i] - 3 * result[i] * timeStep);
    }

    return result;
  }
  
  public static boolean checkResult(double[] result, double threshold, double timeStep) {
    boolean isApprox = true;

    for(int i = 0; i < result.length; i++) {
        double solution = Math.pow(Math.E, -3 * i * timeStep);
      
        if(Math.abs(result[i] - solution) > threshold) {
            isApprox = false;
        }
    }

    return isApprox;
}

  public static void main(String[] args) {
    double timeStep = 0.01;
    int n = 100;
    double threshold = 0.01;
    
    double[] result = solveEuler(timeStep, n);
    boolean isApprox = checkResult(result, threshold, timeStep);

    if(isApprox) {
        System.out.print("All values within threshold");
    } else {
        System.out.print("Value(s) not in threshold");
    } 
  }
}
