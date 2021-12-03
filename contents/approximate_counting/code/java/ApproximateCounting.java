import java.lang.Math;
import java.util.stream.DoubleStream;

public class ApproximateCounting {
    
    /*
     * This function taks
     *   - v: value in register
     *   - a: a scaling value for the logarithm based on Morris's paper
     * It returns the approximate count
     */
    static double n(double v, double a) {
        return a * (Math.pow(1 + 1 / a, v) - 1);
    }
    

    /*
     * This function takes
     *   - v: value in register
     *   - a: a scaling value for the logarithm based on Morris's paper
     * It returns the new value for v
     */
    static double increment(double v, double a) {
        double delta = 1 / (n(v + 1, a) - n(v, a));

        if (Math.random() <= delta) {
            return v + 1;
        } else {
            return v;
        }
    }


    
    /*
     * This function takes
     *   - v: value in register
     *   - a: a scaling value for the logarithm based on Morris's paper
     * It returns the new value for v
     */
    static double approximateCount(int nItems, double a) {
        double v = 0;
    
        for (int i = 0; i < nItems; i++) {
            v = increment(v, a);
        }

        return n(v, a);
    }

    /*
     * This function takes
     *   - nTrials: the number of counting trails
     *   - nItems: the number of items to count
     *   - a: a scaling value for the logarithm based on Morris's paper
     *   - threshold: the maximum percent error allowed
     * It terminates the program on failure
     */
    static void testApproximateCount(int nTrials, int nItems, double a, double threshold) {
        double avg = DoubleStream.generate(() -> approximateCount(nItems, a))
                     .limit(nTrials)
                     .average()
                     .getAsDouble();
    
        if (Math.abs((avg - nItems) / nItems) < threshold) {
            System.out.println("passed");
        } else {
            System.out.println("failed");
        }
    }


    public static void main(String args[]) {
        System.out.println("[#]\nCounting Tests, 100 trials");
        System.out.println("[#]\ntesting 1,000, a = 30, 10% error");
        testApproximateCount(100, 1_000, 30, 0.1);

        System.out.println("[#]\ntesting 12,345, a = 10, 10% error");
        testApproximateCount(100, 12_345, 10, 0.1);
    
        System.out.println("[#]\ntesting 222,222, a = 0.5, 20% error");
        testApproximateCount(100, 222_222, 0.5, 0.2);
    }

}
