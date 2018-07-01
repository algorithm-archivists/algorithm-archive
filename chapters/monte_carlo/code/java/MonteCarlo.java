//submitted by DominikRafacz
import java.util.Random;

public class MonteCarlo {

    public static void main(String[] args){
        monteCarlo(10_000_000);
    }

    //function to check whether point (x,y) is in unit circle
    private static boolean inCircle(double x, double y){
        return x*x + y*y < 1;
    }

    //function to calculate estimation of pi
    public static void monteCarlo(int samples){
        int piCount = 0;

        Random random = new Random();

        for(int i = 0; i < samples; i++){
            double x = random.nextDouble();
            double y = random.nextDouble();
            if(inCircle(x, y)){
                piCount++;
            }
        }

        double estimation = 4.0 * piCount / (samples);

        System.out.println("Estimated pi value: " + estimation);
        System.out.printf("Percent error: %.4f%%",  100*(Math.PI-estimation)/Math.PI);
    }
}
