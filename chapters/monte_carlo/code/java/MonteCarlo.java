//submitted by DominikRafacz
import java.util.Random;

public class MonteCarlo {

    public static void main(String[] args){
        monteCarlo(10_000_000, 0.5);
    }

    //function to check whether point (x,y) is in circle of radius r
    private static boolean inCircle(double x, double y, double radius){
        return x*x + y*y < radius*radius;
    }

    //function to calculate estimation of pi
    public static void monteCarlo(int samples, double radius){
        int piCount = 0;

        Random random = new Random();

        for(int i = 0; i < samples; i++){
            double x = random.nextDouble();
            double y = random.nextDouble();
            if(inCircle(x, y, radius)){
                piCount++;
            }
        }

        double estimation = 4 * piCount / (samples * radius * radius);

        System.out.println("Estimated pi value: " + estimation);
        System.out.printf("Percent error: %.4f%%",  100*(Math.PI-estimation)/Math.PI);
    }
}
