public class Verlet {
    static double verlet(double pos, double acc, double dt) {

      // Note that we are using a temp variable for the previous position
      double prev_pos, temp_pos, time;
      prev_pos = pos;
      time = 0;

      while (pos > 0) {
            time += dt;
            temp_pos = pos;
            pos = pos*2 - prev_pos + acc * dt * dt;
            prev_pos = temp_pos;
        }

        return time;
    }
  
    static VerletValues stormer_verlet(double pos, double acc, double dt) {

        // Note that we are using a temp variable for the previous position
        double prev_pos, temp_pos, time, vel;
        prev_pos = pos;
        vel = 0;
        time = 0;
        while (pos > 0) {
            time += dt;
            temp_pos = pos;
            pos = pos*2 - prev_pos + acc * dt * dt;
            prev_pos = temp_pos;

            // The acceleration is constant, so the velocity is straightforward
             vel += acc*dt;
        }
      
       return new VerletValues(time, vel);
    }

    static VerletValues velocity_verlet(double pos, double acc, double dt) {

        // Note that we are using a temp variable for the previous position
        double time, vel;
        vel = 0;
        time = 0;
        while (pos > 0) {
            time += dt;
            pos += vel*dt + 0.5*acc * dt * dt;
            vel += acc*dt;
        }
        return new VerletValues(time, vel);
    }

    public static void main(String[] args) {

        double verletTime = verlet(5.0, -10, 0.01);
        System.out.println("Time for Verlet integration is: " + verletTime);
      
        VerletValues stormerVerlet = stormer_verlet(5.0, -10, 0.01);
        System.out.println("Time for Stormer Verlet integration is: " + stormerVerlet.time);
        System.out.println("Velocity for Stormer Verlet integration is: " + stormerVerlet.vel);
        
        VerletValues velocityVerlet = velocity_verlet(5.0, -10, 0.01);
        System.out.println("Time for velocity Verlet integration is: " + velocityVerlet.time);
        System.out.println("Velocity for velocity Verlet integration is: " + velocityVerlet.vel);
    }
}
