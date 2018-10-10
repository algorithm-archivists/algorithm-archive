import java.util.*;

class JarvisMarch {
  
  public static class Point {
    private double x, y;
    
    public Point(double a, double b) {
      x = a; y = b;
    }
    public Point(Integer[] p) {
      x = p[0]; y = p[1];
    }
    
    public double getX() { return x; }
    public double getY() { return y; }
    
    public boolean equals(Point p) {
      if(p.getX()==x&&p.getY()==y)
        return true;
      else
        return false;
    }
    public double magnitude() {
      return Math.sqrt(Math.pow(x, 2) + Math.pow(y, 2));
    }
    public void print() {
      System.out.println("["+x+", "+y+"]");
    }
  }
  
  //find the angle by creating two vectors and then using a property of dot products
  private static double angle(Point a, Point b, Point c) {
    Point ab = new Point(b.getX()-a.getX(), b.getY()-a.getY());
    Point bc = new Point(c.getX()-b.getX(), c.getY()-b.getY());
    return Math.acos(-1*((ab.getX()*bc.getX())+(ab.getY()*bc.getY()))/
                         (ab.magnitude()*bc.magnitude()));
  }
  
  public static ArrayList<Point> jarvisMarch(ArrayList<Point> arr) {
    ArrayList<Point> hull = new ArrayList<Point>();
    Point pointOnHull = new Point(Double.MAX_VALUE, 0);
    
    //find leftmost point
    for(Point p: arr) {
      if(p.getX()<pointOnHull.getX())
        pointOnHull = p;
    }
    hull.add(pointOnHull);
    
    //look for the rest of the points on the hull
    Point ref;
    while(true) {
      if(hull.size()==1)
        ref = new Point(pointOnHull.getX(),pointOnHull.getY()+1); //finds a third point to use in calculating the angle
      else
        ref = hull.get(hull.size()-2);
      Point endpoint = arr.get(0); //initial canidate for next point in hull
      for(Point p: arr) {
        if(angle(p,pointOnHull,ref)>angle(endpoint,pointOnHull,ref)) { //found a point that makes a greater angle
          endpoint = p;
        }
      }
      pointOnHull = endpoint;
      if(pointOnHull.equals(hull.get(0))) //add next point to hull if not equal to the leftmost point
        break;
      else {
        hull.add(pointOnHull);
      }
    }
    return hull;
  }
  
  public static void main(String[] args) {
    
    //test array setup
    Integer[][] lst = new Integer[][]{{-5, 2}, {5, 7}, {-6, -12}, {-14, -14}, {9, 9},
                                      {-1, -1}, {-10, 11}, {-6, 15}, {-6, -8}, {15, -9}, 
                                      {7, -7}, {-2, -9}, {6, -5}, {0, 14}, {2, 8}};
    ArrayList<Integer[]> lst2 = new ArrayList<Integer[]>(Arrays.asList(lst));
    ArrayList<Point> gift = new ArrayList<Point>();
    for(Integer[] i: lst2) {
      gift.add(new Point(i));
    }
    
    //print initial array of points
    System.out.println("Gift:");
    for(Integer[] p: lst2) {
      System.out.println(Arrays.toString(p));
    }
    
    //find and print the array of points in the hull
    ArrayList<Point> hull = jarvisMarch(gift);
    System.out.println("Wrapping:");
    for(Point p: hull) {
      p.print();
    }
  }
  
}