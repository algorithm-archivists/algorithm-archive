import java.util.*;

class JarvisMarch {
  
  //calculates the interior angle created by two line segments, ab and cb
  public static double angle(Integer[] a, Integer[] b, Integer[] c) {
    if((a[0]==b[0]&&a[1]==b[1])||(c[0]==b[0]&&c[1]==b[1]))
      return 0;
    else {
      double bs = Math.sqrt(Math.pow(a[0]-b[0],2) + Math.pow(a[1]-b[1],2));
      double cs = Math.sqrt(Math.pow(c[0]-b[0],2) + Math.pow(c[1]-b[1],2));
      double as = Math.sqrt(Math.pow(a[0]-c[0],2) + Math.pow(a[1]-c[1],2));
      double theta = Math.acos(((bs*bs)+(cs*cs)-(as*as))/(2*bs*cs));
      return theta;
    }
  }
  
  public static ArrayList<Integer[]> jarvisMarch(ArrayList<Integer[]> arr) {
    ArrayList<Integer[]> hull = new ArrayList<Integer[]>();
    Integer[] pointOnHull = {Integer.MAX_VALUE,0};
    for(Integer[] p: arr) {
      if(p[0]<pointOnHull[0])
        pointOnHull = p;
    }
    hull.add(pointOnHull);
    Integer[] ref;
    while(true) {
      if(hull.size()==1)
        ref = new Integer[]{pointOnHull[0],pointOnHull[1]-1}; //finds a third point to use in calculating the angle
      else
        ref = hull.get(hull.size()-2);
      Integer[] endpoint = arr.get(0); //initial canidate for next point in hull
      for(Integer[] p: arr) {
        if(angle(p,pointOnHull,ref)>angle(endpoint,pointOnHull,ref)) { //found a point that makes a greater angle
          endpoint = p;
        }
      }
      pointOnHull = endpoint;
      if(pointOnHull[0]==hull.get(0)[0]&&pointOnHull[1]==hull.get(0)[1])
        break;
      else {
        hull.add(pointOnHull);
      }
    }
    return hull;
  }
  
  public static void main(String[] args) {
    Integer[][] lst = new Integer[][]{{-5, 2}, {5, 7}, {-6, -12}, {-14, -14}, {9, 9},
                                      {-1, -1}, {-10, 11}, {-6, 15}, {-6, -8}, {15, -9}, 
                                      {7, -7}, {-2, -9}, {6, -5}, {0, 14}, {2, 8}};
    ArrayList<Integer[]> gift = new ArrayList<Integer[]>();
    for(Integer[] p: lst) {
      gift.add(p);
    }
    System.out.println("Gift:");
    for(Integer[] p: gift) {
      System.out.println(Arrays.toString(p));
    }
    ArrayList<Integer[]> wrapping = jarvisMarch(gift);
    System.out.println("Wrapping:");
    for(Integer[] p: wrapping) {
      System.out.println(Arrays.toString(p));
    }
  }
  
}