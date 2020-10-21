import java.io.*;
import java.util.*;

public class Main {

    public static void main(String[] args) throws Exception {
       Scanner sc= new Scanner(System.in);
       int row=sc.nextInt();
       int col=sc.nextInt();
       int arr[][]= new int[row][col];
       for(int i=0;i<arr.length;i++){
           for(int j=0;j<arr[0].length;j++){
               arr[i][j]=sc.nextInt();
               
           }
       }
       
      int dir=0, r=0,c=0;
       while(true){
           int val=arr[r][c];
            dir=((dir+val)%4);
            if(dir==0){
                c=c+1;
            }
            else if(dir==1){
                r=r+1;
                
            }else if(dir==2){
                c=c-1;
            }else{
                r=r-1;
            }
            if(r==-1){
                r++;break;
            }else if(c==-1){
                c++; break;
            }else if(r==arr.length){
                r--;break;
            }else if(c==arr[0].length){
                c--;break;
            }
       }
       System.out.println(r);
       System.out.println(c);
       
    }

}