import java.io.*;
import java.util.*;

public class Main{

public static void main(String[] args) throws Exception {
    Scanner scn=new Scanner(System.in);
    int m1=scn.nextInt();
    int n1=scn.nextInt();
    int mat1[][]=new int[m1][n1];
    for(int i=0;i<mat1.length;i++){
        for(int j=0;j<mat1[0].length;j++){
            mat1[i][j]=scn.nextInt();
        }
    }
    int m2=scn.nextInt();
    int n2=scn.nextInt();
    int mat2[][]=new int[m2][n2];
    for(int i=0;i<mat2.length;i++){
        for(int j=0;j<mat2[0].length;j++){
            mat2[i][j]=scn.nextInt();
        }
    }
    if(n1==m2){
        int mat3[][]=multiply(mat1,mat2);
        for(int i=0;i<mat3.length;i++){
            for(int j=0;j<mat3[0].length;j++){
            System.out.print(mat3[i][j]+" ");   
            }
            System.out.println();
        }
    }else{
        System.out.println("Invalid input");
    }
 }
 public static int[][] multiply(int[][]mat1,int[][]mat2){
     int m1=mat1.length,n1=mat1[0].length;
     int m2=mat2.length,n2=mat2[0].length;
     int res[][]= new int[m1][n2];
     for(int i=0;i<res.length;i++){
         for(int j=0;j<res[0].length;j++){
             for(int k=0;k<n1;k++){
                 res[i][j] +=(mat1[i][k]*mat2[k][j]);
             }
         }
         }
         return res;
     }
 

}