#include<stdio.h>
int main(){
	int a[5],swap,i,j,flag;
	for(i=0;i<5;i++){
		scanf("\n%d",&a[i]);
	}
	for(i=0;i<4;i++){
		flag=0;
		for(j=0;j<5-i-1;j++){
			if(a[j]>a[j+1]){
				swap=a[j];
				a[j]=a[j+1];
				a[j+1]=swap;
				flag=1;
			}
			
		}
		if(i==0&&flag==0){
			printf("\n the list is already sorted");
			break;
		}
		
	}
	printf("\n the sorted array is:-");
	for(i=0;i<5;i++){
		printf("\n%d",a[i]);
	}
	return 0;
}
