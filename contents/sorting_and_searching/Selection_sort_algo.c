                              //Selecting Sort
#include<stdio.h>
#include<conio.h>
void selection_sort(int arr[],int num){
int i=0,j=0,temp,imin,cmp=0;
for(i=0;i<num;i++){
	imin=i;
	for(j=i+1;j<num;j++){
		if(arr[j]<arr[imin]){
			imin=j;			
		}
	}
	if(imin !=i){	
	temp=arr[i];
	arr[i]=arr[imin];
	arr[imin]=temp;
     	cmp++;
}
}	
printf("Sorted element:-");
for(i=0;i<num;i++){
	printf("%d\n",arr[i]);
}
printf("\nthe no.of Swaps:-%d",cmp);
}
int main(){
	int i=0,n;
	int a[50];
	printf("\nno.of element you want to sort:-");
	scanf("%d",&n);
	printf("\nEnter the values:-");
	for(i=0;i<n;i++){
		scanf("%d",&a[i]);		
	}
	selection_sort(a,n);
	return 0;
}

