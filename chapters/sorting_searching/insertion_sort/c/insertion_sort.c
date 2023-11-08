#include <stdio.h>

void print_array(int *array, int arr_len){
	
	putchar('[');
	for(int i = 0; i < arr_len; i++){
		if(i < (arr_len - 1))
			printf("%d, ", array[i]);
		else
			printf("%d]\n", array[i]);
	}
}

/*Insertion sort sorts the array inplace*/
void insertion_sort(int *array, int arr_len){

	/*loop through array[1:n], array[0] is already sorted*/
	for(int j = 1; j < arr_len; ++j){
		int current_element = array[j];

		/*Place the j-th element to the correct position in the sub array array[0...j]
		 Keeping array[0...j] sorted*/
		int i = j - 1;
		while((i >= 0) && (array[i] > current_element)){
			array[i + 1] = array[i];
			i -= 1;
		}
		array[i + 1] = current_element;
	}
}

int main(){
	int arr_len = 10;
	int array[] = {10, 1, 3, 4, 7, 2, 5, 9, 6, 8};

	printf("This is the array after sorting: ");
	insertion_sort(array, arr_len);
	print_array(array, arr_len);

	return 0;
}
