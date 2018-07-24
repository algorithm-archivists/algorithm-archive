#include <stdio.h>



#define n_rows 3
#define n_cols 3

/*Prints a matrix in a nice way*/
void print_matrix(double matrix[n_rows][n_cols]){
	int i;
	int j;
	for(i = 0; i < n_rows; i++){
		for(j = 0; j < n_cols; j++)
			printf(" %0.1f ", matrix[i][j]);
		printf("\n");
	}
}

int find_col_max_ind(double matrix[n_rows][n_cols], int column){
	int i;
	int max_ind = 0;
	int max = matrix[0][column - 1];

	for(i = 1; i < n_rows; i++){
		if(matrix[i][column - 1] > max){
			max = matrix[i][column - 1];
			max_ind = i;
		}
	}
	return max_ind;
}

void swap_rows(double matrix[n_rows][n_cols], int row_ind1, int row_ind2){
	double tmp;
	int i;

	for(i = 0; i < n_cols; i++){
		tmp = matrix[row_ind1][i];
		matrix[row_ind1][i] = matrix[row_ind2][i];
		matrix[row_ind2][i] = tmp;
	}
}

void gaussian_elimination(double matrix[n_rows][n_cols]){
}

int main(){
	double matrix[n_rows][n_cols] = {{1, 2, 3},
						 {4, 5, 6},
						 {7, 8, 9}};

	print_matrix(matrix);
	printf("\n%d", find_col_max_ind(matrix, 1));
	swap_rows(matrix, 0, 2);
	printf("\n");
	print_matrix(matrix);

	return 0;
}

