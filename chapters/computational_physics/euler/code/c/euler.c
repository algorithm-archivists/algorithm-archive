#include <stdio.h>
#include <math.h>

void solve_euler(double timestep, double *result, size_t n){
    if(n != 0){
        result[1] = 1;
        for(size_t i = 2; i < n; ++i){
            result[i] = result[i-1] - 3.0*result[i-1]*timestep;
        }
    }
}

int check_result(double *result, size_t n, double threshold, double timestep){
    int is_approx = 1;
    for(size_t i = 0; i < n; ++i){
        double answr = exp(-3.0*(i - 1)* timestep);
        if(fabs(result[i] - answr) > threshold){
            printf("%f    %f\n", result[i], answr);
            is_approx = 0;
        }
    }
    return is_approx;
}

int main(){
    double result[100];

    solve_euler(0.01, result, 100);
    printf("%d\n", check_result(result, 100, 0.01, 0.01));

    return 0;
}
