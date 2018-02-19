#include <iostream>
#include <cmath>
#include <vector>
#include <cstddef>

using std::size_t;

std::vector<double> solve_euler(double timestep, size_t size){
    if(size != 0){
        auto result = std::vector<double>(size);
        result[0] = 1.0;
        for(size_t i = 1; i < size; ++i){
            result[i] = result[i-1] - 3.0*result[i-1]*timestep;
        }
        return result;
    }
}

bool check_result(std::vector<double>result, double threshold,
        double timestep){

    bool is_approx = true;
    for(size_t i = 0; i < result.size(); ++i){
        double solution = std::exp(-3.0*i* timestep);
        if(std::fabs(result[i] - solution) > threshold){
            std::cout << result[i] << "     " << solution << std::endl;
            is_approx = false;
        }
    }
    return is_approx;
}

int main(){
    double threshold = 0.01;
    double timestep = 0.01;

    std::vector<double> result = solve_euler(timestep, 100);
    std::cout << check_result(result, threshold, timestep) << std::endl;

    return 0;
}
