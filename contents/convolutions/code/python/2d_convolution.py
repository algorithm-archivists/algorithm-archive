import numpy as np
from contextlib import suppress


def convolve_linear(signal, filter, output_size):
    out = np.zeros(output_size)
    sum = 0

    for i in range(output_size[0]):
        for j in range(output_size[1]):
            for k in range(max(0, i-filter.shape[0]), i+1):
                for l in range(max(0, j-filter.shape[1]), j+1):
                    with suppress(IndexError):
                        sum += signal[k, l] * filter[i-k, j-l]
            out[i, j] = sum
            sum = 0

    return out


def create_gaussian_kernel(kernel_size):
    kernel = np.zeros((kernel_size, kernel_size))

    # The center must be offset by 0.5 to find the correct index
    center = kernel_size*0.5 + 0.5

    sigma = np.sqrt(0.1*kernel_size)

    def kernel_function(x, y):
        return np.exp(-((x-center+1)**2 + (y-center+1)**2)/(2*sigma**2))

    kernel = np.fromfunction(kernel_function, (kernel_size, kernel_size))
    return kernel / np.linalg.norm(kernel)


def create_sobel_operators():
    Sx = np.dot([[1.0], [2.0], [1.0]], [[-1.0, 0.0, 1.0]]) / 9
    Sy = np.dot([[-1.0], [0.0], [1.0]], [[1.0, 2.0, 1.0]]) / 9

    return Sx, Sy

def sum_matrix_dimensions(mat1, mat2):
    return (mat1.shape[0] + mat2.shape[0], 
            mat1.shape[1] + mat2.shape[1])

def compute_sobel(signal):
    Sx, Sy = create_sobel_operators()
    
    Gx = convolve_linear(signal, Sx, sum_matrix_dimensions(signal, Sx))
    Gy = convolve_linear(signal, Sy, sum_matrix_dimensions(signal, Sy))

    return np.sqrt(np.power(Gx, 2) + np.power(Gy, 2))


def create_circle(image_resolution, grid_extents, radius):
    out = np.zeros((image_resolution, image_resolution))

    for i in range(image_resolution):
        x_position = ((i * grid_extents / image_resolution)
                      - 0.5 * grid_extents)
        for j in range(image_resolution):
            y_position = ((j * grid_extents / image_resolution)
                          - 0.5 * grid_extents)
            if x_position ** 2 + y_position ** 2 <= radius ** 2:
                out[i, j] = 1.0

    return out


def main():

    # Random distribution in x
    x = np.random.rand(100, 100)

    # Gaussian signals
    def create_gaussian_signals(i, j):
        return np.exp(-(((i-50)/100) ** 2 +
                        ((j-50)/100) ** 2) / .01)
    y = np.fromfunction(create_gaussian_signals, (100, 100))

    # Normalization is not strictly necessary, but good practice
    x /= np.linalg.norm(x)
    y /= np.linalg.norm(y)

    # full convolution, output will be the size of x + y
    full_linear_output = convolve_linear(x, y, sum_matrix_dimensions(x, y))

    # simple boundaries
    simple_linear_output = convolve_linear(x, y, x.shape)

    np.savetxt("full_linear.dat", full_linear_output)
    np.savetxt("simple_linear.dat", simple_linear_output)

    # creating simple circle and 2 different Gaussian kernels
    circle = create_circle(50, 2, 0.5)

    circle = circle / np.linalg.norm(circle)

    small_kernel = create_gaussian_kernel(3)
    large_kernel = create_gaussian_kernel(25)

    small_kernel_output = convolve_linear(circle, small_kernel,
                                          sum_matrix_dimensions(circle,
                                                                small_kernel))

    large_kernel_output = convolve_linear(circle, large_kernel,
                                          sum_matrix_dimensions(circle,
                                                                large_kernel))

    np.savetxt("small_kernel.dat", small_kernel_output)
    np.savetxt("large_kernel.dat", large_kernel_output)

    circle = create_circle(50, 2, 0.5)

    # Normalization
    circle = circle / np.linalg.norm(circle)

    # using the circle for sobel operations as well
    sobel_output = compute_sobel(circle)

    np.savetxt("sobel_output.dat", sobel_output)

