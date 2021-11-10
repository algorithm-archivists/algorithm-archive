import numpy as np
import matplotlib.pyplot as pp

def f(x):
    '''Function proportional to target distribution, a sum of Gaussians'''
    # Gaussian heights, width parameters, and mean positions respectively:
    a1, b1, x1 = 10,   4, -4
    a2, b2, x2 =  3, 0.2, -1
    a3, b3, x3 =  1,   2,  5
    
    return a1*np.exp( -b1*(x-x1)**2 ) + \
           a2*np.exp( -b2*(x-x2)**2 ) + \
           a3*np.exp( -b3*(x-x3)**2 )

def g():
    '''Random step vector'''
    return np.random.uniform(-1,1)

def iterate(x, f=f, g=g):
    '''Perform one full iteration and return new position'''
    
    x_proposed = x + g()
    A = min(1, f(x_proposed)/f(x) )
    
    u = np.random.uniform(0,1)
    if u <= A:
        x_new = x_proposed
    else:
        x_new = x
        
    return x_new

if __name__ == "__main__":
    xmin, xmax = -10, 10
    x0 = np.random.uniform(xmin, xmax)

    x_dat = [x0]  # for storing values
    N = 50_000   # no. of iterations
    
    x = x0
    for n in range(N):
        x = iterate(x)
        x_dat.append(x)
        
    # Write data to file
    output_string = "\n".join(str(x) for x in x_dat)
    
    with open("output.dat", "w") as out:
        out.write(output_string)
        out.write("\n")
        
    
    
    
