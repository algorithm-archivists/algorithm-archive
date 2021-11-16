import numpy as np


def f(x, normalize=False):
    '''
    Function proportional to target distribution, a sum of Gaussians.
    For testing, set normalize to True, to get target distribution exactly.
    '''
    # Gaussian heights, width parameters, and mean positions respectively:
    a1, b1, x1 = 10,   4, -4
    a2, b2, x2 =  3, 0.2, -1
    a3, b3, x3 =  1,   2,  5

    if normalize:
        norm = np.sqrt(np.pi)*(a1/np.sqrt(b1) + a2/np.sqrt(b2) + a3/np.sqrt(b3))
        a1, a2, a3 = a1/norm, a2/norm, a3/norm

    return (
           a1 * np.exp(-b1 * (x - x1)**2) + 
           a2 * np.exp(-b2 * (x - x2)**2) + 
           a3 * np.exp(-b3 * (x - x3)**2)
        )

def g():
    '''Random step vector.'''
    return np.random.uniform(-1,1)

def metropolis_step(x, f=f, g=g):
    '''Perform one full iteration and return new position.'''
    
    x_proposed = x + g()
    a = min(1, f(x_proposed) / f(x))
    
    x_new = np.random.choice([x_proposed, x], p=[a, 1-a])
        
    return x_new

def metropolis_iterate(x0, num_steps):
    '''Iterate metropolis algorithm for num_steps using iniital position x_0'''
    
    x_dat = [x0]
    x = x0
    for n in range(num_steps):
        x = metropolis_step(x)
        x_dat.append(x)
    
    return x_dat
    

def test_metropolis_iterate(num_steps, xmin, xmax, x0):
    '''
    Calculate error in normalized density histogram of data  
    generated by metropolis_iterate() by using 
    normalized-root-mean-square-deviation metric. 
    '''
    
    bin_width = 0.25
    bins = np.arange(xmin, xmax + bin_width/2, bin_width)
    centers = np.arange(xmin + bin_width/2, xmax, bin_width)
    
    true_values = f(centers, normalize=True)
    mean_value = np.mean(true_values)

    x_dat = metropolis_iterate(x0, num_steps)
    heights, _ = np.histogram(x_dat, bins=bins, density=True)
                    
    nmsd = np.average( (heights - true_values)**2 / mean_value )
    nrmsd = np.sqrt(nmsd)

    print(f"    NRMSD Error : {nrmsd*100:4.1f} %")

        
 
if __name__ == "__main__":
    xmin, xmax = -7, 7
    x0 = np.random.uniform(xmin, xmax)

    num_steps = 50_000

    x_dat = metropolis_iterate(x0, 50_000)
        
    # Write data to file
    output_string = "\n".join(str(x) for x in x_dat)
    
    with open("output.dat", "w") as out:
        out.write(output_string)
        out.write("\n")
        
    
    # Testing
    for num_steps in (500, 5_000, 50_000):
        print(f"Testing with x0 = {x0:5.2f} and num_steps = {num_steps:,}")
        test_metropolis_iterate(num_steps, xmin, xmax, x0)
