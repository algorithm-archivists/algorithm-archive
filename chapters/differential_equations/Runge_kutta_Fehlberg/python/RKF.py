'''Runge-Kutta Fehlberg:
    The Runge-Kutta-Fehlberg method is a variation of the regular
    RK explicit methods - with the variant being the step size.
    Generally speaking - this methods adapts to different step sizes
    depending on if the solution is chaotic vs. non-chaotic in certain bins


    This method is defined for ODEs of the form
                        y' = f(t, y)
            with Initial Conditions
                        t in [a, b]
                        y(a) = alpha

    Function Name: RKF_method.


    Inputs: f: defining function ( from y' = f(t,y) )
            a: starting Time
            b: ending Time
            alpha: Initial Condition y(a)
            tol: Tolerance (The amount of error we are willing to tolerate)
            hmax: maximum step size (the biggest step size we are willing to have)
            hmin: minimum step size (a lower bound on step-size to save computational cost)

    Outputs: y: solution array
             t: time array
             step: stepsize array

'''

def RKF_method(f, a, b, alpha, tol, hmax, hmin):
    t = a
    w = alpha
    h = hmax
    flag = 1

    time = [a] #time array
    y = [alpha] #solution array
    step = [hmax] #step size array
    while(flag == 1):
        #compute RK-6 approximation of the solution
        k1 = h*f(t, w)
        k2 = h*f(t + (1/4)*h, w + (1/4)*k1)
        k3 = h*f(t + (3/8)*h, w + (3/32)*k1 + (9/32)*k2)
        k4 = h*f(t + (12/13)*h, w + (1932/2197)*k1 - (7200/2197)*k2 + (7296/2197)*k3)
        k5 = h*f(t + h, w + (439/216)*k1 - (8)*k2 + (3680/513)*k3 - (845/4104)*k4)
        k6 = h*f(t + (1/2)*h, w - (8/27)*k1 + (2)*k2 - (3544/2565)*k3 + (1859/4104)*k4 - (11/40)*k5)
        R = (1/h)*np.abs((1/360)*k1 - (128/4275)*k3 - (2197/75240)*k4 + (1/50)*k5 + (2/55)*k6)

        #condition the explicit RK-6 solution to the tolerance

        if(R <= tol):
            #solution accepted!!
            t += h
            w += (25/216)*k1 + (1408/2565)*k3 + (2197/4104)*k4 - (1/5)*k5
            time.append(t) #update time
            step.append(h) #update step-size
            y.append(w) #update solution

        #Check to see if the step size needs to change
        delta = 0.84*(tol/R)**(1/4) #compute an optimal bound for h
        if(delta <= 0.1):
            h = 0.1*h #delta serves as lower bound if delta <=0.1
            step.append(h)
        elif(delta >= 4):
            h = 4*h #delta serves as upper bound if delta >= 4
            step.append(h)
        else:
            h = delta*h #other-wise delta is the optimal scaling factor
            step.append(h)

        #housekeeping for step-size
        if(h > hmax):
            h = hmax
            step[-1] = h
        if(t >= b):
            flag = 0
        elif(t + h > b):
            h = b - t
            step[-1] = h
        #if computation forces h below the minimum threshold, we abandon problem
        elif(h<hmin):
            flag = 0
            print("min h exceeded")
            print("completed unsuccessfully")

    return y, time, step
