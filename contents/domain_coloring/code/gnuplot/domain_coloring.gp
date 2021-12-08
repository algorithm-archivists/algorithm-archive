# setting output to file of size 800 x 800
set terminal pngcairo size 1000, 1000 
set output 'domain.png'

# sets title for full plot
set title 'f(z)=z^2'

# removes legend
unset key

# projects image onto 2D plane
set view map

# sets aspect ratio of plot to be square 
set size square

# sets x and y range and labels
set xrange[-2:2]
set yrange[-2:2]

set xlabel "Re(z)"
set ylabel "Im(z)"

# scaling the x, y, and colorbar tics to zero so they are not seen in the plot
set xtics border scale 0,0
set ytics border scale 0,0
set cbtics border scale 0,0

# sets tics in color bar at 0 and 2pi
set cbtics ("0" -3.14159, '2pi' 3.14159)

set cblabel "Phase Angle" 
set cbrange [ -3.14159 : 3.14159 ]

# use hsv for colorbar and set palette to use full hsv space
set palette model HSV
set palette defined ( 0 0 1 1, 1 1 1 1 )

# setting isosamples for output grid and samples for input grid
set isosamples 2000, 2000
set samples 2000, 2000

# setting functions necessary for domain coloring
# setting threshold for gridlines. Smaller threshold will make smaller lines
thresh = 0.1
f(z) = z**2

# atan2 returns a range from -pi to pi, so we need to add pi, but this offsets
# the value by 180 degrees, so we also imput (-y, -x) for another 180 degrees
# to invert rotation
angle(x,y) = (pi + atan2(-y,-x)) / (2*pi)

# complex magnitude
r(x,y) = sqrt(x*x + y*y)

# complex phase and magnitude
theta(x,y) = atan2(y,x)
z(x,y) = r(x,y)*exp(theta(x,y)*sqrt(-1))

# imaginary and real output functions
imaginary_f(z) = imag(f(z))
real_f(z) = real(f(z))

# magnitude contours
magnitude_shading(x,y) = 0.5 + 0.5*(abs(f(z(x,y)))-floor(abs(f(z(x,y)))))

# gridlines
gridlines(x,y) = (abs(sin(real_f(z(x,y))*pi)**thresh) \
                  * abs(sin(imaginary_f(z(x,y))*pi))**thresh)

# overall coloring function
color(x,y) = hsv2rgb(angle(real_f(z(x,y)), imaginary_f(z(x,y))), \
                     magnitude_shading(x,y), \
                     gridlines(x,y))

save_encoding = "utf8"

# Last datafile plotted: "++"
# In this case, it means, "plot the data file created with the
#                          samples and isosamples"
splot '++' using 1:2:(color($1,$2)) with pm3d lc rgb variable nocontour
