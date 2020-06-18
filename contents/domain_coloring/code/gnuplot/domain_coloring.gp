# setting output to file of size 800 x 800
#set terminal pngcairo size 1000, 1000 
#set output 'out_2.png'
set terminal epslatex standalone size 12cm,10cm color
set output "hsv.tex"

# removes legend
unset key

# projects image onto 2D plane
set view map

# sets aspect ratio of plot to be square 
set size square

# setting x and y range
set xrange[-2:2]
set yrange[-2:2]

set xlabel "Re$(x)$"
set ylabel "Im$(x)$"

# scaling the x, y, and colorbar tics to zero so they are not seen in the plot
set xtics border scale 0,0
set ytics border scale 0,0
set cbtics border scale 0,0

# setting tics in color bar at 0 and 2pi
set cbtics ("0" -3.14159, '2$\pi$' 3.14159)
set title "Domain coloring of output" 

set cblabel "Phase Angle" 
set cbrange [ -3.14159 : 3.14159 ]

# use hsv for colorbar and set palette to use full hsv space
set palette model HSV
set palette defined ( 0 0 1 1, 1 1 1 1 )

# setting isosamples for output grid and samples for input grid
set isosamples 1000, 1000
set samples 1000, 1000

# setting functions necessary for domain coloring
# setting threshold for gridlines. Smaller threshold will make smaller lines
thresh = 0.1
f(z) = z**2
#f(z) = z**3-1
#f(z) = z**5 + 3*z**4 + 2*z + 1
#f(z) = z - z**3/3! + z**5/5!
#f(z) = 1-sin(z)
angle(x,y) = (pi + atan2(-y,-x)) / (2*pi)
r(x,y) = sqrt(x*x + y*y)
theta(x,y) = atan2(x,y)
z(x,y) = r(x,y)*exp(theta(x,y)*sqrt(-1))
ip(x,y) = imag(z(x,y))
rp(x,y) = real(z(x,y))
if(z) = imag(f(z))
rf(z) = real(f(z))
val(x,y) = 0.5 + 0.5*(abs(f(z(x,y)))-floor(abs(f(z(x,y)))))
color(x,y) = hsv2rgb(angle(rf(z(x,y)), if(z(x,y))), abs(f(z(x,y)))/8, 1)
#color(x,y) = hsv2rgb(angle(rf(z(x,y)), if(z(x,y))), abs(f(z(x,y))), val(x,y)*shade(x,y))
#color(x,y) = hsv2rgb(angle(rp(x,y), ip(x,y)), abs(z(x,y)), val(x,y)*shade(x,y))
#shade(x,y) = (abs(sin(rp(x,y)*pi)**thresh) * abs(sin(ip(x,y)*pi))**thresh)
shade(x,y) = (abs(sin(rf(z(x,y))*pi)**thresh) * abs(sin(if(z(x,y))*pi))**thresh)
save_encoding = "utf8"

# Last datafile plotted: "++"
# In this case, it means, "plot the data file created with the
#                          samples and isosamples"
splot '++' using 1:2:(color($1,$2)) with pm3d lc rgb variable nocontour
