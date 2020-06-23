# setting output to file of size 800 x 800
#set terminal pngcairo size 1000, 1000 
#set output 'out.png'
set terminal epslatex standalone size 12cm,10cm color
set output "rgb.tex"

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
set cbtics ("0" 0, '2$\pi$' 2*3.14159)
set title "Domain coloring of output" 

#set palette defined (0 'black', 1 'blue')
#set cblabel 'Complex Magnitude $(r)$' offset 1, 0
#set cbrange [ 0 : 8 ]
set palette defined (0 'black', 1 'red')
set cblabel 'Phase Angle $(\theta)$' 
set cbrange [ 0 : 2*3.14159 ]

# use hsv for colorbar and set palette to use full hsv space

# setting isosamples for output grid and samples for input grid
set isosamples 1000, 1000
set samples 1000, 1000

# setting functions necessary for domain coloring
# setting threshold for gridlines. Smaller threshold will make smaller lines
thresh = 0.1
f(z) = z
#f(z) = z**2

rgb(r,g,b) = 65536 * int(r) + 256 * int(g) + int(b)
angle(x,y) = (pi + atan2(-y,-x)) / (2*pi)
r(x,y) = sqrt(x*x + y*y)
theta(x,y) = atan2(y,x)
z(x,y) = r(x,y)*exp(theta(x,y)*sqrt(-1))
ip(x,y) = imag(z(x,y))
rp(x,y) = real(z(x,y))
if(z) = imag(f(z))
rf(z) = real(f(z))
color(x,y) = rgb(angle(rf(z(x,y)), if(z(x,y)))*255, 0, abs(f(z(x,y)))*255/sqrt(8))
save_encoding = "utf8"

# Last datafile plotted: "++"
# In this case, it means, "plot the data file created with the
#                          samples and isosamples"
splot '++' using 1:2:(color($1,$2)) with pm3d lc rgb variable
