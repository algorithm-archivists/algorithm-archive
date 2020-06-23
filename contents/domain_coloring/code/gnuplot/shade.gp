# setting output to file of size 800 x 800
#set terminal pngcairo size 1000, 1000 
#set output 'out_shade.png'
set terminal epslatex standalone size 12cm,10cm color
set output "shade.tex"

# removes legend
unset key

# sets aspect ratio of plot to be square 
set size square

# setting x and y range
set xrange[-4:4]
set yrange[-0:1]

set xlabel "z"
set ylabel '$h(z)$'

# scaling the x, y, and colorbar tics to zero so they are not seen in the plot
set xtics border scale 0,0
set ytics border scale 0,0
set cbtics border scale 0,0

# setting tics in color bar at 0 and 2pi
set cbtics ("0" -3.14159, '2$\pi$' 3.14159)
set title '$h(z)$'

set cblabel "Phase Angle" 
set cbrange [ -3.14159 : 3.14159 ]

# use hsv for colorbar and set palette to use full hsv space
set palette model HSV
set palette defined ( 0 0 1 1, 1 1 1 1 )

# setting isosamples for output grid and samples for input grid
set isosamples 2000, 2000
set samples 2000, 2000

thresh = 0.1

shade(x) = (abs(sin(x*pi))**thresh)

# Last datafile plotted: "++"
# In this case, it means, "plot the data file created with the
#                          samples and isosamples"
plot '++' using 1:(shade($1)) with lines lw 2
