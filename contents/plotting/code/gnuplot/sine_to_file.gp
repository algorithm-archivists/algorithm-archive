set terminal pngcairo
set output "sine_cosone_plot.png"
set xrange [0:10]
set yrange [0:1]

plot sin(x) with lines, cos(x) with lines
