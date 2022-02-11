set terminal pngcairo size 640, 480
set output "sine_cosine_plot.png"
set xrange [0:10]
set yrange [0:1]
set logscale x
set logscale y
set key at 1,0.5
set title "Gnuplot Test"
set size square

plot sin(x) with lines dashtype 2 linecolor rgb "black" title "sin(x)", \
     cos(x) w p pt 17 lc rgb "purple" t "cos(x)"
