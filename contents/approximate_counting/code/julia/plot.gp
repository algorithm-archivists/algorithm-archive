set terminal epslatex standalone color

set output "check.tex"
#set size square

set title "Approximate counting of 1,000,000 items"

set xlabel 'True number of items $\left( \times 10^{5} \right)$'
set xtics ("0" 0, "2" 200000, "4" 400000, "6" 600000, "8" 800000, "10" 1000000)

set ylabel 'Approximate count $\left( \times 10^{5} \right)$'
set ytics ("0" 0, "2" 200000, "4" 400000, "6" 600000, "8" 800000, "10" 1000000)

plot for [i=1:10] "out.dat" u 0:i w l lw 3 title ""

