set terminal epslatex standalone color
#set terminal pdf enhanced
#set style fill transparent solid 0.3

#set output "check.tex"
set output "checkexp.tex"
#set size square

set title "Approximate counting of 1,000,000 items"

set xlabel 'True number of items $\left( \times 10^{5} \right)$'
set xtics ("0" 0, "2" 200000, "4" 400000, "6" 600000, "8" 800000, "10" 1000000)

set ylabel 'Approximate count $\left( \times 10^{5} \right)$'
set ytics ("0" 0, "2" 200000, "4" 400000, "6" 600000, "8" 800000, "10" 1000000, "12" 1200000, "14" 1400000)

#plot  "extremes.dat" u 0:1:2 w filledcu lc "gray" title "", for [i=1:10] "out.dat" u 0:i w l lw 3 title "", x w l lw 3 dt 3 lc "black" title ""
plot  "extremesexp.dat" u 0:1:2 w filledcu lc "gray" title "", for [i=1:10] "outexp.dat" u 0:i w l lw 3 title "", x w l lw 3 dt 3 lc "black" title ""

