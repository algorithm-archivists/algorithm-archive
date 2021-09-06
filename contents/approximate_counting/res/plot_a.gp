set terminal epslatex standalone color

set output "a_change.tex"
#set size square

set title '$n_v$ vs $a$ for $v=255$'

set xlabel '$a$'
set xtics ("25" 0, "27.5" 5, "30" 10, "32.5" 15, "35" 20)

set ylabel '$n_v (\times 10^5)$'
set ytics ("0" 0, "1" 100000, "2" 200000, "3" 300000, "4" 400000, "5" 500000)

plot for [i=1:10] "a_change.dat" w l lw 3 title ""

