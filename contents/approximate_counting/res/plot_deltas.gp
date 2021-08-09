set terminal epslatex standalone color

set output "deltas.tex"
#set size square

set title '$\Delta$ vs $v$'

set xlabel '$v$'

set ylabel '$\Delta$ (logscale)'
set logscale y

plot for [i=1:10] "deltas.dat" w l lw 3 title ""

