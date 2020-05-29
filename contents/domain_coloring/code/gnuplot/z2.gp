set terminal epslatex standalone size 10cm,10cm
set output "z2.tex"

set size square

set xlabel "$z$"
set ylabel "$f(z)$"

plot x**2 w l lw 4 title "$z^2$"
