set terminal epslatex standalone color

#set output "hist_1.tex"
#set output "hist_2.tex"
set output "hist_3.tex"

set size square

#set title "True count of 10,000"
#set title "True count of 500,000"
set title "True count of 1,000,000"

#set xlabel ''
#set xrange [0:40000]
#set xtics ("0" 0, "20,000" 20000, "40,000" 40000)

#set xrange [352000:644000]
#set xrange[10000:310000]
#set xtics ("350,000" 10000, "500,000" 160000, "650,000" 310000)

#set xrange [808000:1240000]
set xrange[-10000:490000]
set xtics ("750,0000" -10000, "1,000,000" 240000, "1,250,000" 490000)

#set ylabel 'Approximate count $\left( \times 10^{5} \right)$'
#set ytics ("0" 0, "2" 200000, "4" 400000, "6" 600000, "8" 800000, "10" 1000000)

#plot "histogram_1.dat" w l lw 10 title ""
#plot "histogram_2.dat" w l lw 10 title ""
plot "histogram_3.dat" w l lw 10 title ""

