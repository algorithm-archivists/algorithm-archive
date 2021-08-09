set terminal epslatex standalone color

#set output "histexp_1.tex"
#set output "histexp_2.tex"
set output "histexp_3.tex"

set size square

#set title "True count of 10,000"
#set title "True count of 500,000"
set title "True count of 1,000,000"

set xlabel ''
#set xrange [-255:9745]
#set xtics ("6000" -255, "10,000" 3745, "16,000" 9745)

#set xrange [352000:644000]
#set xrange[-1458:538542]
#set xtics ("320,000" -1458, "500,000" 178542, "860,000" 538542)

#set xrange [808000:1240000]
set xrange[-19374:1100626]
set xtics ("600,0000" -19374, "1,000,000" 380626, "1,720,000" 1100626)

#set ylabel 'Approximate count $\left( \times 10^{5} \right)$'
#set ytics ("0" 0, "2" 200000, "4" 400000, "6" 600000, "8" 800000, "10" 1000000)

#plot "histogram_1exp.dat" w l lw 10 title ""
#plot "histogram_2exp.dat" w l lw 10 title ""
plot "histogram_3exp.dat" w l lw 10 title ""

