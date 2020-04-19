# use like: gnuplot -e "folder='/path/to/data/directory'" plot_output.plt

set terminal gif animate delay 10
set output folder.'/wavefunction.gif'

set key off

set xrange [-10:10] 
set yrange [0:1]
set y2range [0:50]

set ytics nomirror autofreq tc lt 1
set ylabel 'Psi(x)' tc lt 1

set y2tics nomirror autofreq tc lt 2
set y2label 'V(x)' tc lt 2

list = system('ls '.folder.'/output*.dat')

do for [file in list] {
    plot file u 1:2 smooth csplines, \
         file u 1:3 smooth csplines axes x1y2
}

