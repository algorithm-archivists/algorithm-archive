set terminal epslatex standalone size 12cm,10cm color
set output "absz.tex"
#set terminal pngcairo
#set output "absz.png"

set size square
set view map

set samples 1000
set isosamples 1000

set xrange[-1:1]
set yrange[-1:1]

set xlabel "Re$(z)$"
set ylabel "Im$(z)$"
set cblabel "$|z|$"

# palette
set palette defined ( 0 '#2166AC',\
                      1 '#4393C3',\
                      2 '#92C5DE',\
                      3 '#D1E5F0',\
                      4 '#FDDBC7',\
                      5 '#F4A582',\
                      6 '#D6604D',\
                      7 '#B2182B')
set palette defined (0 '#000000', 1 '#FFFFFF')

cabs(x,y) = sqrt(x**2 + y**2)

splot "++" u 1:2:(cabs($1, $2)) with pm3d notitle
