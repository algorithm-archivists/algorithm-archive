set terminal pngcairo
set size square

set output "cartesian_grid.png"
p "cartesian_grid.dat" pt 7 title ""

set output "cartesian_rand.png"
p "cartesian_rand.dat" pt 7 title ""

set output "polar_rand.png"
p "polar_rand.dat" pt 7 title ""

set output "polar_grid.png"
p "polar_grid.dat" pt 7 title ""


set xrange[-3:3]
set yrange[-3:3]

set output "polar_grid_output.png"
p "polar_grid_output.dat" pt 7 title ""

set output "polar_rand_output.png"
p "polar_rand_output.dat" pt 7 title ""

set output "cartesian_rand_output.png"
p "cartesian_rand_output.dat" pt 7 title ""

set output "cartesian_grid_output.png"
p "cartesian_grid_output.dat" pt 7 title ""

