# This is the size of each bin
bin_width = 1;

# This takes the data and determins which bin id it should fall into
bin_id(x) = floor(x/bin_width)

# This modifies each bin to be the correct width and also centers it over the 
#     correct number
bin(x) = bin_width * ( bin_id(x) + 0.5 )

# Starts the y-axis at 0
set yrange [0:]

# Removes legend
unset key

# Sets a fill style for "fillsteps"
set style fill solid 1.00 border

# The column number to be histogrammed is 1, change $1 to another number if 
#     you want to plot another column
plot '../../data/rand.dat' u (bin($1)):(1) t 'data' smooth frequency w fillsteps
