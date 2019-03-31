# Plotting

Plotting is an essential tool for visualizing and understanding important details of several algorithms and methods and is necessary for studies in various areas of computational science.
For many languages, such as python, julia, and matlab, it is relatively straightforward to create simple plots for various types of data; however, for several other languages, like fortran, C/C++, and java, plotting can be a chore.
Because the Algorithm Archive strives to be language agnostic, we do not want to favor any particular set of languages and have decided instead to output all data that needs plotting into a file format that cen easily be read in my various plotting scripts separate from the algorithm implementations.

If you are implementing any algorithm in a language found on this page, you should be able to modify your existing code to allow for on-the-fly plotting.
Otherwise, please use the language of your choice to write the initial language implementation and output the data to a file before using one of the scripts available here for plotting.

This chapter aims to explain how to plot several different types of data and will be updated as more algorithms require more complex plotting schemes.
Though many complex file formats exist, we will be mainly storing data for plotting in simple ASCII text.
If you wish to use these plotting scripts for other file formats or projects unrelated to the Algorithm Archive, some modification will be necessary.
In addition, each plotting language used in this chapter will likely have many features we are not currently using, so there may be methods to create stunning visualizations that we are ignoring here.

## Plotting a series of functions

In some sense, this chapter is meant as a guide to help better understand plotting with your language of choice.
As such, it is important to first understand how to perform a few basic tasks:

1. Changing the plot title, axis, labels, x/ytic values, and plot dimensions
2. Plotting multiple functions at the same time
3. Outputting the plot to file

### Changing aux features

### Plotting multiple functions

### Outputting the plot to file

From here, all scripts will output an image directly to your computer screen; however, it is important to note that you can use any of the above methods to modify the output or output to a file, if you wish.

## Plotting data from a file 

Each algorithm in the Algorithm Archive that requires plotting will also output a data file to use for this purpose.

### One-dimensional output
In the case of one-dimensional output, the data file is a `.csv` file with numbers separated by a newline.

### Two-dimensional output
In the case of two-dimensional output, the data file will again be a `.csv`, but this time, each number will be separated by a comma for different columns entries and a newline for rows.
It is expected that the numbers of columns does not vary in each row and that we are working with an $$n \times m$$ matrix which can be simply plotted as a series of pixels that scale in color according to some defined colorbar.

#### changing the colorbar

### Algorithms using this method:

## Scatter plot

The scatterplot is another method for plotting data that plots each point in an $$n$$-dimensional space.
For the purposes of the Algorithm Archive, this space is mainly two-dimensional; however, scatterplots in 3D can may also be used for visualizing three-dimensional datasets, such as those requiring octrees.

### Algorithms using this method:

- Monte carlo

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Images/Graphics

##### Pull Requests

The following pull requests have modified the text or graphics of this chapter:
- none
