
# Overview

<!-- badges: start -->
<!-- badges: end -->

The goal of `carsimr.geisler` is to create a simulation of blue and red car 
movement on a grid. At odd iterations, blue cars will move up a space so long as
another car is not in the way. Likewise, red cars will move to the right on even
iterations if they are not blocked. 

Cars can also wrap around the grid! If a blue car makes it to the first row and
the bottom space of its column is free, it will move back to the bottom at the 
next iteration. If a red car is located in the last column and the first element
of its same row is blank, then that red car will move back to the first column.

Matrices will be generated in a list to show the succession of car movements.
However, it will be more visually appealing for the user to plot these 
movements. Luckily, a plot function was created for the specific class of these
matrices and cars have been color-coded appropriately. A list of plots will be
generated and car movement will be shown as a grid.

Lastly, the `track_blocks()` function was created to show the user how cars can 
become either more free or more blocked over many trials, depending on how full
the initial grid was at the start. This function will return a vector with the
proportion of blocked cars at each iteration.

## Installation

You can install the released version of carsimr.geisler from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("carsimr.geisler")
```

## Helpful Tips

To ensure that your example is reproduceable, make sure to generate a random 
seed using the `set.seed()` function. Car generation and allocation is based on
a Bernoulli random number generator and thus will be different every time! This 
should be typed on the line above your `move_cars()` function.

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(carsimr.geisler)
set.seed(774)
first_simulation <- move_cars(rho = 0.4, r = 10, c = 15, p = 0.5, trials = 10)
plot(first_simulation)

track_blocks(first_simulation)
```

