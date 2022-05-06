
library(grid)
library(eventloop)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define the colour matrix to pick from
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
N <- 10
col_mat <- matrix(rainbow(N*N), N, N)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Init function will display the colour matrix
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
init <- function() {
  grid.raster(col_mat, interpolate = FALSE)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Callback function run continously in the loop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pick_colour <- function(event, mouse_x, mouse_y, ...) {
  if (!is.null(event) && event$type == 'mouse_down') {
    x <-     ceiling(mouse_x * N)
    y <- N - ceiling(mouse_y * N) + 1 # Invert Y axis
    cat(col_mat[y, x], "\n")
  }
}


eventloop::run_loop(pick_colour, init_func = init)
