library(grid)
library(eventloop)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Set up the global variables which store the state of the world
#  N       size of grid
#  canvas  the actual canvas (an integer matrix)
#  pen     the current pen state. Use 'NA' to indicate "not drawing"
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
N      <- 8
canvas <- matrix(1L, N, N)
pen    <- NA_integer_


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' The main 'draw' function called within the eventloop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
draw <- function(event, mouse_x, mouse_y, ...) {

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Process events
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.null(event)) {
    if (event$type == 'mouse_down') {
      if (event$button == 0) {
        pen <<- 0L
      } else if (event$button == 2) {
        pen <<- 1L
      }
    } else if (event$type == 'mouse_up') {
      pen <<- NA_integer_
    }

    if (event$type == 'key_press' && event$str == ' ') {
      canvas <<- matrix(1L, N, N)
      grid::grid.raster(canvas, interpolate = FALSE)
    }
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # If the pen is currently active, then draw on the canvas and display
  # the latest version.
  # Note that graphics coordiates are from bottom-left of screen, while
  # matrix coordinates are from top-left.  So the y-axis must be inverted
  # to set a matrix location from a mouse position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (!is.na(pen)) {
    col <-       round(mouse_x * N + 0.5)
    row <- N+1 - round(mouse_y * N + 0.5)

    canvas[row, col] <<- pen
    grid::grid.raster(canvas, interpolate = FALSE)
  }
}



eventloop::run_loop(draw, double_buffer = FALSE)
