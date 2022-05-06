library(eventloop)
library(grid)
library(viridisLite)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Global data encapsulating the state of the board
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
N <- 33
r <- 1/N/2
decay    <- 0.004
min_size <- 0.1
points <- expand.grid(
  x = seq(N)/N - r,
  y = seq(N)/N - r,
  r = r,
  scale = min_size
)

points$fill <- viridisLite::magma(nrow(points))
scale <- rep(min_size, nrow(points))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Callback function
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
canvas <- function(event, mouse_x, mouse_y, ...) {

  # Highlight points near mouse
  near <- (abs(points$x - mouse_x)+ abs(points$y - mouse_y)) < (1.4 * r)
  scale[near] <<- 1

  # Draw all the points
  grid::grid.rect(gp = grid::gpar(fill = 'white'))
  grid::grid.rect(
    x = points$x,
    y = points$y,
    width  = 2 * points$r * scale,
    height = 2 * points$r * scale,
    gp = grid::gpar(fill = points$fill, col = NA)
  )

  # decay the size of each point
  scale <<- pmax(scale - decay, min_size)
}



run_loop(canvas, width = 10, height = 10, fps_target = 30)
