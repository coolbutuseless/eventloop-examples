library(eventloop)
library(grid)


xs <- runif(1000)
ys <- runif(1000)


starfield <- function(event, mouse_x, mouse_y, frame_num, fps_actual,
                      fps_target, dev_width, dev_height, ...) {

  # Set the centre for expansion/rotation to be the current mouse (x,y)
  # could just set to centre to (0.5, 0.5) for non-interactive case
  xcentre <- mouse_x
  ycentre <- mouse_y

  # Expand points around centre
  xt <- (xs - xcentre) * 1.03
  yt <- (ys - ycentre) * 1.03

  # Rotate points around centre
  theta <- 1.5 * pi/180
  xs <<- xt * cos(theta) - yt * sin(theta) + xcentre
  ys <<- xt * sin(theta) + yt * cos(theta) + ycentre

  # Replace any out-of-bound points with new random points
  out_of_bounds <- which(xs < 0 | xs > 1 | ys < 0 | ys > 1)
  N <- length(out_of_bounds)
  if (N > 0) {
    xs[out_of_bounds] <<- runif(N, xcentre - 0.05, xcentre + 0.05)
    ys[out_of_bounds] <<- runif(N, ycentre - 0.05, ycentre + 0.05)
  }

  # Clear screen, plot points, add in FPS info in bottom corner
  grid::grid.rect(gp = grid::gpar(fill = 'white'))
  grid::grid.points(
    x = grid::unit(xs, 'npc'),
    y = grid::unit(ys, 'npc'),
    pch = '+'
  )
}


run_loop(starfield, fps_target = NA)
