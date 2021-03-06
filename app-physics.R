
library(grid)
library(eventloop)

# remotes::install_github("coolbutuseless/chipmunkcore")
# remotes::install_github("coolbutuseless/chipmunkbasic")
library(chipmunkcore)
library(chipmunkbasic)


set.seed(1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Initialize a simulation space
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cm <- Chipmunk$new()

scale <- 100

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add fixed segments
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
cm$add_static_segment(0.1 * scale, 0.5 * scale ,   0.6 * scale, 0.3  * scale, friction = 0.2, elasticity = 1)
cm$add_static_segment(0.9 * scale, 0.35 * scale,   0.4 * scale, 0.05 * scale, friction = 0.2, elasticity = 1)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Fetch all the segments. Use for plotting
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
segments_df <- cm$get_static_segments()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add some circles
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
for (i in 1:10) {
  cm$add_circle(
    x  = runif(1) * scale,
    y  = runif(1) * scale,
    vx = 5 * rnorm(1),
    vy = 5 * rnorm(1)
  )
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Callback function run within loop
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sim <- function(event, mouse_x, mouse_y, ...) {
  circles <- cm$get_circles()
  cm$advance(1)

  if (!is.null(event)) {
    if (event$type == 'mouse_down') {
      cm$add_circle(x = mouse_x * 100, y = mouse_y * 100)
    }
  }

  grid::grid.rect(gp = gpar(fill = 'white'))

  grid::grid.lines(
    x = c(0.1, 0.6),
    y = c(0.5, 0.3)
  )

  grid::grid.lines(
    x = c(0.9, 0.4),
    y = c(0.35, 0.05)
  )

  grid::grid.circle(
    x = circles$x / scale,
    y = circles$y / scale,
    r = unit(2, 'mm'),
    gp = gpar(fill = rainbow(nrow(circles)), col = NA)
  )
}



eventloop::run_loop(sim, width = 7, height = 7, show_fps = TRUE, fps_target = 30)
