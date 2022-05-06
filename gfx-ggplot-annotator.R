
suppressPackageStartupMessages({
  library(ggplot2)
  library(purrr)
  library(grid)
  library(eventloop)
})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Allow the user to draw an annotation on top of a ggplot
#'
#' @param p ggplot2 object
#' @return line information as a list with `x`, `y` and `idx` suitable
#'         for plotting with `grid.polylines()`
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
create_annotation <- function(p) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Set up the global variables which store the state of the world
  #  'drawing'  Currently drawing?
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  drawing <- FALSE
  last_x  <- NA
  last_y  <- NA

  xs <- c()
  ys <- c()

  all_xs <- list()
  all_ys <- list()


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #' The main 'draw' function called within the eventloop
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  draw <- function(event, mouse_x, mouse_y, ...) {

    # Process events
    if (!is.null(event)) {
      if (event$type == 'mouse_down') {
        drawing <<- TRUE
      } else if (event$type == 'mouse_up') {
        drawing <<- FALSE
        last_x <<- NA
        last_y <<- NA

        all_xs <<- c(all_xs, list(xs))
        all_ys <<- c(all_ys, list(ys))
        xs <<- c()
        ys <<- c()

      } else if (event$type == 'key_press' && event$str == ' ') {
        grid::grid.rect(gp = gpar(col=NA, fill='white')) # clear screen
        last_x <<- NA
        last_y <<- NA
      }  else if (event$type == 'key_press' && event$str == 's') {
        png_file <- tempfile(tmpdir = ".", fileext = ".png")
        message("saving: ", png_file)
        grDevices::savePlot(filename = png_file)
      }
    }

    # If drawing mode is active, draw from the last point to this point.
    if (drawing) {

      xs <<- c(xs, mouse_x)
      ys <<- c(ys, mouse_y)

      if (!is.na(last_x)){
        grid::grid.lines(
          x = c(last_x, mouse_x),
          y = c(last_y, mouse_y),
          gp = gpar(col = 'red')
        )
      }

      last_x <<- mouse_x
      last_y <<- mouse_y
    }
  }

  # Init the eventloop by adding the ggplot to it
  init <- function() {
    plot(p)
  }

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Run the eventloop to get the annotation from the users mouse drawings
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  eventloop::run_loop(draw, init_func = init, width = 12, height = 8, fps_target = NA, double_buffer = FALSE)


  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Create the annotation information
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  list(
    x = unlist(all_xs),
    y = unlist(all_ys),
    idx  = rep(seq_along(all_xs), lengths(all_xs))
  )
}



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create a plot
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
p <- ggplot(mtcars) +
  geom_point(aes(mpg, wt))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create an annotation by letting the user draw on it.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
annotation <- create_annotation(p)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save the plot and draw the annotation over the top
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
pdf("ggplot.pdf", width = 10, height = 7)
plot(p)
grid.polyline(annotation$x, annotation$y, id = annotation$idx, gp = gpar(col='red'))
dev.off()










