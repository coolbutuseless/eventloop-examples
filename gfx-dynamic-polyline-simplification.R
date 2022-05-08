
# remotes::install_github("coolbutuseless/visvalingam")
library(visvalingam)
library(rworldmap)
library(rworldxtra)
library(grid)

library(eventloop)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract map of mainland Australia
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
map       <- rworldmap::getMap(resolution = 'high')
australia <- map@polygons[[which(map$ADMIN.1 == 'Australia')]]
mainland  <- australia@Polygons[[1]]@coords
mainland  <- scale(mainland) / 5 + 0.5


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pre-calculate effective areas for Visvalingam simplification
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
areas <- visvalingam::vis_effective_areas(mainland[,1], mainland[,2])


render <- function(mouse_x, ...) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Threshold for filtering is controlled by mouse 'x' position
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  probs <- mouse_x
  if (probs < 0) {
    probs <- 0
  } else if (probs > 0.99) {
    probs <- 0.99
  }
  probs <- probs ^ 0.1

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Filter data to keep simplified points at this threshold
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  threshold  <- quantile(areas, probs)
  simplified <- mainland[areas > threshold,]

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Clear screen and plot latest simplified map
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  grid::grid.rect(gp = gpar(fill = 'white'))
  grid::grid.lines(simplified[,1], simplified[,2])
  label <- sprintf("Removed: %4.1f%%", probs * 100)
  grid::grid.text(label, 0, 0, just = c(-0.1, -0.1), gp = gpar(cex = 4))

}


eventloop::run_loop(render)
