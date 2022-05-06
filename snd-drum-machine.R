library(drumr)
library(eventloop)

drum <- matrix(c('kick', 'snare', 'hihat', 'crash'), 2,2 )
kit  <- 'acoustic'

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Draw the drum kit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
init <- function() {
  grid.raster(matrix(rainbow(4), 2, 2), interpolate = FALSE)
  grid.text('kick' , 0.25, 0.25)
  grid.text('snare', 0.75, 0.25)
  grid.text('hihat', 0.25, 0.75)
  grid.text('crash', 0.75, 0.75)
  grid.rect(0, 0, 0.22, 0.05, just = c(0, 0), gp = gpar(fill = 'white'))
  grid.text(kit, 0, 0, just = c(-0.15, -0.15), gp = gpar(cex = 2))
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#  Click mouse to play a beat
#  Press 1-6 to change the kit
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
drum_machine <- function(event, mouse_x, mouse_y, ...) {
  if (!is.null(event)) {
    if (event$type == 'mouse_down') {
      x <- ceiling(mouse_x * 2)
      y <- ceiling(mouse_y * 2)
      drumr::beat(drum = drum[x, y], kit = kit)
    } else if (event$type == 'key_press') {
      kit <<- switch(
        event$str,
        '1' = 'acoustic',
        '2' = 'hiphop',
        '3' = 'electro',
        '4' = 'beatbox',
        '5' = 'world',
        '6' = 'r2d2',
        'acoustic'
      )
    }
    grid.rect(0, 0, 0.22, 0.05, just = c(0, 0), gp = gpar(fill = 'white'))
    grid.text(kit, 0, 0, just = c(-0.15, -0.15), gp = gpar(cex = 2))
  }
}


eventloop::run_loop(drum_machine, init_func = init, double_buffer = FALSE)
