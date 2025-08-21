## code to prepare `DATASET` dataset goes here

library(grid)
library(ggplot2)
library(leopard)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Open the device, draw the plot, call dev.off()
# Make sure to assign the value returned when opening the device
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
f <- leopard(verbosity = 0, height = 5)
ggplot(mtcars) + 
  geom_point(aes(mpg, wt)) +
  labs(title = "Demo plot")
invisible(dev.off())

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# The value returned when opening the device is a function.
# Call this function to get a list of data.frames
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
leopard <- f()


usethis::use_data(leopard, internal = TRUE, overwrite = TRUE)
