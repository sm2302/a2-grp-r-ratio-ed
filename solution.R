library(tidyverse)
library(ggforce)
theme_set(theme_void()) 

x0=0; y0=0; #center of disk
r=1; #disk radius
numbLines=100;#number of lines

# Method 1..........................................
thetaA1=2*pi*runif(numbLines); #choose angular component uniformly
thetaA2=2*pi*runif(numbLines); #choose angular component uniformly

# Coordinates of equilateral triangle
eqtri_df <- tibble(
  x    = c(0, sqrt(3) / 2, -sqrt(3) / 2),
  y    = c(1, -0.5, -0.5),
  xend = c(sqrt(3) / 2, -sqrt(3) / 2, 0),
  yend = c(-0.5, -0.5, 1)
)

# Coordinates of random chord
rdmchr_df <- tibble(
  x    = x0+r*cos(thetaA1),
  y    = y0+r*sin(thetaA1),
  xend = x0+r*cos(thetaA2),
  yend = y0+r*sin(thetaA2)
)
# calculate probability that the chord is longer than a side of the triangle


# Plot
p <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = rdmchr_df, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

ggsave(p, file = "plot.png", height = 5, width = 7)
