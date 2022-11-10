library(tidyverse)
library(ggforce)
theme_set(theme_void()) 

x0=0; y0=0; #center of disk
r=1; #radius
numbLines=100; #number of lines

# Method A..........................................
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
rdmchr_A <- tibble(
  x    = x0+r*cos(thetaA1),
  y    = y0+r*sin(thetaA1),
  xend = x0+r*cos(thetaA2),
  yend = y0+r*sin(thetaA2)
)
# calculate probability that the chord is longer than a side of the triangle


# Plot
p1 <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = rdmchr_A, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

ggsave(p1, file = "plotA.png", height = 5, width = 7)

# Method B............................................................................
thetaB=2*pi*runif(numbLines); #choose angular component uniformly
pB=r*runif(numbLines); #choose radial component uniformly
qB=sqrt(r^2-pB^2); #distance to circle edge (along line)

#calculate trig values
sin_thetaB=sin(thetaB);
cos_thetaB=cos(thetaB);
# Coordinates of random chord
#calculate segment endpoints

rdmchr_B <- tibble(
  x    = x0+pB*cos_thetaB+qB*sin_thetaB,
  y    = y0+pB*sin_thetaB-qB*cos_thetaB,
  xend = x0+pB*cos_thetaB-qB*sin_thetaB,
  yend = y0+pB*sin_thetaB+qB*cos_thetaB
)

# calculate probability that the chord is longer than a side of the triangle

# Plot
p2 <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "gray50") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend)) +
  geom_segment(data = rdmchr_B, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

ggsave(p2, file = "plotB.png", height = 5, width = 7)

