# The value of probabilities of chords  being longer than 
# the length of the side of equilateral triangle obtained by running the code are:
# PA = 0.33
# PB = 0.50
# PC = 0.25
# However there is a small change in values of probabilities for each running 
# due to the chords being plotted randomly on the surface of the circle. 

library(tidyverse)
library(ggforce)
theme_set(theme_void()) 

x0=0; y0=0; #center of circle
r=1; #radius
numbLines=100; #number of lines

#coordinates of equilateral triangle
eqtri_df <- tibble(
  x    = c(0, sqrt(3) / 2, -sqrt(3) / 2),
  y    = c(1, -0.5, -0.5),
  xend = c(sqrt(3) / 2, -sqrt(3) / 2, 0),
  yend = c(-0.5, -0.5, 1)
)


# Method A......................................................................

thetaA1=2*pi*runif(numbLines); #choose angular component uniformly
thetaA2=2*pi*runif(numbLines); #choose angular component uniformly

#coordinates of random chords
rdmchr_A <- tibble(
  x    = x0+r*cos(thetaA1),
  y    = y0+r*sin(thetaA1),
  xend = x0+r*cos(thetaA2),
  yend = y0+r*sin(thetaA2)
)

## Calculate probability that the chord is longer than a side of the triangle

#coordinates of random chords
xA    = x0+r*cos(thetaA1);
yA    = y0+r*sin(thetaA1);
xAend = x0+r*cos(thetaA2);
yAend = y0+r*sin(thetaA2);

#length of triangle side
lengthSide<-r*sqrt(3); 

#lengths of the chords
lengthA=sqrt((xA-xAend)^2+(yA-yAend)^2);

#probability of chord being longer than triangle side
probA=mean(lengthA > lengthSide); 

## Plot A
pA <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "black") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = "blue") +
  geom_segment(data = rdmchr_A, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

print(pA + ggtitle("Method A: Random endpoint"))

              
# Method B......................................................................

thetaB=2*pi*runif(numbLines); #choose angular component uniformly
pB=r*runif(numbLines); #choose radial component uniformly
qB=sqrt(r^2-pB^2); #distance to circle edge (along line)

#calculate trig values
sin_thetaB=sin(thetaB);
cos_thetaB=cos(thetaB);

# Coordinates of random chord
rdmchr_B <- tibble(
  x    = x0+pB*cos_thetaB+qB*sin_thetaB,
  y    = y0+pB*sin_thetaB-qB*cos_thetaB,
  xend = x0+pB*cos_thetaB-qB*sin_thetaB,
  yend = y0+pB*sin_thetaB+qB*cos_thetaB
)

## Calculate probability that the chord is longer than a side of the triangle

#coordinates of random chord
xB    = x0+pB*cos_thetaB+qB*sin_thetaB;
yB    = y0+pB*sin_thetaB-qB*cos_thetaB;
xBend = x0+pB*cos_thetaB-qB*sin_thetaB;
yBend = y0+pB*sin_thetaB+qB*cos_thetaB;

#length of triangle side
lengthSide<-r*sqrt(3);

#lengths of the chords
lengthB=sqrt((xB-xBend)^2+(yB-yBend)^2);

#probability of chord being longer than triangle side
probB=mean(lengthB>lengthSide);

# Plot B
pB <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "black") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = "blue") +
  geom_segment(data = rdmchr_B, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

print(pB + ggtitle("Method B: Random radial points"))


# Method C.......................................................................

thetaC=2*pi*runif(numbLines); #choose angular component uniformly
pC=r*sqrt(runif(numbLines)); #choose radial component
qC=sqrt(r^2-pC^2); #distance to circle edge (along line)

#calculate trig values
sin_thetaC=sin(thetaC);
cos_thetaC=cos(thetaC);

#coordinates of random chords
rdmchr_C <- tibble(
  x    = x0+pC*cos_thetaC+qC*sin_thetaC,
  y   = y0+pC*sin_thetaC-qC*cos_thetaC,
  xend = x0+pC*cos_thetaC-qC*sin_thetaC,
  yend = y0+pC*sin_thetaC+qC*cos_thetaC
)

## Calculate probability that the chord is longer than a side of the triangle

#coordinates of random chords
xC    = x0+pC*cos_thetaC+qC*sin_thetaC;
yC    = y0+pC*sin_thetaC-qC*cos_thetaC;
xCend = x0+pC*cos_thetaC-qC*sin_thetaC;
yCend = y0+pC*sin_thetaC+qC*cos_thetaC;

#length of triangle side
lengthSide<-r*sqrt(3);

#lengths of the chords
lengthC=sqrt((xC-xCend)^2+(yC-yCend)^2);

#probability of chord being longer than triangle side
probC=mean(lengthC>lengthSide);

# Plot C
pC <- ggplot() +
  ggforce::geom_circle(aes(x0 = 0, y0 = 0, r = 1), col = "black") +
  geom_segment(data = eqtri_df, aes(x = x, y = y, xend = xend, yend = yend), col = "blue") +
  geom_segment(data = rdmchr_C, aes(x = x, y = y, xend = xend, yend = yend),
               col = "red3") +
  coord_equal()

print(pC + ggtitle("Method C: Random midpoint"))


### References..................................................................
## https://en.wikipedia.org/wiki/Bertrand_paradox_(probability)
## https://hpaulkeeler.com/the-bertrand-paradox/
## https://github.com/hpaulkeeler/posts/blob/master/BertrandParadox/BertrandParadox.R
## https://www.wikiwand.com/en/Bertrand_paradox_(probability)
