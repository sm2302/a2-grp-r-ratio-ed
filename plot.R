#bertrand's paradox plot

library(tidyverse)
library(ggplot2)
library(ggforce)
theme_set(theme_void())

r=1; #radius of circle
x0=0; y0=0; #centre of circle

#circle points
k=seq(0,2*pi,len=200);
xp=r*cos(k);
yp=r*sin(k);

#plotting the circle
plot(x0+xp,y0+yp,type="l", col='black');

#angles of equilateral triangles
thetaTri1=2*pi;
thetaTri2=thetaTri1+2*pi/3;
thetaTri3=thetaTri1-2*pi/3;

#points for equilateral triangle
xTri1=x0+r*sin(thetaTri1);yTri1=x0+r*cos(thetaTri1);
xTri2=x0+r*sin(thetaTri2);yTri2=x0+r*cos(thetaTri2);
xTri3=x0+r*sin(thetaTri3);yTri3=x0+r*cos(thetaTri3);

#plotting the equilateral triangle
lines(c(xTri1,xTri2),c(yTri1,yTri2));
lines(c(xTri2,xTri3),c(yTri2,yTri3));
lines(c(xTri3,xTri1),c(yTri3,yTri1));

#angles of chords 
thetaChord1=2*pi*runif(1);
thetaChord2=2*pi*runif(1);

#points chord
xChord1=x0+r*cos(thetaChord1);yChord1=x0+r*sin(thetaChord1);
xChord2=x0+r*cos(thetaChord2);yChord2=x0+r*sin(thetaChord2);

#plotting the chord
lines(c(xChord1,xChord2),c(yChord1,yChord2),col='red');