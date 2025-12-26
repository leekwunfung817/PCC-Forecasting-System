############################################################################
## Testing the code for Cubic Splines
##
library(english)   #You need to load this to use this chapters programs

source("C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\r_library\\ch2.r")
source("C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\r_library\\ch3.r")
source("C:\\xampp\\htdocs\\TacticalDataDrivenDecisionSystem\\r_library\\ch4.r")

#####################################################################
## The duck example from the book
## Table 3.15
x=c(0.9,1.3,1.9,2.1,2.6,3,3.9,4.4,4.7,5,6,7,
    8,9.2,10.5,11.3,11.6,12,12.6,13,13.3)
y=c(1.3,1.5,1.85,2.1,2.6,2.7,2.4,2.15,2.05,2.1,2.25,
    2.3,2.25,1.95,1.4,0.9,0.7,0.6,0.5,0.4,0.25)

A=cbind(x,y)
M=spline(A);M      ## compute the coefficients

##> M
##        x    a                b                 c                 d
##S_0   0.9 1.30  0.5396238492562  0.00000000000000 -0.24764905785144
##S_1   1.3 1.50  0.4207523014875 -0.29717886942173  0.94691209305232
##S_2   1.9 1.85  1.0868027186780  1.40726289807244 -2.95638245731127
##S_3   2.1 2.10  1.2949419830296 -0.36656657631432 -0.44663477948969
##S_4   2.6 2.60  0.5933993220980 -1.03651874554886  0.44505110075970
##S_5   3.0 2.70 -0.0221911459764 -0.50245742463723  0.17415987014396
##S_6   3.9 2.40 -0.5034060258736 -0.03222577524853  0.07807565399152
##S_7   4.4 2.15 -0.4770750606285  0.08488770573875  1.31417128415048
##S_8   4.7 2.05 -0.0713161904646  1.26764186147418 -1.58121890345516
##S_9   5.0 2.10  0.2623398224870 -0.15545515163546  0.04311532914847
##S_10  6.0 2.25  0.0807755066615 -0.02610916419005 -0.00466634247143
##S_11  7.0 2.30  0.0145581508671 -0.04010819160434 -0.02444995926276
##S_12  8.0 2.25 -0.1390081101298 -0.11345806939260  0.01747068986179
##S_13  9.2 1.95 -0.3358340964692 -0.05056358589017 -0.01272790825475
##S_14 10.5 1.40 -0.5318299146352 -0.10020242808368 -0.02032522327792
##S_15 11.3 0.90 -0.7311782282627 -0.14898296395069  1.21340500868025
##S_16 11.6 0.70 -0.4929486542894  0.94308154386153 -0.83927477034485
##S_17 12.0 0.60 -0.1413353089657 -0.06404818055230  0.03638208508460
##S_18 12.6 0.50 -0.1789004737371  0.00143957259997 -0.44797097064283
##S_19 13.0 0.40 -0.3927748815657 -0.53612559217142  0.59569510241269
##S_20 13.3 0.25               NA                NA                NA

splinefunc(M,8.2)  ## evaluate the spline function at 8.2

splineplot(M)      ## plot the spline
points(x,y,col="red")  ## the red circles are the provided points

## Try and fit with a Newton's Interpolatory Divided Difference
## Polynomial (This shows that polynomials have this "oscillatory" weakness
# xx = c(.88,seq(0.9,13.3,by=.1),13.32)
## note that the polynomial and the spline match the nodes, but in
## between the nodes is another story!
# points(xx,lagrange(A)(xx),lty=2,type="l",col="blue") 
# legentext=bquote(expression(S(x),P[.(dim(A)[1])](x))) 
# legend(2,7, legend=eval(legentext) , cex=0.8, col=c("red","blue"), lty=1:2);
title("Natural Cubic spline vs. Lagrange Polynomial",
      "Note how the Lagrange Polynomial matches points ONLY at the nodes.  Spline is better!")


## mess with the derivs below to see how the plot changes on the edges
# M2=spline(A,"clamped",1.0728217,1.02312214)  
# splineplot(M2)

#####################################################################
