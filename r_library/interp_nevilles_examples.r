############################################################################
## Testing the code for Neville's Method for approximating
##
library(english)   #You need to load this to use this chapters programs

## Find approximations of exp(x) over 1.0, 1.1, 1.2, 1.3, 1.4, 1.5
x=1+(0:5)/10
y=exp(x)
A=cbind(x,y)
## Find approx to exp(1.5) using all nodes or only 1:2 (linear)
neville(A,1.5)     # Use all nodes
neville(A,1.5,1:2) # Use only nodes 1:2


## Find approx to Bessel function (first kind) of order 0
##   Note that this is the data the book uses in its examples.
x=c(1,1.3,1.6,1.9,2.2)
y=besselJ(x,0)
A=cbind(x,y)
## Find approx to J0(1.5) using all nodes
neville(A,1.5)     #Use all nodes

## Find approx to function defined below
x=c(1.00,1.05,1.10,1.15)
y=c(.1924,.2414,.2933,.3492)
A=cbind(x,y)
## What is f(1.09)?
##  Note that the table gives all the first order approximations of
##  f(1.09) as well as the second order, third order, etc.
neville(A,1.09)

############################################################################


