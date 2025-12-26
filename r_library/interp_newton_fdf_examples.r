############################################################################
## Testing the code for Newton's Forward Difference Formula
##   Note the slight difference between Newton's Forward Difference
##   Formula and Newton's forward divided-difference formula.
##
library(english)   #You need to load this to use this chapters programs

## Find interpolation approx to Bessel function (first kind) of order 0
##   Note that this is the data the book uses in its examples.
x=c(1,1.3,1.6,1.9,2.2)
y=besselJ(x,0)
A=cbind(x,y)

stuff = nfdf(A,1.5)  
stuff
stuff$table
stuff$coef
stuff$interp

############################################################################
