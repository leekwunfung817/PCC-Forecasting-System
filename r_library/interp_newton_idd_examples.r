############################################################################
## Testing the code for NIDD (Newton's Interpolatory Divided Differences)
##
library(english)   #You need to load this to use this chapters programs

## Find interpolation approx to Bessel function (first kind) of order 0
##   Note that this is the data the book uses in its examples.
x=c(1,1.3,1.6,1.9,2.2)
y=besselJ(x,0)
A=cbind(x,y)
## this generates the table, the coefficients, and the interpolate
stuff = nidd(A,1.5)  

## you can access the info using the $ notation.  Note that the coef
## are the coefficients for the Newton's Divided Difference
## polynomial.  This is equivalent to the Lagrange Polynomials, but is
## easier to generate.  
stuff
stuff$table

############################################################################
## Example 2 - show how well it fits the polynomial
f = function(A,coef,xs) {
    ## The actual interpolary polynomial - suitable for plotting
    x=A[,1]
    out=rep(0,length(xs))
    for (i in 1:length(xs)) out[i]=sum(coef*c(1,cumprod(xs[i]-x[-length(x)])))
    return(out)
}


x = seq(0,5,by=.1)
y=f(A,stuff$coef,x)           # Polynomial approximation
yreal=besselJ(x,0)            # Real answer
x[which(abs(y-yreal)<1e-10)]  # This shows you when there is no error (it's at the nodes!!)

## Here's a plot of the interpolary polynomial with example headings
## and labels.
yrange=range(y,yreal)
plot(x,y,ylim=yrange,type="l",col="red")
points(x,yreal,type="l",lty=2,col="blue")
legentext=bquote(expression(J[0](x),P[.(dim(A)[1])](x)))
legend(1,1, legend=eval(legentext) , cex=0.8, col=c("blue","red"), lty=2:1);
points(c(1,1),c(min(yrange),A[1,2]),ylim=yrange,type="l")
points(c(2.2,2.2),c(min(yrange),A[5,2]),ylim=yrange,type="l")
title("Newton's Interpolary Divided Difference","Interpolation created between the two vertical lines")

############################################################################
