############################################################################
## Testing the code for Lagrange Polynomials used for interpolation
##
library(english)   #You need to load this to use this chapters programs

x = seq(1.2,2.6,by=0.2)
y = digamma(x)  # just an example
plot(x,y)       # The plot looks really linear and crosses x-axis at about x=1.4

A = cbind(x,y)
f = lagrange(A)
curve(f,1,2)    # Note that you can plot the Lagrange polynomial easily on R

## other plotting examples
plot(x,f(x),type="l")           #connect the dots (linear interpolation)
points(x,digamma(x),col="red")  #check vs real answer (pretty close)


############################################################################
## Example on how to use interpolation to compute zeros of functions:

## Use inverse interpolation to find the zero of the digamma function.
z=lagrange(cbind(y,x))(0);z
## Compare to the real answer!  The zero is the minimum of the gamma function.
## You can use the "optimize" command from R to find a minimum
real=optimize(gamma,interval=c(1,2),maximum=FALSE)  
z-real$minimum   #really close!  (It's not usually this close!)
############################################################################
