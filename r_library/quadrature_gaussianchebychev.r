##########################################################################
#
# Gaussian Chebyshev Quadrature
#   
##########################################################################

gausschebyshev = function(f,n) {
    ## The abscissa and weights are REALLY easy here
    ## The weights are pi/n and the abscissa are cos((2i-1)/(2n)*pi)
    xs = cos((2*(1:n)-1)/(2*n)*pi)
    return(pi/n*sum(f(xs)))
}
