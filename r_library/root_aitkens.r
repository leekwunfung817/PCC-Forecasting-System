########################################################################
# Aitken's Delta Squared Method
#   Implements the Aitken's Delta Squared method for accelerating
#   the convergence of a sequence to a fixed point.
########################################################################

aitkens = function(p) {
    ## Inputs
    ##   p = sequence of numbers (a vector)
    ## Outputs
    ##   phat = sequence of numbers accelerated by Aitken's method
    ##
    delta = function(p) { p[1]-(p[2]-p[1])^2/(p[3]-2*p[2]+p[1]) }
    phat = 0
    for (i in 1:(length(p)-2)) phat[i] = delta(p[i:(i+2)])
    return(phat)
}
