################################################################
## 
## Elementary implementation of some Newton-Cotes Composite Methods
##   Given for ease of understanding
##
################################################################

tiny_midpt = function(a,b,f,n) {
    # Composite Midpt rule
    # Inputs
    #   a,b = endpoints
    #   f   = function to integrate
    #   n   = number of intervals to compute (should be even)
    if ( (n %% 2) == 1 ) stop("n must be even")
    h = (b-a)/n
    x = seq(a+h,b-h,by=2*h)
    return(2*h*sum(f(x)))
}

tiny_trapezoid = function(a,b,f,n) {
    # Composite Trapezoid rule
    # Inputs
    #   a,b = endpoints
    #   f   = function to integrate
    #   n   = number of intervals to compute
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(2,n-1),1)
    return(h/2*sum(coef*f(x)))
}

tiny_simpsons = function(a,b,f,n) {
    # Composite Simpson's rule
    # Inputs
    #   a,b = endpoints
    #   f   = function to integrate
    #   n   = number of intervals to compute (must be even)
    if ( (n %% 2) == 1 ) stop("n must be even")
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(c(4,2),(n-2)/2),4,1)
    return(h/3*sum(coef*f(x)))
}

