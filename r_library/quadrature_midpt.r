############################################################################
#
# Midpoint Quadrature - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

midpt = function(a,b,f,n=10,r,plot=FALSE,toscale=FALSE) {
    # Composite Midpt rule
    # Inputs
    #   a,b  = endpoints
    #   f    = function to integrate
    #   n    = number of inside interval points (should be even)
    #          Leads to (n+2)/2 rectangles.  The width of the
    #          rectangles is double of the trapezoid rule.
    #          To compare for a similar width of rec/trap, use
    #          the r optional argument below.
    #   r    = optional argument (number of rectangles)
    #   plot = plot the integral
    #   toscale = keep the height and width to scale
    if ( (n %% 2) == 1 ) stop("n must be even")
    if (!missing(r)) n = 2*r-2  #specify number or rectangles instead
    h = (b-a)/(n+2)  #smallest open Newton-Cote's method h
    x = seq(a+h,b-h,by=2*h)
    intg=2*h*sum(f(x))
    if (plot) {
        rule=paste("Midpoint Rule","\nArea = ",intg,sep="") 
        if ( (n %% 2) == 1 ) stop("n must be even")
        ##n is the num of inside partition points, not numb of rectangles
        if (missing(r)) n = (n+2)/2  else n = r
        if (toscale)
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),asp=1,main=rule)
        else
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),main=rule)
        h = (b-a)/n
        for (i in 0:(n-1) ) {
            xj = a + i*h
            xs = rep(c(xj,xj+h),each=2)
            ys = c(0,rep(f(xj+.5*h),2),0)
            polygon(xs,ys,col=rgb(0,0,1.0,alpha=.5))
        }
    }
    return(intg)
}

