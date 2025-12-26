############################################################################
#
# Trapezoid Quadrature - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

trapezoid = function(a,b,f,n=10,plot=FALSE,toscale=FALSE) {
    # Composite Trapezoid rule
    # Inputs
    #   a,b  = endpoints
    #   f    = function to integrate
    #   n    = number of intervals to compute
    #   plot = plot the integral
    #   toscale = keep the height and width to scale
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(2,n-1),1)
    intg=h/2*sum(coef*f(x))
    if (plot) {
        rule=paste("Trapezoid Rule","\nArea = ",intg,sep="") 
        if (toscale)
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),asp=1,main=rule)
        else
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),main=rule)
        h = (b-a)/n
        for (i in 0:(n-1) ) {
            xj = a + i*h
            xs = rep(c(xj,xj+h),each=2)
            ys = c(0,f(c(xj,xj+h)),0)
            polygon(xs,ys,col=rgb(0,0,1.0,alpha=.5))
        }
    }
    return(intg)
}

