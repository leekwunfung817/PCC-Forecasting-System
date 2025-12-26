############################################################################
#
# Simpson's Quadrature - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

simpsons = function(a,b,f,n=10,plot=FALSE,toscale=FALSE) {
    # Composite Simpson's rule
    # Inputs
    #   a,b  = endpoints
    #   f    = function to integrate
    #   n    = number of intervals to compute (must be even)
    #   plot = plot the integral
    #   toscale = keep the height and width to scale
    if ( (n %% 2) == 1 ) stop("n must be even")
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(c(4,2),(n-2)/2),4,1)
    intg=h/3*sum(coef*f(x))
    if (plot) {
        rule=paste("Simpson's Rule","\nArea = ",intg,sep="") 
        if (toscale)
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),asp=1,main=rule)
        else
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),main=rule)
        h = (b-a)/n
        for (i in seq(0,n-2,by=2) ) {
            xj = a + i*h
            xpts = c(xj,xj+h,xj+2*h)
            ypts=f(xpts)
            coef=solve(cbind(1,xpts,xpts^2),ypts)  #solving for coefs of fitted quadratic
            xx=seq(xj,xj+2*h,len=51)
            xs=c(xj,xx,xj+2*h)
            ys=c(0,as.vector(c(1,1,1)%*%(coef*rbind(1,xx,xx^2))),0) #points of fitted quadratic
            polygon(xs,ys,col=rgb(0,0,1.0,alpha=.5))
        }
    }
    return(intg)
}
