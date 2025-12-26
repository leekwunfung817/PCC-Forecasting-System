########################################################################
# Steffensen's Method 
#   Implements the Steffensen method for finding a fixed point
#   This method is quadratically convergent without computing
#   derivatives at the expense of two function evaluations per step.
########################################################################

steff = function(p,g,eps=1e-7,n=50,complete=FALSE) {
    ##
    ## Inputs
    ##   p = initial guess
    ##   f = function to find the root of
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)
    ##   iter = output iteration details
    ##   complete = include fixed pt iterations too.
    ## Outputs
    ##   fixed point of g
    delta = function(p0,p1,p2) { p0-(p1-p0)^2/(p2-2*p1+p0) }
    fail=TRUE
    save=c("p_n","g(p)","abs(g(p)-p)")
    piter = matrix(c(p,g(p),abs(g(p)-p)),1,length(save))  #save iterations
    pcomp = matrix(0,0,length(save))  #save iterations
    for (i in 1:n) {
        p1 = g(p); p2 = g(p1); #fixed point twice
        if (complete) pcomp = rbind(pcomp,c(p,p1,p2))
        p = delta(p,p1,p2)  #Aitken's method once
        piter = rbind(piter,c(p,g(p),abs(g(p)-p))) #save stuff
        if ( abs(g(p)-p) < eps ) {fail=FALSE; break }
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    if (complete) { pcomp = rbind(pcomp, c( p, NA, NA ))
        dimnames(pcomp)=list(0:i,c("p_0^(n)","p_1^(n)","p_2^(n)"));
        return(pcomp)
    } else { 
        dimnames(piter)=list(0:i,save)
        return(list(iterations=piter,zero=p))
    }
}
