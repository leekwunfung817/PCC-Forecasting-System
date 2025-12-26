########################################################################
# Newton's Method
#   Implements the Newton-Raphson method for finding a root
########################################################################

newtons = function(p,f,fp,eps=1e-7,n=50) {
    ##
    ## Inputs
    ##   p = initial guess
    ##   f = function to find the root of (global function)
    ##   fp = derivative of f (global function)
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)
    ##

    fail=TRUE
    save=c("p_n","abs(diff)")
    piter = matrix(c(p,NA),1,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        diff = f(p)/fp(p)
        p = p - diff
        piter = rbind(piter,c(p,abs(diff))) #save stuff
        if ( f(p)==0 || abs(diff) < eps ) { fail=FALSE; break }
      
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(0:i,save)
    return(list(iterations=piter,zero=p))
}


