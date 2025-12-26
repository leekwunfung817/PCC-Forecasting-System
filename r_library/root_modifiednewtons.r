########################################################################
# Modified Newton's Method
#   Implements the modified Newton-Raphson method for finding a root
#    This allows for zeros of multiplicities greater than 1.  An
#    example of a good one is f(x) = 1-cos(2*x)
########################################################################

mnewtons = function(p,f,fp,fp2,eps=1e-7,n=50) {
    ##
    ## Inputs
    ##   f   = function to find the root of (global function)
    ##   fp  = derivative of function (global function)
    ##   fp2 = second derivative of function (global function)
    ##   p   = initial guess
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n   = iterations allowed (defaults to 30)

    fail=TRUE
    save=c("p_n","err (b-a)")        #Things to save
    piter = matrix(0,0,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        diff = f(p)*fp(p)/(fp(p)^2-f(p)*fp2(p))
        p = p - diff
        piter = rbind(piter,c(p,abs(diff)))     #save stuff
        if ( f(p)==0 | abs(diff) < eps ) { fail=FALSE; break }
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(1:i,save)
    return(list(iterations=piter,zero=p))
}
