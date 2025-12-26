########################################################################
# Bisection Method
#   Implements the bisection method for finding a root
########################################################################

bisection = function(a,b,f,eps=1e-7,n=30) {
    ##
    ## Inputs
    ##   [a,b] = interval root is bracketed by
    ##   f = function to find the root of
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)

    if ( sign(f(a))*sign(f(b)) > 0 )
        stop(paste("root does not exist in [",a,",",b,"]",sep=""))

    fail=TRUE
    save=c("Midpt (p)","LeftB (a)","RightB (b)","err (b-a)")
    piter = matrix(0,0,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        p = (a+b)/2
        piter = rbind(piter,c(p,a,b,abs(b-a)/2)) #save stuff
        if ( f(p)==0 || (b-a)/2 < eps ) { fail=FALSE; break }
        if ( sign(f(a))*sign(f(p)) > 0 ) { a=p } else { b=p }    
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(1:i,save)
    return(list(iterations=piter,zero=p))
}
