########################################################################
# Fixed Point Method
#   Implements the fixed point method for finding a fixed point of g(x)
#     This gives g(p)=p, which also solves f(p)=0
########################################################################

fixedpt = function(x0,g,eps=1e-7,n=30) {
    ##
    ## Inputs
    ##   x0 = initial guess
    ##   g = function to find the fixed point of
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)
    ##   finite = number of iterations to perform before backing out.

    fail=TRUE
    pold=x0 #first guess
    save=c("p_n","abs(delta p_n)")
    piter = matrix(c(x0,0),1,length(save))  #matrix to save iterations
    for (i in 1:n) {
        p = g(pold)
        piter = rbind(piter,c(p,abs(p-pold))) #save stuff
        if ( g(p)==p || abs(p-pold) < eps ) { fail=FALSE; break }
        pold=p
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(0:i,save)
    return(list(iterations=piter,zero=as.numeric(p)))
}

############################################################################
# To perform fixed point for only a few iterations:

fxpt = function(x0,g,n) {
    ## Inputs
    ##  x0 = initial guess
    ##  g = function to find the fixed point of
    ##  n = number of iterations
    p=rep(x0,n+1) #the x0's will get replaced
    for (i in 1:n) p[i+1]=g(p[i])
    piter=matrix(c(p,c(0,abs(diff(p)))),ncol=2)
    colnames(piter)=c("p_n","abs(delta p_n)")
    rownames(piter)=0:(dim(piter)[1]-1) #Iteration numb.
    return(piter)
}

