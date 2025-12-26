########################################################################
# Secant Method
#   Implements the Secant method for finding a root
########################################################################

secant = function(p0,p1,f,eps=1e-7,n=50) {
    ##
    ## Inputs
    ##   p0, p1 = initial guesses
    ##   f = function to find the root of
    ##   eps = tolerance (defaults to 1e-7 = .0000001)
    ##   n = iterations allowed (defaults to 30)

    fp0 = f(p0)
    fp1 = f(p1)

    fail=TRUE
    save=c("new_p","a_n","b_n","err (b-a)")
    piter = matrix(0,0,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        p = p1 - fp1*(p1-p0)/(fp1-fp0)  #secant step
        piter = rbind(piter,c(p,p0,p1,abs(p0-p1))) #save stuff
        if ( f(p)==0 || abs(p-p1) < eps ) { fail=FALSE; break }
        p0=p1; fp0=fp1; #move p1 to p0
        p1=p; fp1=f(p1) #save new p1
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(1:i,save)
    return(list(iterations=piter,zero=p))
}

