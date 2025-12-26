########################################################################
# False Position Method
#   Implements this method for finding a root
########################################################################

falseposition = function(p0,p1,f,eps=1e-7,n=50) {
    ##
    ## Inputs
    ##   [p0, p1] = real root is between p0 and p1
    ##   f = function to find the root of
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)

    fp0 = f(p0); fp1 = f(p1);
    if ( sign(fp0)*sign(fp1) > 0 )
        stop(paste("root does not exist in [",min(c(p0,p1)),",",
                  max(c(p0,p1)),"]",sep=""))

    fail=TRUE
    save=c("new_p","[p0,","p1]","err (p-p1)")
    piter=matrix(0,0,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        p = p1 - fp1*(p1-p0)/(fp1-fp0)
        piter = rbind(piter,c(p,p0,p1,abs(p-p1))) #save stuff
        if ( f(p)==0 || abs(p-p1) < eps ) { fail=FALSE; break }
        if ( (fp=f(p))*fp1 < 0 ) { p0=p1; fp0=fp1} #switch p0&p1 sometimes
        p1=p; fp1=fp 
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(1:i,save)
    return(list(iterations=piter,zero=p))
}
