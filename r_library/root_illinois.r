########################################################################
# Illinois Method
#   Implements this method for finding a root (modification of false position)
#   This tries to fix the issues of slow convergence of false position
########################################################################

illinois = function(s,t,f,eps=1e-15,n=50) {
    ##
    ## Inputs
    ##   [p0, p1] = real root is between p0 and p1
    ##   f = function to find the root of
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)
    ## Algorithm taken from Wikipedia
    
    fs = f(s)
    ft = f(t)
    side = 0

    fail=TRUE
    save=c("p","s","f(s)","t","f(t)","abs(f(p))")
    piter = matrix(0,0,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        p = t - ft*(t-s)/(ft-fs)
        fp=f(p)
        piter = rbind(piter,c(p,s,fs,t,ft,abs(f(p)))) #save stuff
        if (abs(t-s) < eps*abs(t+s)) { fail=FALSE; break}

        if ( fp*ft > 0 ) {
            t=p; ft=fp;
            if ( side == -1 ) fs = fs / 2;
            side = -1
        }
        else if (fs*fp > 0) {
            s=p;fs=fp;
            if (side == 1) ft = ft / 2;
            side = 1;
        }
        else {fail=FALSE; break}
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    dimnames(piter)=list(1:i,save)
    return(list(iterations=piter,zero=p))
}

