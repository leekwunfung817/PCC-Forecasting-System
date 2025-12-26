########################################################################
# Brent's Method
#   Implements Brent's method for finding a root
########################################################################

brent = function(a,b,f,eps=1e-12,n=50) {
    ##
    ## Inputs
    ##   a, b = initial guesses
    ##   f = function to find the root of
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)
    ## From Numerical recipies
    
    fa = f(a)
    fb = f(b)
    if ( sign(fa)*sign(fb) > 0 ) {
        stop("Root is not bracketed between ",a," and ",b)
    }
    fc=fb

    fail=TRUE
    save=c("a_n","f(a)","b_n","f(b)","c_n","f(c)","err (b-a)")
    piter = matrix(0,0,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        if ((fb>0 && fc>0) || (fb < 0 && fc < 0) ) {
            c=a;
            fc=fa;
            e=d=b-a;
        }
        if ( abs(fc) < abs(fb) ) {
            a=b; b=c; c=a;
            fa=fb; fb=fc; fc=fa;
        }
        tol1=2*.Machine$double.eps*abs(b)+.5*eps
        xm=.5*(c-b);
        if (abs(xm) <= tol1 || fb == 0 ) {
            fail=FALSE
            dimnames(piter)=list(1:(i-1),save) #Iteration numb.                
            return(list(iterations=piter,zero=b))
        }        
        if (abs(e) >= tol1 && abs(fa) > abs(fb) ) {
            s=fb/fa;
            if (a == c ) {
                p = 2.0*xm*s;
                q = 1 - s;
            } else {
                q=fa/fc;
                r=fb/fc;
                p=s*(2*xm*q*(q-r)-(b-a)*(r-1));
                q=(q-1)*(r-1)*(s-1)
            }
            if (p > 0) q=-q;
            p=abs(p)
            min1=3*xm*q-abs(tol1*q);
            min2=abs(e*q);
            if (2*p < min(min1,min2)) {
                e=d
                d=p/q
            } else {
                d=xm;
                e-d;
            }
        } else {
            d=xm;
            e=d;
        }
        a=b; fa=fb;
        if (abs(d) > tol1 ) {
            b = b+d;
        } else {
            b = b + sign(xm)*abs(tol1)
        }
        fb=f(b)
        piter = rbind(piter,c(a,fa,b,fb,c,fc,abs(b-a))) #save stuff    
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations")) 
}
