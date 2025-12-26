library(english)
########################################################################
#
# Laguerre's Method
#   Find a zero of a POLYNOMIAL
#
########################################################################

p=function(x) { x^5 -4*x + 1 }
pp=function(x) { 5*x^4 - 4 }
ppp=function(x) { 20*x^3 }


laguerre = function(x,p,d,eps=1e-7,n=50) {
    ##
    ## Inputs
    ##   x = initial guess
    ##   d = polynomial degree
    ##   p = polynomial (global function)
    ##   pp = derivative of p (global function)
    ##   eps = tolerance (defaults to 1e-9 = .000000001)
    ##   n = iterations allowed (defaults to 30)
    ##

    fail=TRUE
    save=c("x_n","abs(diff)")
    xiter = matrix(c(x,NA),1,length(save))  #initialize a matrix to save iterations
    for (i in 1:n) {
        G = pp(x)/p(x)
        H = G^2 - ppp(x)/p(x)
        denom = if (G>0) G + sqrt((n-1)*(n*H-G*G)) else G - sqrt((n-1)*(n*H-G*G))
        a = n/denom
        x = x - a
        xiter = rbind(xiter,c(x,abs(a))) #save stuff
        if ( abs(a) < eps ) { fail=FALSE; break }
    }
    if (fail) warning(paste("Failed to converge after",i,"iterations"))
    xiter
    dimnames(xiter)=list(0:i,save)
    return(list(iterations=xiter,zero=x))
}


########################################################################
# Aitken's Delta Squared Method
#   Implements the Aitken's Delta Squared method for accelerating
#   the convergence of a sequence to a fixed point.
########################################################################

aitkens = function(p) {
    ## Inputs
    ##   p = sequence of numbers (a vector)
    ## Outputs
    ##   phat = sequence of numbers accelerated by Aitken's method
    ##
    delta = function(p) { p[1]-(p[2]-p[1])^2/(p[3]-2*p[2]+p[1]) }
    phat = 0
    for (i in 1:(length(p)-2)) phat[i] = delta(p[i:(i+2)])
    return(phat)
}
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
