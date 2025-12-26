library(english)
############################################################################
#
# Cubic Spline Interpolation -
#   This returns the coefficients, aj,bj,cj,dj for i=0,...,n-1 for the
#     cubic spline interpolatry function
#          S_j = aj+bj(x-xj)+cj(x-xj)^2+dj(x-xj)^3
#   given f is stored in table form as A, which contains the 
#     x-values in the first column, the f(x-values) in the second
#     column,
#   The return is a matrix with the columns [ xj aj bj cj dj ]
#  written by Scott Hyde from algorithm 3.4&3.5
#    additions now plot the function with splineplot below
############################################################################

spline = function(A,type="free",fp0,fpn) {
    ## Inputs
    ##   A = table of nodes with function values and deriv values
    ##   type = "free" or "clamped".  Free is default.
    ##   fp0 = f'(x0)  (only used with clamped)
    ##   fpn = f'(xn)  (only used with clamped)
    ## Outputs
    ##   M is a matrix with the columns [ xj aj bj cj dj ], where each
    ##   row are the coefficients of the cubic polynomial for that
    ##   range.
    ##
    ## Since R does not allow a zero index in its arrays (because they
    ## are matrices), the index i for the vector (x0,x1,...,xn) has
    ## been reindexed to start at 1 - (x1,x2,...,x(n+1))  The output
    ## however, shows the proper label on the S_i functions. 
    x=A[,1]
    a=A[,2]
    n=dim(A)[1]-1
    
    ## Step 1 of the algorithm (follow it if you want)
    h=diff(x)
    ## Step 2
    if ( type == "clamped" ) {
        alpha = 3*(a[2]-a[1])/h[1]-3*fp0; #first alpha
        alpha[n+1] = 3*fpn - 3*(a[n+1]-a[n])/h[n]
    } else alpha=rep(0,n-1) # Natural boundary conditions
    ## Step 3
    for (i in 2:n) alpha[i] = 3/h[i]*(a[i+1]-a[i])-3/h[i-1]*(a[i]-a[i-1]) 

    ## Step 4
    if ( type == "clamped" ) {
        l=2*h[1]; mu=0.5; z=alpha[1]/l[1]
    } else {
        l=2; mu=0; z=0;  # Natural spline (sets the first element)
    }
    ## Step 5
    for (i in 2:n) {
        l[i] = 2*(x[i+1]-x[i-1]) - h[i-1]*mu[i-1]
        mu[i] = h[i]/l[i]
        z[i] = (alpha[i]-h[i-1]*z[i-1])/l[i]
    }
    ## Step 6
    c=b=d=rep(0,n+1)
    if ( type == "clamped" ) {
        l[n+1] = h[n]*(2-mu[n]);
        z[n+1] = (alpha[n+1]-h[n]*z[n])/l[n+1];
        c[n+1] = z[n+1]
    } else {
        l[n+1] = 1; z[n+1] = 0;  #Natural spline
    }
    ## Step 7
    for (j in n:1 ) {
        c[j] = z[j] - mu[j]*c[j+1];
        b[j] = (a[j+1]-a[j])/h[j] - h[j]*(c[j+1]+2*c[j])/3;
        d[j] = (c[j+1]-c[j])/(3*h[j])
    }

    mat=cbind(x,a,b,c,d)#[-(n+1),] #drop last row
    mat[n+1,3:5]=NA
    rownames(mat)=paste("S_",0:n,sep="")   #label the points
    return(mat)
}

############################################################################

splinefunc = function(M,x) {
    ## This evaluates the spline function defined by the matrix M at
    ## the value of x (M is from the spline function.)
    ## Inputs
    ##    M - Matrix of coefficients from spline.
    ##    x - value to evaluate spline at
    y=c()
    for ( i in 1:length(x) ) {
        whichf=if (all(M[,1]>=x[i])) 1 else max(which(M[,1]<=x[i]))
        y[i] = sum(M[whichf,]*c(0,1,cumprod(rep(x[i]-M[whichf,1],3))))
    }
    return(y)
}

############################################################################

splineplot = function(M,xmin=min(M[,1]),xmax=max(M[,1])) {
    ## Use this to get a plot of the spline over the range defined by
    ## the matrix.  If you want to go beyond the limits, then choose a
    ## new xmin and xmax
    par(pty="s")
    xx=seq(xmin,xmax,length=100)
    plot(xx,splinefunc(M,xx),type="l",xlab="x",ylab="spline",asp=1)
}

############################################################################
############################################################################
#
# Hermite Interpolation -
#   This returns the coefficients of the Hermite interpolatory polynomial H 
#   where f is stored in table form as A, which contains the 
#     x-values in the first column, the f(x-values) in the second column, 
#     and the f'(x-values) in the third column
#  written by Scott Hyde
############################################################################

hermite = function(A,xs) {
    ##
    ## Inputs
    ##   A = table of nodes with function values and deriv values
    ##   xs = value to interpolate from the table 
    ##
    ## Note that n below is really n+1 from algorithm This is because
    ##  R does not use 0 as an index, so everything has to be
    ##  increased by one (except when subtracting i-j, which needs to
    ##  be increased by 1 as well
    n=dim(A)[1]  
    x=A[,1]
    z=rep(x,each=2)
    Q=matrix(NA,2*n,2*n)
    Q[,1]=rep(A[,2],each=2)
    Q[seq(2,2*n,by=2),2]=A[,3]
    Q[seq(3,2*n-1,by=2),2]=diff(A[,2])/diff(x)
    for (i in 3:(2*n)) {
      for (j in 3:i) {
        Q[i,j] = (Q[i,j-1]-Q[i-1,j-1])/(z[i]-z[i-j+1])
      }
    }
## #The below code follows the algorithm in the book explicitly,
## #whereas above are changes I made to program it more efficiently
## n=dim(A)[1]-1
## z=rep(0,2*n+1)
## Q=matrix(NA,2*n+2,2*n+2)
##                                        #
## #note how the code adds one to EACH location value
## for (i in 0:n) {
##   row=i+1
##   erow=2*i+1   #even row + 1 (no zero row or column)
##   orow=2*i+1+1 #odd row
##   z[erow]=A[row,1]
##   z[orow]=A[row,1]
##   Q[erow,0+1]=A[row,2]
##   Q[orow,0+1]=A[row,2]
##   Q[orow,1+1]=A[row,3]
##   if ( i != 0 )
##     Q[erow,1+1]=(Q[erow,0+1]-Q[orow-2,0+1]) / (z[erow]-z[orow-2])
##   }
##   for (i in 2:(2*n+1) ) {
##      for (j in 2:i) {
##	   # note EACH location has +1 added to it Q[i,j] becomes Q[i+1,j+1]
##	   #    because R cannot do matrices with 0 indices.
##         Q[i+1,j+1]=(Q[i+1,j-1+1]-Q[i-1+1,j-1+1])/(z[i+1]-z[i-j+1])
##	}
##   }	
    # The coefficients of the Newton Interpolary Divided Difference
    # Formula are the diagonal entries of Q
    coef=diag(Q)
    # The next two lines use the coefficients to figure out the
    # interpolation.  First line creates a vector of the product of
    # x-xj, then the second finds the dot product of them.
    zvec=c(1,cumprod(xs-z[-length(z)]))
    interp=sum(coef*zvec)
    dimnames(Q)=list(z,c("f(x)","f'(x) or first DD",paste(ordinal(2:(2*n-1)),"DD")))
    return(list(table=Q,coef=coef,interp=interp))
}

############################################################################
#
# Lagrange Polynomial -
#  Given f is stored in table form as A, which contains the 
#     x-values in the first column, the f(x-values) in the second column, 
#  written by Scott Hyde
#
############################################################################


lagrange = function(A) {
    ## Lagrange
    ##   Inputs:
    ##      Matrix A, column 1 are the x-values, column 2 are the
    ##      y-values
    ##   Outputs:
    ##   Constructs a the Lagrange polynomial which interpolates the
    ##   points.  Note that this is a function, so can be used in plots.
    ##   The form of the answer is
    ##     P(x) =
    ##      coef[1]*(x-x[2])...(x-x[end]) +
    ##      coef[2]*(x-x[1])(x-x[3])...(x-x[end]) +
    ##      coef[3]*(x-x[1])(x-x[2])(x-x[4]...(x-x[end]) +
    ##      ...
    ##      coef[k]*(x-x[1])...(x-x[k-1])(x-x[k+1])...(x-x[end]) +
    ##      ...
    ##      coef[end]*(x-x[1])...(x-x[end-])
    ##
    library(MASS)
    make_function <- function(args, body, env = parent.frame()) {
        subs <- list(args = as.pairlist(args), body = noquote(body))
        eval(substitute(`function`(args, body), subs), env)
    }
    
    x = A[,1]
    y = A[,2]
    coef = 0;
    Lnk = rep("",length(x))
    for (i in 1:length(x)) {
        v = x[i]-x
        coef[i] = y[i]/prod(v[-i])
        pt = paste("(x-",x[-i],collapse="*",")",sep="")
        Lnk[i] = paste(fractions(coef[i]),pt,collapse="*",sep="*")
    }
    lagr = parse(text=paste(Lnk,collapse="+"))[[1]]
    return(make_function(alist(x=), lagr))
}
############################################################################
#
# Neville's Method - this function interpolates the value f(xs),
#   where xs, where f is stored in table form as A, which contains the 
#     x-values and f(x-values) in the first and second columns 
#  written by Scott Hyde
############################################################################

neville = function(A,xs,nodes=0:(dim(A)[1]-1)) {
    ## Inputs
    ##   A = table of nodes with function values
    ##   xs = value to interpolate from the table 
    ##   nodes = nodes to use for the approximation.  Should be
    ##        created using :, so 2:4, 1:3, 0:2 (Uses zero index, so
    ##        first node is zero.
    ## Outputs
    ##   Row labels are the x values from the matrix A.
    ##   First column is the y values from the matrix A.
    ##   Second column and all the rest are the evaluation of the
    ##     function at xs with increasing orders of accuracy.
    if (all(diff(nodes)==1)) { # This checks that the nodes are in a sequence separated by 1.
        n=length(nodes)
        nodes=nodes+1
        x=A[nodes,1]
        Q=matrix(,length(nodes),length(nodes))
        Q[,1]=A[nodes,2]
        
        for (i in 2:(dim(Q)[1])) {
            for (j in 2:i) {
                Q[i,j] = ((xs-x[i-j+1])*Q[i,j-1]-(xs-x[i])*Q[i-1,j-1])/(x[i]-x[i-j+1])
            }
        }
        dimnames(Q)=list(x,c("f(x)",paste(ordinal(1:(n-1)),"order")))
        
        return(list(table=Q,interp=Q[n,n]))
    } else stop("Nodes need to be consecutive.  Use 2:4, 1:3, 0:2, etc\n(Use zero index, so the first node is zero)")
}
############################################################################
#
# Newton's Backward Difference Formula
#   This returns the coefficients of the interpolatory polynomial P
#   where f is stored in table form as A, which contains the 
#     x-values and f(x-values) in the first and second columns 
#  written by Scott Hyde
############################################################################

nbdf = function(A,xs) {
    ## This is easy!  Just apply the forward difference to the reverse
    ## of x and y.  Needs the nfdf function to work!
    return(nfdf(apply(A,2,rev),xs))
}
############################################################################
#
# Newton's Forward Difference Formula & Backward Difference Formula
#   This returns the coefficients of the interpolatory polynomial P
#   where f is stored in table form as A, which contains the 
#     x-values and f(x-values) in the first and second columns 
#  written by Scott Hyde
############################################################################

nfdf = function(A,xs) {
    ##
    ## Inputs
    ##   A = table of nodes with function values
    ##   xs = value to interpolate from the table 
    ##
    n = dim(A)[1]-1  
    h = diff(A[1:2,1])  #find the difference between first two x's
    s = (xs-A[1,1])/h   #find s
    ##check if all the differences are equal
    if (!all(diff(A[,1])-h < 1e-10)) stop("Nodes are not equally spaced")
    ## Compute Deltak for each k. (This is the forward difference
    ## operator!) (not divided differences - they're not the same)
    deltak = rep(0,n+1)
    for (k in 0:n) deltak[k+1] = sum((-1)^(k-0:k)*choose(k,0:k)*A[0:k+1,2])
    ## Note that DD=deltak/(factorial(0:n)*h^(0:n)) are the divided
    ## difference coefs returned along the top in the divided
    ## differences table. 
    names(deltak)=paste("Δ^",0:n,sep="")
    choosesk=choose(s,0:n)
    names(choosesk)=paste("(s choose",0:n,")")
    names(s)="s"
    return(list(s=s,choose_sk=choosesk,delta_k=deltak,iterp=sum(choosesk*deltak)))
}

nbdf = function(A,xs) {
    ## This is easy!  Just apply the forward difference to the reverse
    ## of x and y
    ## Note that the odd coefficients returns are off by a sign, but
    ## so is the s choose k, so when you interpolate with them, you
    ## get the same result.
    return(nfdf(apply(A,2,rev),xs))
}
############################################################################
#
# Newton's Interpolatory Divided Difference Formula -
#   This returns the coefficients of the interpolatory polynomial P where
#   f is stored in table form as A, which contains the x -values and f(x-values)
#     in the first and second columns.
#   It will also evaluate the polynomial at a certain point.
#  written by Scott Hyde
############################################################################

nidd = function(A,xs) {
    ## Inputs
    ##   A = table of nodes with function values
    ##   xs = value to interpolate from the table 
    ##
    ## Note that n below is really n+1 from algorithm. This is because R does
    ##  not use 0 as an index, so everything has to be increased by one (except
    ##  when subtracting i-j, which needs to be increased by 1 as well
    n=dim(A)[1]  
    x=A[,1]
    F=matrix(NA,n,n)
    F[,1]=A[,2]
    
    for (i in 2:n) {
      for (j in 2:i) {
        F[i,j] = (F[i,j-1]-F[i-1,j-1])/(x[i]-x[i-j+1])
      }
    }
    ## The coefficients of the Newton Interpolary Divided Difference Formula are
    ##   the diagonal entries of F
    coef=diag(F)
    ## The next two lines use the coefficients to figure out the interpolation.
    ## First line creates a vector of the product of x-xj, then the second
    ##   finds the dot product of them.
    xvec=c(1,cumprod(xs-x[-length(x)]))
    interp=sum(coef*xvec)

    ## Next, it names the columns appropriately.
    names(coef)=paste("a",0:(n-1),sep="")
    dimnames(F)=list(x,c("f(x)",paste(ordinal(1:(n-1)),"DD")))
    return(list(table=F,coef=coef,interp=interp))
}

############################################################################
##
## Stirlings Interpolatory Divided Difference Formula -
##   This evaluates the interpolatory polynomial when the x-value is near
##   the center of the table.  Requires that h is equally spaced.
##   Inputs:
##     A - table containing the x-values and y-values in first and
##         second columns
##     xs - evaluate the polynomial at this point
##   Outputs:
##     s - value of s (xs-f00)/h
##     F - Table holding the divided differences
##     fvals - entries from F (averages of middle two every other time)
##     spart - multipliers based on h and s
##     interp - sum(fvals*spart)  #The interpolated value of f(xs)
##
##  written by Scott Hyde
############################################################################

stirlings = function(A,xs) {
    ##
    ## Inputs
    ##   A = table of nodes with function values
    ##   xs = value to interpolate from the table 
    ##
    ##  Note the index can't use 0 (R starts at 1), so
    ##  everything has to be increased by one (except when
    ##  subtracting i-j, which needs to be increased by 1
    ##  as well
    n=dim(A)[1]-1  
    x=A[,1]
    if ( (n %% 2) == 0 ) {
        iseven = TRUE; m = n/2 
    } else {
        iseven = FALSE; m = (n-1)/2 
    }
    h = diff(A[1:2,1])  #find the difference between first two x's
    s = as.numeric((xs-A[m+1,1])/h)   #find s
    if (!all(diff(A[,1])-h < 1e-10)) stop("Nodes must be equally spaced")

    F=matrix(NA,n+1,n+1)
    F[,1]=A[,2]
   
    for (i in 2:(n+1)) {
      for (j in 2:i) {
        F[i,j] = (F[i,j-1]-F[i-1,j-1])/(x[i]-x[i-j+1])
      }
    }

    # The coefficients of the Stirling's Interpolation method are the
    # two middle (top and bottom) routes that are possible.  You add
    # them and divide by two.  
    # Formulas pick the right elements to extract
    topx=F[c(rep((m+1):n,each=2),n+1)+seq(0,n)*(n+1)]
    botx=F[c(m+1,rep((m+2):(n+1),each=2))+seq(0,n)*(n+1)]
    fbar = (topx+botx)/2
    
    # The next lines use the coefficients to figure out the
    # interpolation.  First line creates a vector of powers of h.
    # spart is either s or s^2 (note that in the formula).
    # s2part are the product of s^2-i^2,
    # interp is the dot product of them all.
    hs = h^(0:n)
    if (iseven) {
        spart = c(1,rep(c(s,s^2),m))
        s2part = c(1,1,1,rep(cumprod(s^2-(1:(m-1))^2),2))
    } else {
        spart = c(1,rep(c(s,s^2),m),s)
        s2part = c(1,1,1,rep(cumprod(s^2-(1:m)^2),2))
        s2part = s2part[-length(s2part)]  #remove last entry   
    }    
    spart=hs*spart*s2part
    interp=sum(fbar*scoef)
    
    names(scoef)=paste("a",0:n,sep="")
    dimnames(F)=list(x,c("f(x)",paste(ordinal(1:n),"DD")))
    return(list(s=s,table=F,fvals=fbar,spart=spart,interp=interp))
}


