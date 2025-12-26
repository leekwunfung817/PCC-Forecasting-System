############################################################################
#
# Newton's Interpolatory Divided Difference Formula -
#   This returns the coefficients of the interpolatory polynomial P
#   where f is stored in table form as A, which contains the 
#     x-values and f(x-values) in the first and second columns
#   It will also evaluate the polynomial at a certain point.
#  written by Scott Hyde
############################################################################

nidd = function(A,xs) {
    ##
    ## Inputs
    ##   A = table of nodes with function values
    ##   xs = value to interpolate from the table 
    ##
    ## Note that n below is really n+1 from algorithm
    ##  This is because R does not use 0 as an index, so
    ##  everything has to be increased by one (except when
    ##  subtracting i-j, which needs to be increased by 1
    ##  as well
    n=dim(A)[1]  
    x=A[,1]
    F=matrix(NA,n,n)
    F[,1]=A[,2]
    
    for (i in 2:n) {
      for (j in 2:i) {
        F[i,j] = (F[i,j-1]-F[i-1,j-1])/(x[i]-x[i-j+1])
      }
    }

    # The coefficients of the Newton Interpolary Divided Difference
    # Formula are the diagonal entries of F
    coef=diag(F)
    # The next two lines use the coefficients to figure out the
    # interpolation.  First line creates a vector of the product of
    # x-xj, then the second finds the dot product of them.
    xvec=c(1,cumprod(xs-x[-length(x)]))
    interp=sum(coef*xvec)

    names(coef)=paste("a",0:(n-1),sep="")
    dimnames(F)=list(x,c("f(x)",paste(ordinal(1:(n-1)),"DD")))
    return(list(table=F,coef=coef,interp=interp))
}

