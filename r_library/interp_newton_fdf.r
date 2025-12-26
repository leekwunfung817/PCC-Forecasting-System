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
