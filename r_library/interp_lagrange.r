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
        Lnk[i] = paste(coef[i],pt,collapse="*",sep="*")
    }
    lagr = parse(text=paste(Lnk,collapse="+"))[[1]]
    return(make_function(alist(x=), lagr))
}
