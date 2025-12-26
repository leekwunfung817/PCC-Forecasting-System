############################################################################
#
# Neville's Method - this function interpolates the value f(xs),
#   where xs, where f is stored in table form as A, which contains the 
#     x-values and f(x-values) in the first and second columns 
#  written by Scott Hyde
############################################################################

neville = function(A,xs,nodes=0:(dim(A)[1]-1)) {
  ##
  ## Inputs
  ##   A = table of nodes with function values
  ##   xs = value to interpolate from the table 
  ##   nodes = nodes to use for the approximation.  Should be
  ##        created using :, so 2:4, 1:3, 0:2 (Uses zero index, so
  ##        first node is zero.
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
