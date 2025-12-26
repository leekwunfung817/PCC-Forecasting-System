##########################################################################
#
# Gaussian Chebyshev Quadrature
#   
##########################################################################

g=function(x) { tan(x)^2 }
gausschebyshev(g,3)

results=matrix(rep(0,30),ncol=1)
for (i in 1:30)
    results[i,1]=gausschebyshev(g,i)
results  #it doesn't change beyond 20!

gausschebyshev(g,20)
 
##########################################################################
