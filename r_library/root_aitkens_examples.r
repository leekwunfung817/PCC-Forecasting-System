############################################################################
## Test of fixed point for Aitken's Delta Squared Method.  Comparison
## to normal fixed point
g = function(x) { .5*sqrt(10-x^3) }

#Next few lines implement the fixed point method
#  (but it doesn't check for convergence)
iter = 30 #number of iterations
fixpt = 1 #fixpt=initial guess; values after will be added
for (i in 2:iter) fixpt[i] = g(fixpt[i-1]) #fast-no check-fixed point method
phat = aitkens(fixpt)

#Compare the fixpt with Aitkens (NA is added to end of phat so they
#have the same size
p=1; for (i in 2:300) p=g(p)  # find the fixed pt to analyze the absolute error

compare = cbind(fixpt,abs(fixpt-p),c(phat,NA,NA),abs(c(phat,NA,NA)-p))
colnames(compare)=c("fixed pt", "abs err", "Aitkens", "abs error")

options(width=100)
noquote(formatC(compare,digits=18,format="f"))
############################################################################
