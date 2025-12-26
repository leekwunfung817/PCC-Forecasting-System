############################################################################
## Testing the code
## Function to find the zero of
f = function(x) { x^2-3 }

## Examples:
bb=bisection(1,2,f)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f")) 

bisection(1,2,f,1e-15)     ## fails to convert after 30 iterations
bisection(1,2,f,1e-15,100) ## give it more
############################################################################
