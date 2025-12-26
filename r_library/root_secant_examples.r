############################################################################
## Testing the code
## Function to find the zero of
f = function(x) { x^2-3 }

## Examples
bb=secant(1,2,f)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f"))

secant(1,2,f,1e-15,100) 
############################################################################
