############################################################################
## Testing the code
## Function to find the zero of
f = function(x) { x^4+3*x^2-2*x-20}

## Examples
bb=brent(-5,0,f)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f"))

brent(-5,0,f,1e-15)
############################################################################
g=function(x) {atan(x)+1}
brent(-100,100,g,eps=1e-16)
############################################################################
