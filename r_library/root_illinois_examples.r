############################################################################
## Testing the code
## Function to find the zero of
options(width=200)
f = function(x) { x^4+2*x^2-3*x-3 }

## Examples
bb=illinois(1,2,f)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f"))

############################################################################
