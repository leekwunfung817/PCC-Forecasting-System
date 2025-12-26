############################################################################
## Testing the code
## Function to find the zero of
f = function(x) { x^2-3 }
fp = function(x) { 2*x }

## Examples:
bb=newtons(1,f,fp)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f"))

newtons(1,f,fp,1e-15)
newtons(1,f,fp,1e-15,100)
############################################################################
