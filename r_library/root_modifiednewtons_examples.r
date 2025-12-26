############################################################################
## Testing the code
## Function to find the zero of
f = function(x) { 1-cos(2*x) }
fp = function(x) { 2*sin(2*x)  }
fp2 = function(x) { 4*cos(2*x) }

## Examples:
bb=mnewtons(1,f,fp,fp2)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f"))

mnewtons(1,f,fp,fp2,1e-16,100)
############################################################################

