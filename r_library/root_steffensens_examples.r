############################################################################
## Testing the code
## Function to find the fixed point of
g = function(x) { cos(x) }

bb=steff(1,g)
bb            ##this shows iterations and zero
bb$zero       ##this shows just the zero
bb$iterations ##this shows just the iterations

#If you want to display the error as a regular decimal, then do this:
noquote(formatC(bb$iterations,digits=11,format="f"))

steff(1,g,1e-15,100)
steff(1,g,1e-15,100,complete=TRUE)

