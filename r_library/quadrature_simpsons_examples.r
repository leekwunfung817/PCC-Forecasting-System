##########################################################
## Example
f=function(x) { x+sin(7*x) }
simpsons(0,3,f)
simpsons(0,3,f,plot=TRUE)
simpsons(0,3,f,50,plot=TRUE)
real = -cos(7*3)/7+1/7+9/2
real

abserror=real-simpsons(0,3,f)
abserror #wow!  better than midpt and trap!

abserror=real-simpsons(0,3,f,1000)
abserror # way better than midpt and trap!
##########################################################
