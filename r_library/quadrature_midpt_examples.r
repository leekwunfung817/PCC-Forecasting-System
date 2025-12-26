############################################################################
## Examples
f=function(x) { x+sin(7*x) }
midpt(0,3,f)
midpt(0,3,f,10,plot=TRUE)
midpt(0,3,f,r=10,plot=TRUE)
real = -cos(7*3)/7+1/7+9/2
real

abserror = real-midpt(0,3,f)
abserror

abserror = real-midpt(0,3,f,r=10)
abserror #better than trap if width of rectangles is the same!!

abserror = real-midpt(0,3,f,1000)
abserror #trap is better with same n

abserror = real-midpt(0,3,f,r=1000)
abserror #midpt is better if width of rectangles is the same!!
############################################################################

