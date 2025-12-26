############################################################################
## Example
f=function(x) { x+sin(7*x) } 
trapezoid(0,3,f)
trapezoid(0,3,f,10,plot=TRUE)
trapezoid(0,3,f,100,plot=TRUE)
real = -cos(7*3)/7+1/7+9/2
real

abserror=real-trapezoid(0,3,f)
abserror

abserror=real-trapezoid(0,3,f,1000)
abserror #midpt is better (if counting rectanlges/trapezoids)


## Trapezoid is really good at periodic functions integrated over their period.
##  In fact, it's exact.
f=function(x) {sin(x)+cos(2*x)+2}
trapezoid(-pi/2,3*pi/2,f,2,plot=TRUE)
trapezoid(-pi/2,3*pi/2,f,3,plot=TRUE)
trapezoid(-pi/2,3*pi/2,f,4,plot=TRUE)
trapezoid(-pi/2,3*pi/2,f,40,plot=TRUE)
trapezoid(0,24*pi,cos,10,plot=TRUE)


############################################################################
