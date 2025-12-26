##########################################################################
options(digits=16)

#Check the examples from Chapter 4.7, at the end of the section
f=function(x) { exp(-x^2) }
gaussianquad(1,1.5,f,2)
gaussianquad(1,1.5,f,3)
gaussianquad(1,1.5,f,10)

f=function(x) { log(x)^2 }
# Real answer to integral is 2
gaussianquad(0,1,f,20)
midpt(0,1,f,10000000)
midpt(0,1,f,20)
trapezoid(0,1,f,10)
simpsons(0,1,f,10)

f=function(x) { exp(x)/sqrt(x) }
gaussianquad(0,1,f,20)
midpt(0,1,f,10000)
# real answer is 2.9253

##########################################################################
