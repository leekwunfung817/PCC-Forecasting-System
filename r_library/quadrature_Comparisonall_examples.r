G=function(x) { y=(exp(x)-(1+x+x^2/2+x^3/6+x^4/24))/sqrt(x)*(x!=0); y[x==0]=0;y  }
f=function(x) { sqrt(x)*(x^5-3*x^2+x-2) }
plot(f,1,2)

f=function(x) { y=sin(x)/x*(x!=0); y[x==0]=1;y }

real=0.94608307036718301 #From Wolfram Alpha
midpt(0,1,f,10,plot=TRUE)-real
trapezoid(0,1,f,10,plot=TRUE)-real
midpt(0,1,f,r=10,plot=TRUE)-real
simpsons(0,1,f,10,plot=TRUE)-real
romberg(0,1,f)$intg-real  # 2^5 Trapezoid evals!  How does Trapezoid do with 32?
trapezoid(0,1,f,32)-real  #terrible!!  Romberg sorcery!
gaussianquad(0,1,f,2)-real
gaussianquad(0,1,f,3)-real
gaussianquad(0,1,f,4)-real  # How many to beat romberg?
gaussianquad(0,1,f,5)-real  # How many to beat romberg?
gaussianquad(0,1,f,6)-real  # How many to beat romberg?  6!!



#Note that Midpt and Trapezoid can be used together to get a better estimate!
#When you look at the errors, Midpt is half of Trapezoid and of opposite sign
#Thus, 2*(M-real)+(T-real) = small.   So 2*M+T - 3real = small and
#  real approx (2*M+T)/3
(2*(midpt(0,1,f,r=10))+(trapezoid(0,1,f,10)))/3-real  #Note how good!!!
# error is 6.15*10^-9, where they were
#  M = 1.255*10^-4 and T=-2.510*10^-4 individually.
# If you compare to the errror of Simpson's Rule with n=20, what do you conclude?
simpsons(0,1,f,20,plot=TRUE)-real
