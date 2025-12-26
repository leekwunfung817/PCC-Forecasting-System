library(english)
##
##
## Adaptive Quadrature
##
##

adaptive = function(a,b,f,tol=1e-4,n=8) {
    ## Direct implementation of algorithm 4.2 from Burden/Faires 5th
    ## Ed.
    ## Inputs:
    ##   a,b  end points of interval to be integrated
    ##   f    function to integrate
    ##   dtol tolerance of integral
    ##   n    number of levels

    app = 0
    i = 1
    dtol=tol
    tol=aa=h=fa=fc=fb=S=L=0
    tol[i] = 10*dtol
    aa[i] = a
    h[i] = (b-a)/2
    fa[i] = f(a)
    fc[i] = f(a+h[i])
    fb[i] = f(b)
    S[i] = h[i]*(fa[i]+4*fc[i]+fb[i])/3
    L[i] = 1
    funcevals=3
    while ( i > 0 ) {
        fd = f(aa[i]+h[i]/2)
        fe = f(aa[i]+3*h[i]/2)
        funcevals = funcevals + 2
        S1 = h[i]*(fa[i] + 4*fd + fc[i])/6
        S2 = h[i]*(fc[i] + 4*fe + fb[i])/6
        v1 = aa[i]
        v2 = fa[i]
        v3 = fc[i]
        v4 = fb[i]
        v5 = h[i]
        v6 = tol[i]
        v7 = S[i]
        v8 = L[i]
        i = i - 1
        if ( abs( S1 + S2 - v7 ) < v6 ) {
            app = app + ( S1 + S2 )
        }
        else if (v8 >= n) stop("Level Exceeded")
        else {
            i = i + 1
            aa[i] = v1 + v5
            fa[i] = v3
            fc[i] = fe
            fb[i] = v4
            h[i] = v5/2
            tol[i] = v6/2
            S[i] = S2
            L[i] = v8 + 1
            i = i + 1
            aa[i] = v1
            fa[i] = v2
            fc[i] = fd
            fb[i] = v3
            h[i] = h[i-1]
            tol[i] = tol[i-1]
            S[i] = S1
            L[i] = L[i-1]
        }
    }
    return(list(xs=aa,intvalue=app,functevals=funcevals,levels=L))
}

compgauss = function(f,a,b,p,n) { 
  x=seq(a,b,len=n)
  sum=0
  for (i in 1:(n-1)) {
    sum = sum + gauss(f,x[i],x[i+1],p)
  }
  return(sum);
}



gauss <- function(f,a,b,n) {
  ## This performs gaussian integration of f from a to b using the
  ##   eigenvalue decomp.
  ##
  ## From "Is Gauss Quadrature Better than
  ##   Clenshaw-Curtis?" by Lloyd N. Trefethen
  beta = .5/sqrt(1-(2*(1:(n-1)))^(-2))  ##3-term recurrence coeffs
  foo = bandSparse(n,k=-1,diag=as.matrix(beta),symm=TRUE) ##Jacobi matrix
  mat = eigen(foo)
  sorteig = sort(mat$values,index.return=TRUE) ## nodes (Legendre points)
  w = 2*mat$vectors[1,sorteig$ix]^2            ## weights
  m = (b-a)/2
  as.numeric(m*crossprod(w,f(m*sorteig$x+m+a)))## integral
}
    
  
clenshaw_curtis <- function(f,a,b,n) {
  ## This performs Clenshaw Curtis quadrature of f from a to b using the
  ##   fast fourier transformation which is O(nlogn)
  ##
  ## From "Is Gauss Quadrature Better than
  ##   Clenshaw-Curtis?" by Lloyd N. Trefethen
  m = (b-a)/2
  x = cos(pi*(0:n)/n)    ##Chebychev points
  fx = f(m*x+m+a)/(2*n)  ##f(x)
  g = Re(fft(fx[c(seq(1,n+1),seq(n,2))]))   ##Fast Fourier Transform
  a = c(g[1],g[2:n]+g[seq(2*n,n+2)],g[n+1]) ##Chebychev coeffs
  w = rep(0,length(a))
  w[seq(1,n+1,by=2)] = 2/(1-(seq(0,n,by=2))^2) ##weight vector
  as.numeric(m*crossprod(w,a))
}
  
##
## Note that both of these were adapted from the interval from a to b
## by using the transformation u=(b-a)/2*x + (a+b)/2, and du=(b-a)/2 dx
## This changes the integral to be from x=-1 to x=1, which is what is
## needed.  In particular:
##
##  int_a^b f(u) du = (b-a)/2*int_{-1}^1 f((b-a)/2 x + (b+a)/2)
##
## if you define m=(b-a)/2, then (a+b)/2 = m+a and
##
##  int_a^b f(u) du = m*int_{-1}^1 f(mx + m + a)
##                  = m*int_{-1}^1 f((m+1)x + a)
##
##########################################################################
#
# Gaussian Quadrature
#   Implements Gaussian Quadrature with static weights.  The acutal
# function is quite small once we have the weights and abscissa!
#   Written by Scott Hyde
##########################################################################

####################
### See gaussianquad function and examples below!!! 
#####################

getweights = function(n) {
    ## Keep a static number of these to pick from quick
    ## The first column of each matrix are the weights, and the second
    ## the abscissa.  The code makes use of the property that the
    ## roots are +-, so they only store the positive ones, but
    ## reconstruct the negative ones before passing back.
    stuff=switch(n,,
           matrix(c(1.0000000000000000,0.5773502691896257),ncol=2,byrow=T),
           matrix(c(0.8888888888888888,0.0000000000000000,
                    0.5555555555555556,0.7745966692414834),ncol=2,byrow=T),
           matrix(c(0.6521451548625461,0.3399810435848563,
                    0.3478548451374538,0.8611363115940526),ncol=2,byrow=T),
           matrix(c(0.5688888888888889,0.0000000000000000,
                    0.4786286704993665,0.5384693101056831,
                    0.2369268850561891,0.9061798459386640),ncol=2,byrow=T),
           matrix(c(0.3607615730481386,0.6612093864662645,
                    0.4679139345726910,0.2386191860831969,
                    0.1713244923791704,0.9324695142031521),ncol=2,byrow=T),
           matrix(c(0.4179591836734694,0.0000000000000000,
                    0.3818300505051189,0.4058451513773972,
                    0.2797053914892766,0.7415311855993945,
                    0.1294849661688697,0.9491079123427585),ncol=2,byrow=T),
           matrix(c(0.3626837833783620,0.1834346424956498,
                    0.3137066458778873,0.5255324099163290,
                    0.2223810344533745,0.7966664774136267,
                    0.1012285362903763,0.9602898564975363),ncol=2,byrow=T),
           matrix(c(0.3302393550012598,0.0000000000000000,
                    0.1806481606948574,0.8360311073266358,
                    0.0812743883615744,0.9681602395076261,
                    0.3123470770400029,0.3242534234038089,
                    0.2606106964029354,0.6133714327005904),ncol=2,byrow=T),
           matrix(c(0.2955242247147529,0.1488743389816312,
                    0.2692667193099963,0.4333953941292472,
                    0.2190863625159820,0.6794095682990244,
                    0.1494513491505806,0.8650633666889845,
                    0.0666713443086881,0.9739065285171717),ncol=2,byrow=T),
           matrix(c(0.2729250867779006,0.0000000000000000,
                    0.2628045445102467,0.2695431559523450,
                    0.2331937645919905,0.5190961292068118,
                    0.1862902109277343,0.7301520055740494,
                    0.1255803694649046,0.8870625997680953,
                    0.0556685671161737,0.9782286581460570),ncol=2,byrow=T),
           matrix(c(0.2491470458134028,0.1252334085114689,
                    0.2334925365383548,0.3678314989981802,
                    0.2031674267230659,0.5873179542866175,
                    0.1600783285433462,0.7699026741943047,
                    0.1069393259953184,0.9041172563704749,
                    0.0471753363865118,0.9815606342467192),ncol=2,byrow=T),
           matrix(c(0.2325515532308739,0.0000000000000000,
                    0.2262831802628972,0.2304583159551348,
                    0.2078160475368885,0.4484927510364469,
                    0.1781459807619457,0.6423493394403402,
                    0.1388735102197872,0.8015780907333099,
                    0.0921214998377285,0.9175983992229779,
                    0.0404840047653159,0.9841830547185881),ncol=2,byrow=T),
           matrix(c(0.2152638534631578,0.1080549487073437,
                    0.2051984637212956,0.3191123689278897,
                    0.1855383974779378,0.5152486363581541,
                    0.1572031671581935,0.6872929048116855,
                    0.1215185706879032,0.8272013150697650,
                    0.0801580871597602,0.9284348836635735,
                    0.0351194603317519,0.9862838086968123),ncol=2,byrow=T),
           matrix(c(0.2025782419255613,0.0000000000000000,
                    0.1984314853271116,0.2011940939974345,
                    0.1861610000155622,0.3941513470775634,
                    0.1662692058169939,0.5709721726085388,
                    0.1395706779261543,0.7244177313601701,
                    0.1071592204671719,0.8482065834104272,
                    0.0703660474881081,0.9372733924007060,
                    0.0307532419961173,0.9879925180204854),ncol=2,byrow=T),
           matrix(c(0.1894506104550685,0.0950125098376374,
                    0.1826034150449236,0.2816035507792589,
                    0.1691565193950025,0.4580167776572274,
                    0.1495959888165767,0.6178762444026438,
                    0.1246289712555339,0.7554044083550030,
                    0.0951585116824928,0.8656312023878318,
                    0.0622535239386479,0.9445750230732326,
                    0.0271524594117541,0.9894009349916499),ncol=2,byrow=T),
           matrix(c(0.1794464703562065,0.0000000000000000,
                    0.1765627053669926,0.1784841814958479,
                    0.1680041021564500,0.3512317634538763,
                    0.1540457610768103,0.5126905370864769,
                    0.1351363684685255,0.6576711592166907,
                    0.1118838471934040,0.7815140038968014,
                    0.0850361483171792,0.8802391537269859,
                    0.0554595293739872,0.9506755217687678,
                    0.0241483028685479,0.9905754753144174),ncol=2,byrow=T),
           matrix(c(0.1691423829631436,0.0847750130417353,
                    0.1642764837458327,0.2518862256915055,
                    0.1546846751262652,0.4117511614628426,
                    0.1406429146706507,0.5597708310739475,
                    0.1225552067114785,0.6916870430603532,
                    0.1009420441062872,0.8037049589725231,
                    0.0764257302548891,0.8926024664975557,
                    0.0497145488949698,0.9558239495713977,
                    0.0216160135264833,0.9915651684209309),ncol=2,byrow=T),
           matrix(c(0.1610544498487837,0.0000000000000000,
                    0.1589688433939543,0.1603586456402254,
                    0.1527660420658597,0.3165640999636298,
                    0.1426067021736066,0.4645707413759609,
                    0.1287539625393362,0.6005453046616810,
                    0.1115666455473340,0.7209661773352294,
                    0.0914900216224500,0.8227146565371428,
                    0.0690445427376412,0.9031559036148179,
                    0.0448142267656996,0.9602081521348300,
                    0.0194617882297265,0.9924068438435844),ncol=2,byrow=T),
           matrix(c(0.1527533871307258,0.0765265211334973,
                    0.1491729864726037,0.2277858511416451,
                    0.1420961093183820,0.3737060887154195,
                    0.1316886384491766,0.5108670019508271,
                    0.1181945319615184,0.6360536807265150,
                    0.1019301198172404,0.7463319064601508,
                    0.0832767415767048,0.8391169718222188,
                    0.0626720483341091,0.9122344282513259,
                    0.0406014298003869,0.9639719272779138,
                    0.0176140071391521,0.9931285991850949),ncol=2,byrow=T))
    nz=stuff[stuff[,2]!=0,,drop=FALSE] #remove zero root
    nz[,2]=-nz[,2]                     #add neg roots
    cnw = rbind(stuff,nz)              #combine together
    return(cnw)
}


gaussianquad = function(a,b,f,n) {
    ## Gaussian Quadrature
    ##    Inputs
    ##      a,b = bounds of integration
    ##      f   = function to integrate
    ##      n   = number of points to use
    stuff = getweights(n)
    wi = stuff[,1]
    ti = stuff[,2]
    xi = (b-a)/2*ti+(b+a)/2;
    int = (b-a)/2*sum(wi*f(xi))
    return(int)
}

##########################################################################
#
# Gaussian Chebyshev Quadrature
#   
##########################################################################

gausschebyshev = function(f,n) {
    ## The abscissa and weights are REALLY easy here
    ## The weights are pi/n and the abscissa are cos((2i-1)/(2n)*pi)
    xs = cos((2*(1:n)-1)/(2*n)*pi)
    return(pi/n*sum(f(xs)))
}
############################################################################
#
# Midpoint Quadrature - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

midpt = function(a,b,f,n=10,r,plot=FALSE,toscale=FALSE) {
    # Composite Midpt rule
    # Inputs
    #   a,b  = endpoints
    #   f    = function to integrate
    #   n    = number of inside interval points (should be even)
    #          Leads to (n+2)/2 rectangles.  The width of the
    #          rectangles is double of the trapezoid rule.
    #          To compare for a similar width of rec/trap, use
    #          the r optional argument below.
    #   r    = optional argument (number of rectangles)
    #   plot = plot the integral
    #   toscale = keep the height and width to scale
    if ( (n %% 2) == 1 ) stop("n must be even")
    if (!missing(r)) n = 2*r-2  #specify number or rectangles instead
    h = (b-a)/(n+2)  #smallest open Newton-Cote's method h
    x = seq(a+h,b-h,by=2*h)
    intg=2*h*sum(f(x))
    if (plot) {
        rule=paste("Midpoint Rule","\nArea = ",intg,sep="") 
        if ( (n %% 2) == 1 ) stop("n must be even")
        ##n is the num of inside partition points, not numb of rectangles
        if (missing(r)) n = (n+2)/2  else n = r
        if (toscale)
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),asp=1,main=rule)
        else
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),main=rule)
        h = (b-a)/n
        for (i in 0:(n-1) ) {
            xj = a + i*h
            xs = rep(c(xj,xj+h),each=2)
            ys = c(0,rep(f(xj+.5*h),2),0)
            polygon(xs,ys,col=rgb(0,0,1.0,alpha=.5))
        }
    }
    return(intg)
}

############################################################################
#
# Romberg Integration - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

romberg = function(a,b,f,n=5) {
    #
    # Inputs
    #   a,b = endpoints of interval
    #   f   = function to integrate
    #   n   = number of rows of Romberg to generate
    h = b-a
    R = matrix(NA,n,n)
    R[1,1] = h*(f(a)+f(b))/2 #Trapezoid Rule n=1
    for (i in 2:n) {
        k=1:2^(i-2) #k is a vector
        R[i,1]=1/2*(R[i-1,1]+h*sum(f(a+(k-.5)*h)))
        for (j in 2:i) {
            R[i,j] = R[i,j-1] + (R[i,j-1] - R[i-1,j-1])/(4^(j-1)-1)
        }
        h = h/2
    }
    return(list(R=R,intg=R[n,n])) #return the R matrix and best approx
}

############################################################################
#
# Simpson's Quadrature - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

simpsons = function(a,b,f,n=10,plot=FALSE,toscale=FALSE) {
    # Composite Simpson's rule
    # Inputs
    #   a,b  = endpoints
    #   f    = function to integrate
    #   n    = number of intervals to compute (must be even)
    #   plot = plot the integral
    #   toscale = keep the height and width to scale
    if ( (n %% 2) == 1 ) stop("n must be even")
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(c(4,2),(n-2)/2),4,1)
    intg=h/3*sum(coef*f(x))
    if (plot) {
        rule=paste("Simpson's Rule","\nArea = ",intg,sep="") 
        if (toscale)
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),asp=1,main=rule)
        else
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),main=rule)
        h = (b-a)/n
        for (i in seq(0,n-2,by=2) ) {
            xj = a + i*h
            xpts = c(xj,xj+h,xj+2*h)
            ypts=f(xpts)
            coef=solve(cbind(1,xpts,xpts^2),ypts)  #solving for coefs of fitted quadratic
            xx=seq(xj,xj+2*h,len=51)
            xs=c(xj,xx,xj+2*h)
            ys=c(0,as.vector(c(1,1,1)%*%(coef*rbind(1,xx,xx^2))),0) #points of fitted quadratic
            polygon(xs,ys,col=rgb(0,0,1.0,alpha=.5))
        }
    }
    return(intg)
}
############################################################################
#
# Trapezoid Quadrature - This evaluates the integral of f over the
#   interval [a,b] using n as the number of nodes evaluated
#  written by Scott Hyde
############################################################################

trapezoid = function(a,b,f,n=10,plot=FALSE,toscale=FALSE) {
    # Composite Trapezoid rule
    # Inputs
    #   a,b  = endpoints
    #   f    = function to integrate
    #   n    = number of intervals to compute
    #   plot = plot the integral
    #   toscale = keep the height and width to scale
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(2,n-1),1)
    intg=h/2*sum(coef*f(x))
    if (plot) {
        rule=paste("Trapezoid Rule","\nArea = ",intg,sep="") 
        if (toscale)
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),asp=1,main=rule)
        else
            curve(f(x),xlim=c(a,b),xaxp=c(a,b,n),main=rule)
        h = (b-a)/n
        for (i in 0:(n-1) ) {
            xj = a + i*h
            xs = rep(c(xj,xj+h),each=2)
            ys = c(0,f(c(xj,xj+h)),0)
            polygon(xs,ys,col=rgb(0,0,1.0,alpha=.5))
        }
    }
    return(intg)
}

################################################################
## 
## Elementary implementation of some Newton-Cotes Composite Methods
##   Given for ease of understanding
##
################################################################

tiny_midpt = function(a,b,f,n) {
    # Composite Midpt rule
    # Inputs
    #   a,b = endpoints
    #   f   = function to integrate
    #   n   = number of intervals to compute (should be even)
    if ( (n %% 2) == 1 ) stop("n must be even")
    h = (b-a)/n
    x = seq(a+h,b-h,by=2*h)
    return(2*h*sum(f(x)))
}

tiny_trapezoid = function(a,b,f,n) {
    # Composite Trapezoid rule
    # Inputs
    #   a,b = endpoints
    #   f   = function to integrate
    #   n   = number of intervals to compute
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(2,n-1),1)
    return(h/2*sum(coef*f(x)))
}

tiny_simpsons = function(a,b,f,n) {
    # Composite Simpson's rule
    # Inputs
    #   a,b = endpoints
    #   f   = function to integrate
    #   n   = number of intervals to compute (must be even)
    if ( (n %% 2) == 1 ) stop("n must be even")
    h = (b-a)/n
    x = seq(a,b,by=h)
    coef = c(1,rep(c(4,2),(n-2)/2),4,1)
    return(h/3*sum(coef*f(x)))
}

