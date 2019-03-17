############################################################
#
#          grad function
#
#      input args:
#
#                    f,   a function
#                    x,   a point
#                    h,   a parameter used to determine
#                         the difference between points
#                         used to evaluate f when calculating
#                         the gradient
#
#     The function grad returns a vector containing the
#     computed gradient of the function f evaluated at x
#
#############################################################

grad <- function(f,x,h = 1/8)
{ # start grad
  
  n        = length(x)
  gradient = numeric(n)
  dir      = diag(rep(1,n))
  for ( i in 1:n )
  {
    v     = dir[i,]        # direction we move for derivative wrt x[i]
    f.m2h = f(x-2*h*v)
    f.mh  = f(x-h*v)
    f.ph  = f(x+h*v)
    f.p2h = f(x + 2*h*v)
    #  compute derivative using h
    r1    = (f.m2h -8*f.mh +8*f.ph - f.p2h)/(12*h)
    f.mhdiv2 = f(x - 0.5*h*v)
    f.phdiv2 = f(x + 0.5*h*v)
    #  compute derivative using h/2
    r2       = (f.mh -8*f.mhdiv2 + 8*f.phdiv2 - f.ph)/(6*h)
    # use one step of Richardson
    gradient[i] = (16*r2 - r1)/15
  }
  return(gradient)
} # end grad   

