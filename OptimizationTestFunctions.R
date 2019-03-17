########################################################
#
#       This file contains some popular functions
#       to test unconstrained optimization functions.
#
#       These functions have minimum, but may not
#       have maximum, values
#
#######################################################

####  Beale's function

####  min value at (3,0.5) of 0

f.beale <- function(x)
  {
    return( (1.5   - x[1] + x[1]*x[2])^2 
         +  (2.25  - x[1] + x[1]*x[2]^2)^2
         +  (2.625 - x[1] + x[1]*x[2]^3)^2 )
  }

####  Booth's function

####  min value at (1,3) of 0

f.booth <- function(x)
{
  return( (x[1] + 2*x[2] - 7)^2 
          +  (2*x[1] + x[2] - 5)^2
           )
}

####  Matya's function

####  minimum value at (0,0) of 0
f.matya <- function(x)
{
  return( 0.26*(x[1]^2 + x[2]^2) -0.48*x[1]*x[2])
}

#### Rosenbrock's function

#### minimum value at (1,1) of 0
f.r <- function(x)
{
   return(100*(x[2] - x[1]^2)^2 + (1 - x[1])^2  )
}


#####  Easom function     root at (pi,pi)

#####  minimum value at (pi,pi) of -1
#####  function also contains many local minima
#####  with value 0. The minima accur in a regular pattern.

f.e<- function(x) 
{
  return( -cos(x[1])*cos(x[2])*exp(-( (x[1]-pi)^2 + (x[2] -pi)^2) ) + 1 )
}

