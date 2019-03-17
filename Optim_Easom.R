
# values to change
f<-f.e
actual.optim = c(pi,pi)
n    = 40000


conv = numeric(n)
st.x = runif(n,-19,21)
st.y = runif(n,-19,21)

in.pts = cbind(st.x,st.y)

out.pts  = matrix(rep(0,2*n), ncol = 2)
out.grad = out.pts
out.fval = numeric(n)

for ( i in 1:n )
{
  optim.out = optim(in.pts[i,],f)
  if (optim.out$conv == 0) { conv[i] = 1}
  out.pts[i,] = optim.out$par
  out.fval[i] = optim.out$value
  if (out.fval[i]==0){ out.fval[i]=1e-60}
  out.grad[i,] = grad(f,optim.out$par)
}


#Graph shows evidence of convergence (q1)
good = which(conv == 1)
bad  = which(conv == 0)
dev.new()
plot(in.pts,
     main = 'Graphical evidence of convergence',
     xlab = 'first value of input parameter',
     ylab = 'second value of input parameter',
     type = 'n')
points(in.pts[good,],col = 'blue')
points(in.pts[bad,],col = 'red')
legend('bottomright',c('good', 'bad'), cex=0.8, fill=c('blue','red'))

# The proportion of times convergence was reached (q2)
paste(length(which(conv == 1))*100/n,'%', sep="")


#find the error
err = out.pts - matrix(rep(actual.optim,n),ncol = 2, byrow = TRUE)
err.norm = sqrt(rowSums(err^2))


# No. of time the function claim to get converged (q3)
length(which(conv == 1))
# No. of time the function actual did achieve convergence (q3)
tol.1 = length(which(err.norm < 1e-5))
tol.2 = length(which(err.norm < 1e-7))
tol.3 = length(which(err.norm < 1e-9))
tol.1
tol.2
tol.3

####   find actual good, bad

actual.ok = which(err.norm < 1e-5)
actual.good = which(err.norm < 1e-7)
actual.excel = which(err.norm < 1e-9)
actual.bad   = which(err.norm >= 1e-5)

#Graph shows evidence of actual convergence
dev.new()
plot(in.pts, 
     main = 'Graphical evidence of actual convergence',
     xlab = 'first value of input parameter',
     ylab = 'second value of input parameter',
     type = 'n')
points(in.pts[actual.bad,], col = 'yellow',pch = 19)
points(in.pts[actual.ok,], col = 'red',pch = 19)
points(in.pts[actual.good,], col = 'green',pch = 19)
points(in.pts[actual.excel,], col = 'blue',pch = 17)
legend('bottomright',c('bad','ok','good','exel'), cex=0.8, fill=c('yellow','red','green','blue'))

grad.norm = sqrt(rowSums(out.grad^2))
grad.tol.1 = length(which(grad.norm < 1e-6))

# Graph shows the function values when actual convergence happens (q4)
dev.new();plot(ceiling(log10(out.fval)),
               main = 'Function Values for Actual Convergence',
               xlab = 'fval index',
               ylab = 'function values (1e)')
points(ceiling(log10(out.fval[actual.bad])),col = 'yellow', cex=0.8,pch = 19)
points(ceiling(log10(out.fval[actual.ok])), col = 'red', cex=0.8,pch = 19)
points(ceiling(log10(out.fval[actual.good])), col = 'green', cex=0.8,pch = 19)
points(ceiling(log10(out.fval[actual.excel])), col = 'blue', cex=0.8,pch = 19)
legend('bottomright',c('bad','ok','good','exel'), cex=0.8, fill=c('yellow','red','green','blue'))

# Graph shows the function values when the function falsely claimed to achieve convergence (q5)
dev.new();plot(ceiling(log(out.fval)),
               main = 'Function Values for Unconvergence',
               xlab = 'fval index',
               ylab = 'function values (1e)')
points(ceiling(log10(out.fval[bad])), col = 'red',cex=1,pch = 19)
legend('bottomright',c('bad'), cex=0.8, fill=c('red'))

# In.pts, out.pts, grad, fval graphs (q6)
dev.new();plot(in.pts,
               main = 'Initial Values',
               xlab = 'value of first parameter found',
               ylab = 'value of second parameter found',
               cex=0.4)
dev.new();plot(out.pts,
               main = 'Final Values',
               xlab = 'value of first parameter found',
               ylab = 'value of second parameter found',
               cex = 0.4)
dev.new();plot(out.grad,
               main = 'Gradient Graph',
               xlab = 'gradient value of first parameter',
               ylab = 'gradient value of second parameter',
               cex = 0.4)
dev.new();plot(ceiling(log10(out.fval)),
               main = 'Function Values Graph',
               xlab = 'fval index',
               ylab = 'function values (1e)',
               cex=0.4)
