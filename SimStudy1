###### Simulation Studies

# libraries
library(MASS)

n = 20
bias = 0#10

lambda = 1#1.05
rho = 0#0.83
Psi = diag( 924 * c(1,lambda) )
Psi[1,2] = Psi[2,1] = rho * sqrt(Psi[1,1] * Psi[2,2])

upsilon = 1#2.22
gamma = 0#0.3
Sigma = diag( 37.4 * c(1,upsilon) )
Sigma[1,2] = Sigma[2,1] = gamma * sqrt(Sigma[1,1] * Sigma[2,2])

Z = cbind(rep(c(1,0),2), rep(c(0,1),2))

b = mvrnorm(n, mu = rep(0,2), Sigma = Sigma)
e = c(t(mvrnorm(2*n, mu = rep(0,2), Sigma = Sigma)))
Y = rep(c(100,100+bias),2*n) + c(Z%*% t(b)) + e
dat = data.frame(Y, Subject= rep(seq(n), rep(4,n)),
 Method= rep(c("Standard","New"),2*n), Replication= rep(c(1,1,2,2),n))

x = unlist(subset(dat, subset= Method == "Standard", select = Y))
y = unlist(subset(dat, subset= Method == "New", select = Y))

plot(x,y, pch= 16, cex=0.5, col = rep((1:20), rep(2,20)))
abline(c(0,1), col="grey")
for(i in 1:n) segments(x[2*i -1],y[2*i -1],x[2*i],y[2*i], lwd=0.8, lty=1, col = i)
