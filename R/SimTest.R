#source("MCSprep.R")
source("Functions2.R")
source("Blood Data.R")
source("sourcetest.R")
n = 55
bias = 15.6196
####################################################################
#Constructing the D matrix

lambda = 1.0512
Psi = diag( 923.99 * c(1,lambda) )
Psi

rho = 0.828
Psi[1,2] = Psi[2,1] = rho * sqrt(Psi[1,1] * Psi[2,2])
Psi
####################################################################

upsilon = 2.12
gamma = 0.3
Sigma = diag( 37.4 * c(1,upsilon) )
Sigma[1,2] = Sigma[2,1] = gamma * sqrt(Sigma[1,1] * Sigma[2,2])

####################################################################
Z = cbind(rep(c(1,0),2), rep(c(0,1),2))

b = mvrnorm(n, mu = rep(0,2), Sigma = Sigma) 
e = c(t(mvrnorm(2*n, mu = rep(0,2), Sigma = Sigma))) 
Y = rep(c(100,100+bias),2*n) + c(Z%*% t(b)) + e 
dat = groupedData( Y ~ meth | item ,data = data.frame(Y, item= rep(seq(n), rep(4,n)),  
meth= rep(c("Standard","New"),2*n), 
repl= rep(c(1,1,2,2),n)))
####################################################################
source("Candidate Models.R")
source("Estimates.R")
source("fitoutput.R")
source("joinplot.R")
