
F=rnorm(20,100,3)
C=rnorm(20,100,3)
n=length(F)
D=F-C
A=(F+C)/2
Dbar=mean(D)



############################################################################
X=cbind(1,A)
XT=t(X)
XTX=XT%*%X
XXT=X%*%XT
XTXI=solve(XTX)

H=X%*%XTXI%*%XT
H
fit=lm(D~A)

intercept=summary(fit)$coefficients[1]
slope=summary(fit)$coefficients[2]
Dfit=intercept+(slope*A)
RES = cbind(D,Dfit,D-Dfit)
RES

influence.measures(fit)



#####################

Diag=matrix(0,n,7)

for (Q in 1:n)
{
W=A[-Q]
E=D[-Q]
fit2=lm(E~W)

Diag[Q,1]=summary(fit2)$coefficients[1]
Diag[Q,2]=summary(fit2)$coefficients[2]
Diag[Q,3]=summary(fit2)$coefficients[1,2]
Diag[Q,4]=intercept
Diag[Q,5] = (Diag[Q,1]-Diag[Q,4])/Diag[Q,3]
Diag[Q,6] = qt(0.975,n-3)
Diag[Q,7] = as.numeric(abs(Diag[Q,5])>Diag[Q,6])
}
Diag
