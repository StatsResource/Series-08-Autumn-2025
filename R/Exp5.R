temp1 = apply(Blood[,1:3],1,mean)
temp2 = apply(Blood[,4:6],1,mean)
temp3 = apply(Blood[,7:9],1,mean)

Blood1=cbind(temp1,temp1,temp1,temp2,temp2,temp2,temp3,temp3,temp3)
dimnames(Blood1) = list(NULL,c("J1","J2","J3","R1","R2","R3","S1","S2","S3")) 
head(Blood1)
temp1 = apply(Blood[,1:3],1,mean)
temp2 = apply(Blood[,4:6],1,mean)
temp3 = apply(Blood[,7:9],1,mean)

Blood1=cbind(temp1,temp1,temp1,temp2,temp2,temp2,temp3,temp3,temp3)


Error1 = rnorm((85*9),0,sd=5)

Err1Mat= matrix(Error1, nrow=85, byrow=T)
Blood1=Blood1+Err1Mat

head(Blood1)


#####################################################################

dimnames(Blood1) = list(NULL, c("J1","J2","J3","R1","R2","R3","S1","S2","S3")) 


#####################################################################
#Preparing the Blood1 Data
library(nlme)
blood1 = groupedData( Y ~ meth | item ,
    data = data.frame( Y = c(Blood1), item = c(row(Blood1)),
        meth = rep(c("J","R","S"), rep(nrow(Blood)*3, 3)),
        repl = rep(rep(c(1:3), rep(nrow(Blood1), 3)), 3) ),
    labels = list(BP = "Systolic Blood Pressure", method = "Measurement Device"),
    order.groups = FALSE )
 
#pick out two of the three methods ( use J and S ) 
   
dat = subset(blood1, subset = meth != "R")


#####################################################################

fit1 = lme(Y ~ meth-1, data = dat,   #Symm , Symm#
    random = list(item=pdSymm(~ meth-1)), 
    weights=varIdent(form=~1|meth),
    correlation = corSymm(form=~1 | item/repl), 
    method="ML")

print(getD(fit1))
cat("\n")
print(getSigma(fit1))
