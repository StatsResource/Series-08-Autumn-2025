


#######################################################################################################
# JSR Blood Data

blood = groupedData( y ~ meth | item ,
    data = data.frame( y = c(Blood), item = c(row(Blood)),
        meth = rep(c("J","R","S"), rep(nrow(Blood)*3, 3)),
        repl = rep(rep(c(1:3), rep(nrow(Blood), 3)), 3) ),
    labels = list(y = "Systolic Blood Pressure", meth= "Measurement Device"),
    order.groups = FALSE )


# make a data frame containing J and S groups only:

datJS = subset(blood, subset = meth != "R")
datRS = subset(blood, subset = meth != "J")
datJR = subset(blood, subset = meth != "S")

datJS$item <- factor(datJS$item)
datJS$repl <- factor(datJS$repl)

####################################################################################################

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=datJS
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datJS
)

##############################################################################################


datRS$item <- factor(datRS$item)
datRS$repl <- factor(datRS$repl)

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=datRS
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datRS
)

###########################################################################################




datJR$item <- factor(datJR$item)
datJR$repl <- factor(datJR$repl)

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=datJR
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datJR
)






data( fat )
fat <- data.frame( item=factor(fat$Id),
meth=fat$Obs,
repl=factor(fat$Rep),
y=fat$Sub )


lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=fat
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=fat
)





MCS1 = lme(y ~ meth-1, data = fat,
random = list(item=pdSymm(~ meth-1)),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")



MCS2 = lme(BP ~ method-1, data = datRS,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")


MCS3 = lme(BP ~ method-1, data = datJR,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")


MCS3 = lme(BP ~ method-1, data = ox,
random = list(subject=pdSymm(~ method-1)),
weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")








lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datJS
)





MCS1 = lme(y ~ meth-1, data = fat,random = list(item=pdSymm(~ method-1)),weights=varIdent(form=~1|method), correlation = corSymm(form=~1 | subject/obs), method="ML")





fat <- data.frame( item=factor(fat$Id),
meth=fat$Obs,
repl=factor(fat$Rep),
y=fat$Sub )





lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=cardiac
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=cardiac
)




MCS1 = lme(y ~ meth-1, data = hamlett,
random = list(item=pdSymm(~ meth-1)),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=hamlett
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=hamlett
)




MCS2 = lme(y ~ meth-1, data = hamlett,
random=list( item = pdSymm( ~ meth-1 ),
 repl = ~1 ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=hamlett
)






getD = function(x) {
    v = as.numeric(VarCorr(x)[1:2,1])
    r = as.numeric(VarCorr(x)[2,3])
    Dhat = matrix(0,2,2)
    diag(Dhat) = v
    Dhat[1,2] = Dhat[2,1] = r * sqrt(prod(v))
    arr=rownames(summary(x)$contrasts$method)
    colnames(Dhat)=arr
    rownames(Dhat)=arr
    Dhat
}
#########################################################################


getSigma = function(x) {
    res = as.numeric(VarCorr(x)[6])
    V = max(is.null(intervals(x)$varStruct[2]),intervals(x)$varStruct[2])
    C = intervals(x)$corStruct[2]
    Sighat = matrix(1,2,2)
    Sighat[2,2] = as.numeric(VarCorr(x)[3,1])
    Sighat[1,1] = as.numeric(VarCorr(x)[3,1])*(V^2)
    Sighat[1,2] = Sighat[2,1] = as.numeric(VarCorr(x)[3,1])*V*C
    arr=rownames(summary(x)$contrasts$method)
    colnames(Sighat)=arr
    rownames(Sighat)=arr
    Sighat
}

#########################################################################
getOmega = function(x) {
    Omega = matrix(1,2,2)
    Omega[1,1] = getD(x)[1,1] + getSigma(x)[1,1] 
    Omega[2,2] = getD(x)[2,2] + getSigma(x)[2,2]
    Omega[1,2] = Omega[2,1] = getD(x)[1,2] + getSigma(x)[1,2]
    Omega
}


Roy.JS.1 = lme(y ~ meth-1, data = datJS,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


Roy.RS.1 = lme(y ~ meth-1, data = datRS,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.JR.1 = lme(y ~ meth-1, data = datJR,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.ox.1 = lme(y ~ meth-1, data = ox,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.fat.1 = lme(y ~ meth-1, data = fat,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

Roy.hamlett.1 = lme(y ~ meth-1, data = hamlett,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


getSigma(Roy.JS.1 )
getSigma(Roy.RS.1 )
getSigma(Roy.JR.1 )
getSigma(Roy.fat.1 )
getSigma(Roy.ox.1 )
getSigma(Roy.hamlett.1 )


getOmega(Roy.JS.1 )
getOmega(Roy.RS.1 )
getOmega(Roy.JR.1 )
getOmega(Roy.fat.1 )
getOmega(Roy.ox.1 )
getOmega(Roy.hamlett.1 )



getD(Roy.fat.1 )
getD(Roy.ox.1 )



Roy.card.1 = lme(y ~ meth-1, data = cardiac,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")

getD(Roy.card.1)
getSigma(Roy.card.1)


############################################################################################################################










Roy.PEFR.1 = lme(y ~ meth-1, data = PEFR,
random=list( item = pdSymm( ~ meth-1 ) ),
weights=varIdent(form=~1|meth), correlation = corSymm(form=~1 | item/repl), method="ML")


lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=PEFR
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=PEFR
)
















lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=datRS
)


data( ox )
ox$item <- factor(ox$item)
ox$repl <- factor(ox$repl)


lme( y ~ meth + item,
 random = list( item = pdIdent( ~ meth-1 ) ),
 weights = varIdent( form = ~1 | meth ),
 data=ox
)
lme( y ~ meth + item,
 random=list( item = pdIdent( ~ meth-1 ),
 repl = ~1 ),
 weights = varIdent( form = ~1 | meth ),
 data=ox
)
