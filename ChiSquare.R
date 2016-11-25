


colsum1 <- 100;colsum2<-100;colsum3<-100;
rowsum1 <- 150; rowsum2 <- 150; 
grandtotal <- sum(rowsum1,rowsum2);

E1 <- (rowsum1*colsum1)/ grandtotal;
E2 <- (rowsum1*colsum2)/ grandtotal;
E3 <- (rowsum1*colsum3)/ grandtotal;
E4 <- (rowsum2*colsum1)/ grandtotal;
E5 <- (rowsum2*colsum2)/ grandtotal;
E6 <- (rowsum2*colsum3)/ grandtotal;

myExps <- c(E1,E2,E3,E4,E5,E6)

Ob1 <- 5*sample(2:15,1);
Ob2 <- 5*sample(1:20,1);
Ob3 <- rowsum1 -(Ob1+Ob2);
Ob4 <- colsum1 - Ob1
Ob5 <- colsum2 - Ob2
Ob6 <- rowsum2 -(Ob4+Ob5);

myObs <- c(Ob1,Ob2,Ob3,Ob4,Ob5,Ob6)
CheckPoint=sum( myObs > 0)

while(CheckPoint != 6){
Ob1 <- 5*sample(2:15,1);
Ob2 <- 5*sample(1:20,1);
Ob3 <- rowsum1 -(Ob1+Ob2);
Ob4 <- colsum1 - Ob1
Ob5 <- colsum2 - Ob2
Ob6 <- rowsum2 -(Ob4+Ob5);
myObs <- c(Ob1,Ob2,Ob3,Ob4,Ob5,Ob6)
CheckPoint=sum( myObs > 0)
}


TS = sum((myObs-myExps)^2/myExps)

cat("\n",
"XX Suppose we have a two-way contigency table of observed values","\n",
"- Variable X has two levels: A and J","\n",
"- Variable Y has three levels: K L and F","\n","\n",
"  K  L  F","\n",
"-----------","\n",
"A",c(Ob1,Ob2,Ob3),"\n",
"J",c(Ob4,Ob5,Ob6),"\n",
"-----------","\n",
"\n","XX Suppose that the expected value in each cell is 50","\n",
"\n",
"  K  L  F","\n",
"-----------","\n",
"A",c(E1,E2,E3),"\n",
"J",c(E4,E5,E6),"\n",
"-----------","\n","\n",
"XX Compute the Chi-Square Test Statistic","\n",
"XX Write Your Answer Here: {" , 0.99*TS ,"|", 1.01*TS , "}", "\n",
"XX Write your answer to three decimal places.","\n","\n"
)