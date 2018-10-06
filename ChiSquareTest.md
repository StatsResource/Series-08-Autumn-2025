
<h3>Chi-squared Test</h3>



A $\chi^2$ test is carried out on tabular data containing counts, e.g. the
number of animals that died, the number of days of rain, the
number of stocks that grew in value, etc.
<p>
Usually have two qualitative variables, each with a number of
levels, and want to determine if there is a relationship between the
two variables, e.g. hair colour and eye colour, social status and
crime rates, house price and house size, gender and left/right
handedness.
</p><p>
The data are presented in a contingency table:
right-handed left-handed TOTAL
</p><p>
\begin{tabular}{|c|c|c|c|}
	\hline
	% after \\: \hline or \cline{col1-col2} \cline{col3-col4} ...
	&amp; right-handed &amp;left-handed &amp; TOTAL\\\hline
	Male &amp; 43 &amp; 9 &amp; 52 \\
	Female &amp; 44 &amp; 4 &amp; 48 \\
	TOTAL &amp; 87 &amp; 13 &amp; 100 \\
	\hline
\end{tabular}
</p><p>

The hypothesis to be tested is:<br>
* $H0 :$There is no relationship between gender and left/right-handedness<br>
* $H1 :$There is a relationship between gender and left/right-handedness<br>
</p><p>
The values that we collect from our sample are called the observed
(O) frequencies (counts). Now need to calculate the expected (E)
frequencies, i.e. the values we would expect to see in the table, if
H0 was true.
</p>


```R
\section{Section 3. Chi-squared Test of Independence}
Two random variables x and y are called independent if the probability distribution of one variable is not affected by the presence of another.

Assume $f_{ij}$ is the observed frequency count of events belonging to both i-th category of x and j-th category of y. 
Also assume $e_{ij}$ to be the corresponding expected count if x and y are independent. 

The null hypothesis of the independence assumption is to be rejected if the p-value of the following Chi-squared test statistics is less than a given significance level $\alpha$.

<h4>Example</h4>
In the embedded data set “survey”, the Smoke column records the students smoking habit, while the Exer column records their exercise level. 
<ul>
<li> The allowed values in Smoke are "Heavy", "Regul" (regularly), "Occas" (occasionally) and "Never". 
<li> As for Exer, they are "Freq" (frequently), "Some" and "None".
</ul>

We can tally the students smoking habit against the exercise level with the table function in \texttt{R}. 

The result is called the contingency table of the two variables.
<pre>
<code>
> library(MASS)       # load the MASS package 
> tbl = table(survey$Smoke, survey$Exer) 
> tbl                 # the contingency table 
</code>
</pre>
<pre>
<code>
        Freq None Some 
  Heavy    7    1    3 
  Never   87   18   84 
  Occas   12    3    4 
  Regul    9    1    7

help(normal)
</code>
</pre>
<p>
```


\subsection{Problem}
Test the hypothesis whether the students smoking habit is independent of their exercise level at 0.05 significance level.
Solution
We apply the chisq.test function to the contingency table tbl, and found the p-value to be 0.4828.
<pre><code>







</code></pre>


```R
> chisq.test(tbl) 
 
        Pearson’s Chi-squared test 
 
data:  table(survey$Smoke, survey$Exer) 
X-squared = 5.4885, df = 6, p-value = 0.4828 
 
Warning message: 
In chisq.test(table(survey$Smoke, survey$Exer)) : 
  Chi-squared approximation may be incorrect

```


```R
### Answer

As the p-value 0.4828 is greater than the 0.05 significance level, we do not reject the null hypothesis that the smoking habit is independent of the exercise level of the students.

```


```R
### Enhanced Solution
The warning message found in the solution above is due to the small cell values in the contingency table. To avoid such warning, we combine the second and third columns of tbl, and save it in a new table named ctbl. Then we apply the chisq.test function against ctblinstead.

```


```R
ctbl = cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) 
ctbl 
      [,1] [,2] 
Heavy    7    4 
Never   87  102 
Occas   12    7 
Regul    9    8 
</code>
</pre>



```


```R
chisq.test(ctbl) 

```

The hypothesis to be tested is

* $H0 :$There is no relationship between gender and left/right-handedness
* $H1 :$There is a relationship between gender and left/right-handedness

The values that we collect from our sample are called the observed (O) frequencies (counts). Now need to calculate the expected (E)
frequencies, i.e. the values we would expect to see in the table, if H0 was true.


### Chi-squared Test of Independence
Two random variables x and y are called independent if the probability distribution of one variable is not affected by the presence of another.

Assume $f_{ij}$ is the observed frequency count of events belonging to both i-th category of x and j-th category of y. Also assume eij to be the corresponding expected count if x and yare independent. 

The null hypothesis of the independence assumption is to be rejected if the p-value of the following Chi-squared test statistics is less than a given significance level α.

 
### Example
In the embedded data set “survey”, the Smoke column records the students smoking habit, while the Exer column records their exercise level. The allowed values in Smoke are "Heavy", "Regul" (regularly), "Occas" (occasionally) and "Never". As for Exer, they are "Freq" (frequently), "Some" and "None".

We can tally the students smoking habit against the exercise level with the table function in R. The result is called the contingency table of the two variables.
 


```R
library(MASS)       # load the MASS package 
tbl = table(survey$Smoke, survey$Exer) 
tbl                 # the contingency table 

```


           
            Freq None Some
      Heavy    7    1    3
      Never   87   18   84
      Occas   12    3    4
      Regul    9    1    7



```R
Problem
Test the hypothesis whether the students smoking habit is independent of their exercise level at 0.05 significance level.
Solution
We apply the chisq.test function to the contingency table tbl, and found the p-value to be 0.4828.

```


```R
> chisq.test(tbl) 
 
        Pearson’s Chi-squared test 
 
data:  table(survey$Smoke, survey$Exer) 
X-squared = 5.4885, df = 6, p-value = 0.4828 
 
Warning message: 
In chisq.test(table(survey$Smoke, survey$Exer)) : 
  Chi-squared approximation may be incorrect
Answer
As the p-value 0.4828 is greater than the .05 significance level, we do not reject the null hypothesis that the smoking habit is independent of the exercise level of the students.
Enhanced Solution
The warning message found in the solution above is due to the small cell values in the contingency table. To avoid such warning, we combine the second and third columns of tbl, and save it in a new table named ctbl. Then we apply the chisq.test function against ctblinstead.
 


```


```R
ctbl <- cbind(tbl[,"Freq"], tbl[,"None"] + tbl[,"Some"]) 
ctbl 
```


```R
chisq.test(ctbl) 
```
