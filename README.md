# Parametric and Monte Carlo Value-at-Risk
In this project, I begin with calculating Value-at-Risk for an equally weighted portfolio of the N225 and the CAC 40 stock indices using a parametric approach. Next, I discuss the advantages and disadvantages associated with this method. 

In the second section, I calculate Value-at-Risk for the same portfolio using a Monte Carlo approach. I use ARMA and GARCH to approximate the generating structure. After fitting, I find the residuals and then find a copula that can be applied to the residual pairs to give the joint pdf (proven to exist from Sklar's Theorem). I then simulate 10,000 residuals for the next time-step with this copula dependency structure and use the residuals and the corresponding model structures to get back the new simulated log returns. I then combine the 2 sets of 10,000 log returns with the corresponding simulation and find the overall log return for each simulation. Finally, I determine the corresponding quantiles for the VAR and once again discuss the advantages and disadvantages.

I conclude the report by comparing the two approaches and supporting my ultimate choice.

You can download the R Markdown file and knit it in RStudio for an interactive document, or simply read the PDF above.
