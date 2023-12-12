# Parametric and Monte Carlo Value-at-Risk
In this project, I begin with calculating Value-at-Risk for an equally weighted portfolio of the N225 and the CAC 40 stock indices using a parametric approach. I discuss the advantages and disadvantages of the approach using this method. 

Next, I calculate Value-at-Risk for the same portfolio using a Monte Carlo approach. I use ARMA and GARCH to approximate the generating structure. After fitting, I find the resiudals, and use Sklar's Theorem to find a copula that can be applied to give the joint pdf. I then simulate 10,000 residuals for the next time-step with this dependency structure, and use the residuals and underlying structure of the corresponding models to get back the new simulated log returns. I then combine the 2 sets of 10,000 log returns with the corresponding simulation and find the overall log return for each simulation. Finally, I find the corresponding quantiles for the VAR. I again discuss advantages and disadvantages.

I conclude the report by comparing the two approaches and substantiating my ultimate choice.

You can download the R markdown file and knit in Rstudio for an interactive document, or simply read the pdf above.
