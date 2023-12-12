# Parametric and Monte Carlo Value-at-Risk
In this project, I begin with calculating Value-at-Risk for an equally weighted portfolio of the N225 and CAC40 stock indices using a parametric approach. I discuss the advanatges and disadvantages of the approach using this method 

Next, I calculate Value-at-Risk for the same portfolio using a Monte Carlo approach. I use ARMA and GARCH to approximate the generating structure. After fitting, I find the resiudals, and use Sklar's Theorem to find a copula that can be applied to give the joint pdf. I then simulate the next residuals with this dependency structure, and use the structure of the corresponding models to "backtrack" to the actual log return. I then combine the separate log returns and find the overall log return and find the corresponding quantiles for the VAR. I again discuss advantages and disadvantages.

I conclude the report by comparing the two approaches and substantiating my ultimate choice.

You can download the R markdown file and knit in Rstudio for an interactive document, or simply read the pdf above.
