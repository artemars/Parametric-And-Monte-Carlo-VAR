groceryProducts <- read.csv('groceryProducts.csv', header = TRUE)
groceryStores <- read.csv('groceryStores.csv', header = TRUE)
groceryTransactions <- read.csv('groceryTransactions.csv', header = TRUE)

# Replace all the "-1s" in the 'units' column with NA

groceryTransactions$UNITS[groceryTransactions$UNITS == -1] <- NA

# Merge all the data we were supplied into one data frame 

GroceryData <- merge(groceryTransactions, groceryProducts, all.x = TRUE)
GroceryData <- merge(GroceryData, groceryStores, all.x = TRUE)

# Order the data appropriately

GroceryData <- GroceryData[order(GroceryData$YEAR, GroceryData$MONTH),]


# Explanatory analysis


# min, max and mean of response variable

min(GroceryData$UNITS, na.rm = TRUE)
max(GroceryData$UNITS, na.rm = TRUE)
mean(GroceryData$UNITS, na.rm = TRUE)

# 'Outliers' by boxplot criterion

UQ <- quantile(GroceryData$UNITS, 0.75, na.rm = TRUE)
UnitIQR <- IQR(GroceryData$UNITS, na.rm = TRUE)
Outlier <- UQ + 1.5*UnitIQR

length(GroceryData$UNITS[GroceryData$UNITS > Outlier])

# Form plots of units sold against 'price' and 'base_price' below

par(mfrow=c(1,2), mar = c(6,4,4,2))

plot(GroceryData$PRICE, GroceryData$UNITS, ylab = 'Number of units sold in a month', xlab = 'average price of product',
     pch = 16, cex.lab = 1.4)

plot(GroceryData$BASE_PRICE, GroceryData$UNITS, ylab = 'Number of units sold in a month',
     xlab = 'average price of product without promotions', pch = 16, cex.lab = 1.4)

# Form a main title for both plots

mtext("Plots of units sold against price covariates", side = 3,line = -2,outer = TRUE, cex = 2)

# Save the plots as a png file

dev.copy(png,"prices.png",width=13*72,height=6*72)
dev.off()

# Form plots of units sold against 'feature,' 'display,' and 'tpr_only' below 

par(mfrow=c(1,3), mar = c(6,4,4,2), mgp=c(2.7, 1, 0))

plot(GroceryData$FEATURE, GroceryData$UNITS, ylab = 'Number of units sold in a month',
     xlab = 'Proportion of weeks the product was in a marketing circle', pch = 16, cex.lab = 2.2, cex.axis = 2)

plot(GroceryData$DISPLAY, GroceryData$UNITS, ylab = 'Number of units sold in a month',
                           xlab = 'Proportion of weeks the product was on display', pch = 16, cex.lab = 2.2, cex.axis = 2)

plot(GroceryData$TPR_ONLY, GroceryData$UNITS, ylab = 'Number of units sold in a month',
     xlab = 'Proportion of weeks of temporary price reduction', pch = 16, cex.lab = 2.2, cex.axis = 2)

# Form a main title for all plots

mtext("Plots of units sold against marketing strategies", side = 3,line = -2,outer = TRUE, cex = 2)

# Save the plots as a png file

dev.copy(png,"marketing.png",width=18*72,height=7*72)
dev.off()

# Clustering

# Cluster 'store_num' based on their sizes  

NumVars <- (sapply(GroceryData, is.numeric) & # Columns containing numeric
              + !names(GroceryData) %in% c("UNITS","MONTH","YEAR","UPC","STORE_NUM","ID","AREA_CODE", "NWEEKS")) 
StoreMeans <- aggregate(GroceryData[,NumVars], by=list(GroceryData$STORE_NUM), FUN=mean)
rownames(StoreMeans) <- StoreMeans[,1]
StoreMeans <- scale(StoreMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(StoreMeans) # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering
NewStores <- cutree(ClusTree, k=4)
GroceryData <- merge(data.frame(STORE_NUM=names(NewStores), NewStore=NewStores), GroceryData, by = "STORE_NUM")

# Cluster 'area_codes' based on their marketing efforts (ie 'feature,' 'display,' and 'tpr_only')

NumVars <- (sapply(GroceryData, is.numeric) & # Columns containing numeric
            + !names(GroceryData) %in% c("UNITS","MONTH","YEAR","UPC","STORE_NUM","ID","AREA_CODE", "NNWEEKS", "PRICE",
                                         "AVG_WEEKLY_BASKETS", "BASE_PRICE", "NewStore"))
AreaMeans <- aggregate(GroceryData[,NumVars], by=list(GroceryData$AREA_CODE), FUN=mean) 
rownames(AreaMeans) <- AreaMeans[,1]
AreaMeans <- scale(AreaMeans[,-1]) # Standardise to mean 0 & SD 1
Distances <- dist(AreaMeans) # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering
NewAreas <- cutree(ClusTree, k=3)
GroceryData <- merge(data.frame(AREA_CODE=names(NewAreas), NewArea=NewAreas), GroceryData, by = "AREA_CODE")

# Cluster 'city' based on the single variable 'nweeks'  

NumVars <- (sapply(GroceryData, is.numeric) & # Columns containing numeric
              + !names(GroceryData) %in% c("UNITS","MONTH","YEAR","UPC","STORE_NUM","ID","AREA_CODE", "NNWEEKS", "PRICE",
                                           "AVG_WEEKLY_BASKETS", "BASE_PRICE", "NewStore", "TPR_ONLY", "FEATURE", "DISPLAY",
                                           "NewArea"))
CityMeans <- aggregate(GroceryData[,NumVars], by=list(GroceryData$CITY), FUN=mean) 
CityNames <- CityMeans[,1]
CityMeans <- scale(CityMeans[,-1]) # Standardise to mean 0 & SD 1
CityMeans = as.numeric(CityMeans)
names(CityMeans) = CityNames
Distances <- dist(CityMeans) # Pairwise distances
ClusTree <- hclust(Distances, method="complete") # Do the clustering
NewCities <- cutree(ClusTree, k=4)
GroceryData <- merge(data.frame(CITY=names(NewCities), NewCity=NewCities), GroceryData, by = "CITY")

# Function for adding means of units sold for each category into the corresponding boxplots, including a legend 

UnitMeans <- function(covariate, location, PointSize = 1, LegendSize = 1) {
  #
  # covariate - the categorical covariate from which we'll calculate the mean units sold for each of its levels
  # location - the location of the legend on the plot
  # PointSize - the size of the points
  # LegendSize - the size of the legend
  #
  
  # calculate mean units sold for all levels of categorical covariate
  UnitMean <- tapply(GroceryData$UNITS, INDEX=covariate, FUN=mean, na.rm = TRUE)
  # assign 'UnitPoints' the number of different levels in categorical covariate
  UnitPoints <- length(levels(as.factor(covariate)))
  # Use 'UnitPoints' to form an adequate number of points in the plot
  points(c(1:UnitPoints), UnitMean, pch = 'x', col = 'blue', cex = PointSize)
  # Form the legend 
  legend(location, "mean", pch = 'x', col = 'blue', cex = LegendSize)
}

# Form boxplots of all clustered covariates above related to the units sold 

par(mfrow=c(1,3))

boxplot(GroceryData$UNITS ~ GroceryData$NewStore, ylab = 'Number of units sold in a month',
        xlab = 'Subgroups of clustered store numbers', cex.lab = 2, cex.axis = 2, cex = 1.5)
UnitMeans(GroceryData$NewStore, 'topleft', 2, 1.5)

boxplot(GroceryData$UNITS ~ GroceryData$NewArea, ylab = 'Number of units sold in a month',
        xlab = 'Subgroups of clustered area codes', cex.lab = 2, cex.axis = 2, cex = 1.5)
UnitMeans(GroceryData$NewArea, 'topright', 2, 1.5)

boxplot(GroceryData$UNITS ~ GroceryData$NewCity, ylab = 'Number of units sold in a month',
        xlab = 'Subgroups of clustered cities', cex.lab = 2, cex.axis = 2, cex = 1.5)
UnitMeans(GroceryData$NewCity, 'topright', 2, 1.5)


# Form a main title for all plots

mtext("Plots of units sold against clustered covariates", side = 3,line = -2,outer = TRUE, cex = 2)

# Save the plots as a png file

dev.copy(png,"cluster.png",width=15*72,height=6*72)
dev.off()

# Form boxplots of 'state' related to the units sold 

par(mfrow=c(1,2), mar = c(6,4,4,2))

boxplot(GroceryData$UNITS ~ GroceryData$STATE, ylab = 'Number of units sold in a month',
        xlab = 'State', cex.lab = 1.5)
UnitMeans(GroceryData$STATE, 'topright', 1.5)

# Calculate how many observations come from each state (not included in report)

tapply(GroceryData$UNITS, INDEX=GroceryData$STATE, FUN=length)

# Form plot of units sold against 'avg_weekly_baskets'

plot(GroceryData$AVG_WEEKLY_BASKETS, GroceryData$UNITS, ylab = 'Number of units sold in a month',
     xlab = 'average number of weekly baskets sold', pch = 16, cex.lab = 1.5)

# Form a main title for both plots

mtext("Boxplot of effects of state on units sold and plot of units sold against 'avg_weekly_baskets'", side = 3,line = -2,outer = TRUE,
      cex = 1.5)

# Save the plots as a png file

dev.copy(png,"storechar.png",width=13*72,height=6*72)
dev.off()

# Form boxplots of 'UPC' related to the units sold and scatter plot of units sold against 'nweeks'

par(mfrow=c(1,2))

boxplot(GroceryData$UNITS ~ GroceryData$UPC, ylab = 'Number of units sold in a month',
        xlab = 'UPC', xaxt = 'n', cex.lab = 1.5)
UnitMeans(GroceryData$UPC, 'topright')

plot(GroceryData$NWEEKS, GroceryData$UNITS, ylab = 'Number of units sold in a month',
        xlab = 'Number of weeks product was available', cex.lab = 1.5)

# Form a main title for both plots

mtext("Boxplot of effects of upc on units sold and plot of units sold against 'nweeks'", side = 3,line = -2,outer = TRUE, cex = 2)

# Save the plots as a png file

dev.copy(png,"upcandnweeks.png",width=13*72,height=6*72)
dev.off()

# Make a new column in the data frame 'GroceryData' related to the season the product was sold in

GroceryData$Season[GroceryData$MONTH == 1 | GroceryData$MONTH == 12 | GroceryData$MONTH == 2 ] <- "Winter"
GroceryData$Season[GroceryData$MONTH == 3 | GroceryData$MONTH == 4 | GroceryData$MONTH == 5 ] <- "Spring"
GroceryData$Season[GroceryData$MONTH == 6 | GroceryData$MONTH == 7 | GroceryData$MONTH == 8 ] <- "Summer"
GroceryData$Season[GroceryData$MONTH == 9 | GroceryData$MONTH == 10 | GroceryData$MONTH == 11 ] <- "Autumn"

# Form boxplots of 'season' (formed above) and 'year' related to units sold (not included in report)

par(mfrow=c(1,2))

boxplot(GroceryData$UNITS ~ GroceryData$Season, ylab = 'Number of units sold in a month',
        xlab = 'Season')
UnitMeans(GroceryData$Season, 'topleft')

boxplot(GroceryData$UNITS ~ GroceryData$YEAR, ylab = 'Number of units sold in a month',
        xlab = 'Year')
UnitMeans(GroceryData$YEAR, 'topleft')

# Change the below columns into factors as, although representing numbers, they're in fact categorical covariates

GroceryData$NewArea <- as.factor(GroceryData$NewArea)
GroceryData$NewStore <- as.factor(GroceryData$NewStore)
GroceryData$NewCity <- as.factor(GroceryData$NewCity)
GroceryData$YEAR <- as.factor(GroceryData$YEAR)
GroceryData$UPC <- as.factor(GroceryData$UPC)

# Form two data frames, one with all observations with recorded units sold and another with missing observations for units sold

GroceryDataNoNAs <- GroceryData[!(is.na(GroceryData$UNITS)),]
GroceryDataNAs <- GroceryData[(is.na(GroceryData$UNITS)),]


# Model building 


# First model, from data frame without NAs

model1 <- glm(UNITS ~ PRICE + BASE_PRICE + UPC + AVG_WEEKLY_BASKETS + NWEEKS + NewArea
              + NewCity + NewStore
             , family=poisson(link="log"),data=GroceryDataNoNAs)

summary(model1)

# Diagnostic plots for 'model1'

par(mfrow=c(1,3), mar = c(6,5,4,2), mgp=c(2.7, 1, 0))
plot(model1,which=c(1,3,4), cex.lab = 2, cex.axis = 2)

# Form a main title for all plots

mtext("Diagnostic plots for 'model1'", side = 3,line = -2,outer = TRUE, cex = 2)

# Save the plots as a png file

dev.copy(png, "diagnosticsplot1.png",width=18*72,height=6*72)
dev.off()

# Second model, adding more covariates

model2 <- update(model1, . ~ . + TPR_ONLY + DISPLAY + FEATURE + YEAR + Season + STORE_TYPE)

summary(model2)

# Perform an anova chi-squared test to compare the nested model with the new one

anova(model1,model2,test="Chi")

# Third model, modeling rate of units sold per weekly transaction and interaction between 'price' and 'UPC'

model3 <- update(model2, . ~ . + PRICE:UPC + log(AVG_WEEKLY_BASKETS * NWEEKS) - NWEEKS - AVG_WEEKLY_BASKETS)

summary(model3)

# Model 4, removing 'base_price'

model4 <- update(model3, . ~ . - BASE_PRICE)

summary(model4)

# Perform an anova chi-squared test to compare the nested model with the previous one

anova(model4,model3,test="Chi")

# Diagnostic plots for 'model3'

par(mfrow=c(1,3), mar = c(6,5,4,2), mgp=c(2.7, 1, 0))
plot(model3,which=c(1,3,4), cex.lab = 2, cex.axis = 2)

# Form a main title for all plots

mtext("Diagnostic plots for 'model3'", side = 3,line = -2,outer = TRUE, cex = 2)

# Save the plots as a png file

dev.copy(png, "diagnosticsplot2.png",width=18*72,height=6*72)
dev.off()

# calculate the variance of the Pearson residuals

PearsonVar <- sum(resid(model3,type="pearson")^2)/model3$df.residual

print(PearsonVar)

# Model 5, same model as 3, but without the three most influential observations according to the Cook's distance plot

model5 <- update(model3, data=GroceryDataNoNAs[-c(4470,5139,7288),])

summary(model5)

# Model 6, forming glm based on 'quasipoisson' distribution (preferred model)

model6 <- update(model3, family=quasipoisson(link="log"))

summary(model6)

# Perform f test to see if all covariates are important 

anova(model6,test="F")


# Predictions


# Calculate dispersion parameter

dispersion <- sum(resid(model6,type="pearson")^2)/model6$df.residual

# Make predictions of number of sales of each product at each store from 'GroceryDataNAs' and include the standard deviation of miu[i] 

pred <- predict.glm(model6, GroceryDataNAs, se.fit = TRUE, type = 'response')

# Calculate standard deviations of prediction errors

standard <- sqrt(pred$se.fit^2+pred$fit*dispersion)


# Form a 'dat' file of 'ID,' 'prediction' and 'standard deviation of prediction errors'

FinalTable <- data.frame(GroceryDataNAs$ID, pred$fit, standard)
write.table(FinalTable, file = 'ICA2Group_AM_pred.dat', sep = " ", col.names = FALSE, row.names = FALSE)
