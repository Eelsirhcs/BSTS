## Chris Lee
## Overall conclusive used models at bottom end.
rm(list = ls())
graphics.off()

#install.packages('bsts')
library(bsts)

### data <- read.csv("gilbraltersimple.csv") #Video Data

setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8200/Mini-Project")
data <- read.csv("gilbralter_time_series_r_2.csv") #Time series of monthly sea temperatures sampled every 10 meters up to 100 meters.
ndata <- data[,-c(1,3)] #Remove Column 1 (timeIdx) and Remove Column 3 (StartDate)
names(ndata) <- c("SST", "10m", "20m", "30m", "40m", "50m", "60m", "70m", "80m", "90m") #Renaming the Data Columns

split <- round(nrow(ndata)*0.8) ## splitting the data to training and testing
train <- ndata[1:split,]
test <- ndata[(split+1):nrow(ndata),]

#Data Exploration
ndatats <- ts(ndata$SST, start = c(2004,1,1), end = c(2018,4,12), frequency = 11) 
plot(ndatats, main = 'Temperature at 0m, SST', xlab = 'Date [Year]', ylab = 'SST [deg C]')


#BSTS
ll_ss <- list() #Local Level
ll_ss <- AddLocalLevel(state.specification = ll_ss, y = ndatats)
ll_fit <- bsts(ndatats, state.specification = ll_ss, niter = 1000)
plot(ll_fit, main = "Expectation of Posterior", xlab = 'Date idx []', ylab = 'SST [deg C]')
plot(ll_fit, 'components', xlab = 'Date idx []', ylab = 'SST [deg C]') #plot of the posterior of the state components, in this case the local level
plot(ll_fit, 'residuals', main = "Residuals", xlab = 'Date idx []', ylab = 'SST [deg C]')
#Predict
ll_predict <- predict.bsts(ll_fit, horizon = 32)
plot(ll_predict, plot.original = 126, main = 'Local Linear Forcasts', xlab = 'Date idx [ ]', ylab = 'SST [deg C]') 
#The Horizon argument specifies how far ahead we want to forcast, in this case, we forcast 30 time steps ahead, which corresponds to 1 year. 
#Plot.Original argument plots the last 90 time step sof the original time series alongside the forecase. 
#The blue line indicates the median of the predictive distribution and the green line indicates the prediction interval. Default is 95% prediction interval. 
#The grey shade displays the posterior density distribution.
mean((ll_predict$mean-test$SST)^2)

llt_ss <- list() #Local Linear Trend
llt_ss <- AddLocalLinearTrend(state.specification = llt_ss, y = ndatats)
llt_fit <- bsts(ndatats, state.specification = llt_ss, niter = 1000)
llt_pred <- predict.bsts(llt_fit, horizon = 32)
plot(llt_pred, plot.original = 126, main = 'Local Linear Trend Forcasts', xlab = 'Date idx [ ]', ylab = 'SST [deg C]')

#Model Seasonality
lst_ss <- list()
lst_ss <- AddLocalLinearTrend(lst_ss, ndatats)
lts_ss <- AddSeasonal(lst_ss, ndatats, nseasons = 11) 
lts_fit <- bsts(ndatats, state.specification = lts_ss, niter = 1000)
plot(lts_fit, 'components', xlab = 'Date idx [ ]', ylab = 'SST [deg C]') #Trend Component, Seasonal
lts_pred <- predict.bsts(lts_fit, horizon = 32)
plot(lts_pred, plot.original = 126, main = 'Seasonal Local Linear Trend Forcasts', xlab = 'Date idx [ ]', ylab = 'SST [deg C]')

#Compare Several Models
CompareBstsModels(lwd = 4, model.list = list(level = ll_fit, trend = llt_fit, season = lts_fit))
#BSTS provides a plot that is useful for comparing several models. CompareBSTS function to compare the local level, the local linear trend, and the local linear trend with seasonality.


####
#Local Linear Seasonal and One linear component model.
ss <- list() 
ss <- AddLocalLinearTrend(ss, y = train$`0m`)
ss <- AddSeasonal(ss, train$`0m`, nseasons = 11)
rlls_model <- bsts(SST ~ ., state.specification = ss, data = train, niter = 1000, ping = 0, expected.model.size = 1)
#The ping parameter sets a print statement while the model is running.
#Expected model size sets our spike and slab prior to have one spike. 
#In other words, we are expecting one depth to heavily influence sea surface temperature, most likely the layer directly underneath.
plot(rlls_model, 'components', xlab = 'Date idx [ ]', ylab = 'SST [deg C')
#Reggression componenet plotted alongside out other ocmponents. 
plot(rlls_model, 'coefficients')
#We plot out coefficients to see which depths are significant. 
#The 10 to 20 meter depth has a positive coefficient and has a high inclusion probability.
#White bars represent positive betas and black bars represents negative betas.
rlls_model_pred <- predict.bsts(rlls_model, newdata = test, horizon = 28)
plot(rlls_model_pred, plot.original = 90, main = "Seasonal Local Linear Trend Forcasts with Regression", xlab = "Date idx [ ]", ylab = "SST [deg C]")

  

###################
m1ss <- AddLocalLinearTrend(list(),ndata$SST)
m1ss <- AddSeasonal(m1ss, ndata$SST, nseasons = 11)
Model1 <- bsts(SST ~ ., state.specification = m1ss, data = train, niter = 1000)
plot(Model1, "components" )


pred1 <- predict.bsts(Model1, horizon = 32)
plot(pred1, plot.original = 126)


###################################################################################
## Conclusive R with less thoughts ad comments as above                          ##
###################################################################################
rm(list = ls())
graphics.off()

#install.packages('bsts')
library(bsts)

setwd("C:/Users/leech/Documents/MSA/Spring2020/MSA 8200/Mini-Project")
data <- read.csv("gilbralter_time_series_r_2.csv") #Time series of monthly sea temperatures sampled every 10 meters up to 100 meters.
ndata <- data[,-c(1,3)] #Remove Column 1 (timeIdx) and Remove Column 3 (StartDate)
names(ndata) <- c("SST", "10m", "20m", "30m", "40m", "50m", "60m", "70m", "80m", "90m") #Renaming the Data Columns

split <- round(nrow(ndata)*0.8) ## splitting the data to training and testing
train <- ndata[1:split,]
test <- ndata[(split+1):nrow(ndata),]

ndatats <- ts(ndata$SST, start = c(2004,1,1), end = c(2018,4,12), frequency = 11)

ll_ss <- list() #Local Level
ll_ss <- AddLocalLevel(state.specification = ll_ss, y = ndatats)
ll_fit <- bsts(ndatats, state.specification = ll_ss, niter = 1000)
model1 <- ll_fit
plot(model1, "components")
pred1 <- predict.bsts(model1, horizon = 32)
plot(pred1, plot.original = 126, main = 'Local Level Forcast', xlab = 'Date Index [ ]', ylab = 'SST')
mean1 <-  mean((pred1$mean-test$SST)^2) #Really high MSPE 5.09

llt_ss <- list() #Local Linear Trend
llt_ss <- AddLocalLinearTrend(state.specification = llt_ss, y = ndatats)
llt_fit <- bsts(ndatats, state.specification = llt_ss, niter = 1000)
model2 <- llt_fit
pred2 <- predict.bsts(model2, horizon = 32)
plot(pred2, plot.original = 126,main = 'Local Linear Trend Forcast', xlab = 'Date Index [ ]', ylab = 'SST')
mean2 <- mean((pred2$mean-test$SST)^2) #9.955

lst_ss <- list() #Model Seasonality
lst_ss <- AddLocalLinearTrend(lst_ss, ndatats)
lts_ss <- AddSeasonal(lst_ss, ndatats, nseasons = 11) 
lts_fit <- bsts(ndatats, state.specification = lts_ss, niter = 1000)
model3 <- lts_fit
plot(model3, 'components') #Trend Component, Seasonal - 
#Component plot provides us with a glimpse of how our model will behave.
#Trend component seems to be roughly leveled.
#Seasonal component is seasonal

pred3 <- predict.bsts(model3, horizon = 32)
plot(pred3, plot.original = 126, main = 'Seasonal Local Linear Trend Forcasts', xlab = 'Date index [ ]', ylab = 'SST')
mean3 <- mean((pred3$mean-test$SST)^2) #2.25

CompareBstsModels(lwd = 4, model.list = list(level = model1, trend = model2, season = model3), colors = c("black", "red", "blue"), xlab = 'Date Index []')

lls_ss <- list() #Local Linear Seasonal and One linear component model.
lls_ss <- AddLocalLinearTrend(lls_ss, y = ndatats)
lls_ss <- AddSeasonal(lls_ss, ndatats, nseasons = 11)
rlls_model <- bsts(SST ~ ., state.specification = lls_ss, data = train, niter = 1000, ping = 0, expected.model.size = 1)
model4 <- rlls_model
plot(model4, "coef")
plot(model4, "comp")
pred4 <- predict.bsts(model4,newdata = test, horizon = 32)
plot(pred4, plot.original = 126, main = 'Seasonal Local Linear Trend Forcast with Regression', xlab = 'Date index [ ]', ylab = 'SST')
mean4 <- mean((pred4$mean-test$SST)^2) #0.024

lls2_ss <- list() #Testnig with expected model size 2
lls2_ss <- AddLocalLinearTrend(lls2_ss, y = ndatats)
lls2_ss <- AddSeasonal(lls2_ss, ndatats, nseasons = 11)
rlls2_model <- bsts(SST ~ ., state.specification = lls2_ss, data = train, niter = 1000, ping = 0, expected.model.size = 2)
model5 <- rlls2_model
plot(model5, "coef")
plot(model5, "comp")
pred5 <- predict.bsts(model5, newdata = test, horizon = 32)
plot(pred5, plot.original = 126,  main = 'Seasonal Local Linear Trend Forcast with Regression [2]', xlab = 'Date index [ ]', ylab = 'SST')
mean5 <- mean((pred5$mean-test$SST)^2) #0.021

lls3_ss <- list() #testing with expected model size 3
lls3_ss <- AddLocalLinearTrend(lls3_ss, y = ndatats)
lls3_ss <- AddSeasonal(lls3_ss, ndatats, nseasons = 11)
rlls3_model <- bsts(SST ~ ., state.specification = lls3_ss, data = train, niter = 1000, ping = 0, expected.model.size = 3)
model6 <- rlls3_model
plot(model6, "coef")
plot(model6, "comp")
pred6 <- predict.bsts(model6, newdata = test, horizon = 32)
plot(pred6, plot.original = 126,  main = 'Seasonal Local Linear Trend Forcast with Regression [2]', xlab = 'Date index [ ]', ylab = 'SST')
mean6 <- mean((pred6$mean-test$SST)^2) #0.021

CompareBstsModels(lwd = 4, model.list = list(SLLTWR1 = model4, SLLTRW2 = model5), colors = c("black", "red"), xlab = 'Date Index []')

bsts.prediction.errors(model5) ## One-step prediction error
#####
