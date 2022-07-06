# Clear memory
rm(list = ls())

# Set the working directory
setwd("~/Documents/IÉSEG SCHOOL OF MANAGEMENT/R/ECONOMETRICS R /GProject-ECONOMETRICS")

# Import and load packages here

# install.packages("car")
library(car)
# install.packages("readxl")
library(readxl)
# install.packages("lmtest") # Install package
library(lmtest) # Load library lmtest
# install.packages("sandwich")
library(sandwich) # Load library sandwich
library(forecast)
library(quantmod) # Library to Optimize Returns and Key indicators computation

# Import Excel file and read different Sheets + Define the DataFrame 
rv_data <- read_excel("Group Work Data - JL.xlsx")
rv_dataPAGE2 <- read_excel("Group Work Data - JL.xlsx", sheet = 2)
rv_dataPAGE3 <- read_excel("Group Work Data - JL.xlsx", sheet = 3)
rv_dataPAGE4 <- read_excel("Group Work Data - JL.xlsx", sheet = 4)

###############################################
# Case 1: The Single Index Model (Weight: 40%)
###############################################
#1.	Work out the OLS estimates of the parameters α and β using data for the full period from January 2nd, 2018 until November 25th, 2020. 
#Define the variables
EXRSP500 <- rv_data$EXRSP500
EXRET_P1 <- rv_data$EXRET_P1

#Single Index Model
model1 <- lm(EXRSP500 ~ EXRET_P1)
summary(model1)

# Plot residuals
par(mfrow=c(1,1)) # 1x2 array of plots
plot.ts(model1$residuals) # Plot residuals

#--------#
#2.	Solve the following hypothesis tests (with confidence level at the 95%). 
confint(model1, level=0.95)

# Test A
linearHypothesis(model1, c('EXRET_P1=0'))
### Since p-value>0.05 the test fails to reject the null H0: alpha = 0

# Test B
linearHypothesis(model1, c("(Intercept)=1"))
### Since p-value>0.05 the test fails to reject the null H0: beta = 1

# Test C
linearHypothesis(model1, c("EXRET_P1=0", "(Intercept)=1"))
### Since p-value>0.05 the test fails to reject the null H0: alpha = 0 and Beta = 1

#--------#
#3.For the regression above, compute the decomposition TSS = RSS + ESS. Comment on your results and draw a parallel with the following risk decomposition Total Risk = Systematic Risk + Idiosyncratic Risk.
# TSS Computation, 
TSS <- (length(rv_data)-1)*var(rv_data)
print(TSS)

#RSS Computation 
e1hat <- model1$residuals
e1hat_sq <- e1hat*e1hat
RSS <- sum(e1hat_sq)
print(RSS)

#ESS Computation
ESS <- TSS - RSS 
print(ESS)

#--------#
#4.Produce OLS estimates for such linear regression over the full period 2 January 2018 until 25 Nov 2020.
DTB3 <- rv_data$DTB3
model2 <- lm(EXRSP500 ~ EXRET_P1 + DTB3)
summary(model2)
#--------#
#5.	Solve the following hypothesis tests, and report your results in the tables: 
# Test A
linearHypothesis(model2, c('EXRET_P1=0'))
### Since p-value>0.05 the test fails to reject the null H0:

# Test B
linearHypothesis(model2, c('(Intercept)=1'))
### Since p-value>0.05 the test fails to reject the null H0:

# Test c
linearHypothesis(model2, c('DTB3=0'))
### Since p-value>0.05 the test fails to reject the null H0:

# Test E
linearHypothesis(model2, c("EXRET_P1=0", "(Intercept)=0", "DTB3=0"))
### Since p-value>0.05 the test fails to reject the null H0:

# Test F
linearHypothesis(model2, c("EXRET_P1=0", "DTB3=0"))
### Since p-value>0.05 the test fails to reject the null H0:

#--------#
#6.	Comment on the meaning and the implications of these tests conducted in question 5. Explain what happened to this stock during the Covid pandemic (about 200 words).

###############################################
# Case 2: GDP Forecasting (Weight: 60%)
###############################################
#1.	Start by removing the last 4 observations from the dataset in order to have GDP data until and including 2019Q4. Then, draw a time series graph of GDP. Interpret the series from a visual perspective. Explain whether you think it is stationary or not.
 
rv_dataPAGE3[-c(101, 102, 103, 104),,drop=F]
#--------#
#2.	Conduct an Augmented Dickey-Fuller test and interpret the results.
library(tseries)
adf.test(rv_dataPAGE3$GPD)
#--------#
#3.	If the series is not stationary add the necessary boxes with your answers
#a
#b
#c
#d
#--------#
#4.	Show a graph of the ACF (autocorrelation function) and PACF (partial autocorrelation function). 
acf(rv_dataPAGE3$GPD)
pacf(rv_dataPAGE3$GPD)
#--------#
#5.	Compute several ARMA models, each time by changing the parameter p and the parameter q, and record the AIC (Akaike information criterion).

#Define the zero paramater
inf <- 0.1/10^99

# Run a white noise Process
eps<-rnorm(500, 0, 1)

#Running Sim1 Arma (0:0)
sim1 <- arima.sim(list(ar=c(inf),ma=c(inf)),n=500,innov=eps)

#Finding best AIC fit using forecast library
sim1bestfit <- auto.arima(sim1)
print(sim1bestfit)

#We filter the important value = AIC. 
#And we save it into a variable
AICsim1 <- sim1bestfit$aic

# Repeat the process for Sim2 Arma(0:1)
sim2 <- arima.sim(list(ar=c(inf),ma=c(1)),n=500,innov=eps)
sim2bestfit <- auto.arima(sim2)
print(sim2bestfit)
AICsim2 <- sim2bestfit$aic

# Repeat the process for Sim2 Arma(0:2)
sim3 <- arima.sim(list(ar=c(inf),ma=c(2)),n=500,innov=eps)
sim3bestfit <- auto.arima(sim3)
print(sim3bestfit)
AICsim3 <- sim3bestfit$aic

# Repeat the process for Sim2 Arma(0:3)
sim4 <- arima.sim(list(ar=c(inf),ma=c(3)),n=500,innov=eps)
sim4bestfit <- auto.arima(sim4)
print(sim4bestfit)
AICsim4 <- sim4bestfit$aic


# We made plots to summarize all ARMA into one place
par(mfrow=c(2,2)) # 2x2 array of plots
plot(sim1, main="ARMA(0,0)", col="blue")
plot(sim2, main="ARMA(0,1)", col="blue")
plot(sim3, main="ARMA(0,2)", col="blue")
plot(sim4, main="ARMA(0,3)", col="blue")

#--------#
#6.	Perform the ARMA regression that you determined in Step 5c. Show the table of results in your report (no need to comment on these results for now).

#--------#
#7.	Using the residuals from the regression in Step 6:
acf(model$residuals, type = "correlation")
Box.test(resid(fit1),type="Ljung",lag=20,fitdf=1) changer fit1
#--------#
#8.	Perform an out-of-sample forecast of the quarters 2020Q1 until 2020Q4. 

#--------#
#9.	Compute the GDP forecast, and plot in a graph the original GDP series together with its forecast (can be done either in Excel or in R). Comment on this graph. Tell whether the actual European GDP has already bounced back and reached the lost GDP due to the pandemic, according to the forecasted GDP had the pandemic not occurred.

#--------#
#10.	Write a conclusion to this case (about 300 words) among which:
#a.	Write a short summary of what you did
#b.	What do we learn from these results?
#c.	What do you think about the results?


  
