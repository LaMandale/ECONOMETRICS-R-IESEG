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
#a.

RGDP <- Delt(rv_dataPAGE3$GPD)

ARMARGDP <- arima(RGDP, order = c(0,0,0))
ARMARGDP1 <- arima(RGDP, order = c(0,0,1))
ARMARGDP2 <- arima(RGDP, order = c(0,0,2))
ARMARGDP3 <- arima(RGDP, order = c(0,0,3))
ARMARGDP10 <- arima(RGDP, order = c(1,0,0))
ARMARGDP4 <- arima(RGDP, order = c(1,0,1))
ARMARGDP5 <- arima(RGDP, order = c(1,0,2))
ARMARGDP6 <- arima(RGDP, order = c(1,0,3))
ARMARGDP11 <- arima(RGDP, order = c(2,0,0))
ARMARGDP12 <- arima(RGDP, order = c(2,0,1))
ARMARGDP7 <- arima(RGDP, order = c(2,0,2))
ARMARGDP8 <- arima(RGDP, order = c(2,0,3))
ARMARGDP13 <- arima(RGDP, order = c(3,0,0))
ARMARGDP14 <- arima(RGDP, order = c(3,0,1))
ARMARGDP15 <- arima(RGDP, order = c(3,0,2))
ARMARGDP16 <- arima(RGDP, order = c(3,0,3))
###b. The Model that has the lowest AIC is ARMA(0,1) with an AIC equal to -536.18.
#c.
#--------#
#6.	Perform the ARMA regression that you determined in Step 5c. Show the table of results in your report (no need to comment on these results for now).
ARMARGDP1 <- arima(RGDP, order = c(0,0,1))
print(ARMARGDP1)
#--------#
#7.	Using the residuals from the regression in Step 6:
resid <- residuals(ARMARGDP1)
plot.ts(resid)
acf(resid, main="Resid ARMA(0,1)")
Box.test(resid, lag = 4, type = c("Ljung-Box"))
Box.test(resid, lag = 8, type = c("Ljung-Box"))
Box.test(resid, lag = 12, type = c("Ljung-Box"))

#--------#
#8.	Perform an out-of-sample forecast of the quarters 2020Q1 until 2020Q4. 
dhp <- EXRSP500
armadhp <- arima(dhp, order = c(2,0,0))
fitted <- fitted(armadhp)
plot.ts(dhp)
lines(fitted, col = "blue")

f <- (predict(armadhp, se.fit = TRUE, n.ahead = 60))

plot.ts(dhp, xlim = c(300,length(dhp)+60))
lines((f$pred), col="blue")
CIhi <- f$pred+2*f$se
CIlo <- f$pred-2*f$se
lines(CIhi, col="blue", lty=2)
lines(CIlo, col="blue", lty=2)

#--------#
#9.	Compute the GDP forecast, and plot in a graph the original GDP series together with its forecast (can be done either in Excel or in R). Comment on this graph. Tell whether the actual European GDP has already bounced back and reached the lost GDP due to the pandemic, according to the forecasted GDP had the pandemic not occurred.
gdpf <- Delt(rv_dataPAGE3$GPD)
armagdpf <- arima(gdpf, order = c(2,0,0))
fittedgdp <- fitted(armagdpf)
plot.ts(gdpf)
lines(fittedgdp, col = "blue")

fgdp <- (predict(armagdpf, se.fit = TRUE, n.ahead = 6))

plot.ts(gdpf, xlim2 = c(300,length(gdpf)+6))
lines((fgdp$pred), col="blue")
CIhi <- fgdp$pred+2*fgdp$se
CIlo <- fgdp$pred-2*fgdp$se
lines(CIhi, col="blue", lty=2)
lines(CIlo, col="blue", lty=2)

#--------#
#10.	Write a conclusion to this case (about 300 words) among which:
#a.	Write a short summary of what you did
#b.	What do we learn from these results?
#c.	What do you think about the results?


  
