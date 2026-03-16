#' ---
#' title: Time Series Week NNN
#' author: You
#' date: Today
#' ---
#install.packages("prophet")
library(prophet)
library(Rcpp);library(rlang)
#1. load the library using library(prophet)
#Results indicate two other packages need to be downloaded
#in order for library prophet to work
#Loading required package: Rcpp
#Loading required package: rlang

#Looking at suggested method by Meta

#install.packages("remotes")
#remotes::install_github('facebook/prophet@*release', subdir='R',force = TRUE)

#The system required 6 packages (much more than the default install), which were downloaded
#after required verification on number of updates specified

knitr::opts_chunk$set(echo = TRUE)
library(readxl)

internet_reference_tables <- read_excel("data/internet reference tables.xlsx", sheet="My Data", skip=5)
internet_reference_tables <- internet_reference_tables[1:231, ]

#2. Setting up Time Series:  INTERNET SALES: VALUE NON-SEASONALLY ADJUSTED AVERAGE MONTHLY INTERNET SALES IN POUNDS MILLION
#library(zoo)
ds <- internet_reference_tables[,1]
start_date <- as.Date("2006-11-30")
end_date <- as.Date("2026-01-31")
ds_1 <- seq.Date(from = start_date, to = end_date, by = "1 month")
y <- as.numeric(internet_reference_tables[[2]])
length(ds_1);length(y)      #, to ensure the data frame is of equal dimension
d <- data.frame(ds = ds_1, y = y)

#3. Exploring Prophet
m <- prophet(d,weekly.seasonality=FALSE,daily.seasonality=FALSE)
?prophet
#Disabled as dataset is too spares to reliably estimates these seasonal patterns
#4. Forecasting with data

f <- make_future_dataframe(m, periods = 11)
?make_future_dataframe

#5. Prediction

p <- predict(m, f)

#6. Plot

plot(m, p, main = "NON-SEASONALLY ADJUSTED AVERAGE MONTHLY INTERNET SALES, in pounds million",
     xlab = "Months between 2006-2026", ylab = "Sales in million pounds")


prophet_plot_components(m,p)
?prophet_plot_components
# Linear regression of Data
t <- c(1:231)
model <- lm(y~t)
plot(y, main = "Linear trend in internet sales", xlab = "time", ylab = "Internet sales, pound in millions")
abline(model, col = "red")
res <- residuals(model)
plot(res, main = "Residual")

summary(model)



library(feasts)
library(tseries)
library(lmtest)
library(forecast)

#Let the significant figure be 5%

#Dickey-Fuller test
#null hypothesis: time series is not stationary
tseries::adf.test(y)
#The p value is large, indicating time series is not stationary

#Kwiatkowski-Phillips-Schmidt-Shin test
#null hypothesis: time series is stationary
feasts::unitroot_kpss(y)
#The p value is small, indicating t.s. is not stationary

#Breusch-Pagan test
#null hypothesis: time series has homoskedasticity
lmtest::bptest(model)
#a symbolic description for the model to be tested (or a fitted "lm" object)
#The p value is small, indicating t.s. has heteroskedasticity

#Ljung-Box test
#null hypothesis: xt/time series is white noise
Box.test(y , type="Ljung-Box")
#The p value is very small, indicating t.s. is not white noise

lam <- forecast::BoxCox.lambda(y)
box_c <- forecast::BoxCox(y,lam)
plot(box_c,main = "Box-Cox of Internet sales", ylab = "filtered internet sales" )
#optimal lambda for Box-Cox transformation, lambda = -0.09388
# the transformation will slightly compress large values and slightly expand small values.
#The effect is similar to a mild inverse power transformation.








