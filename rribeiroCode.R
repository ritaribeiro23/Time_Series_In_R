
library('tseries')
library('forecast')
library('lattice')

kkk <- scan(file = "C:/Users/admin/Desktop/MADSAD/MPST/Trabalho/values.csv")
ecm2 <- ts(kkk[1:204], start = c(2000, 1), deltat = 1/12)


ecm <- scan(file = "C:/Users/admin/Desktop/MADSAD/MPST/Trabalho/values.csv")
ecm <- ts(ecm, start=c(2000, 1), deltat = 1/12)

plot(ecm, 
     main = "E-commerce revenue in the USA", 
     ylab="Values in millions of dollars",
     xlab='Time (in Months)')

monthplot(ecm, 
          main = "Month plot for E-commerce revenue", 
          ylab='Values in million of dollars',
          xlab = 'Month')

plot(decompose(ecm, type="additive")$seasonal, 
     main="a) Seasonal component of ECM as an Additive Model",
     ylab='')

plot(decompose(log(ecm), type="additive")$seasonal, 
     main="b) Seasonal component of log ECM as an Additive Model",
     ylab='')

plot(decompose(ecm, type="multiplicative")$seasonal,
     main="c) Seasonal component of ECM as a Multiplicative Model",
     ylab='')

plot(decompose(log(ecm), type="multiplicative")$seasonal,
     main="d) Seasonal component of log ECM as a Multiplicative Model",
     ylab='')

seasonplot(ecm,
           main = "Seasonplot for E-commerce revenue in the USA", 
           ylab="Values in millions of dollars",
           xlab='Time (in Months)',
           year.labels = TRUE,
           col = 1:20,
           pch = 10)

plot(decompose(ecm, type="additive")$trend, 
     main="Trend of ECM as an Additive Model",
     ylab='')


acf(as.vector(ecm), 
    lag.max = 36, 
    main = "a) Sample ACF for E-commerce Revenue Time Series")

pacf(as.vector(ecm), 
    lag.max = 36, 
    main = "b) Sample PACF for E-commerce Revenue Time Series")

## Differentiation

firstDiff <- diff(ecm2)
plot(firstDiff, main = "First Differentiation of the ecm time series")

acf(ts(firstDiff, freq=1), 
    main="a) Sample ACF for the First Differentiation of the ecm time series",
    lag.max = 36)

pacf(ts(firstDiff, freq=1), 
    main="b) Sample PACF for the First Differentiation of the ecm time series",
    lag.max = 36)

adf.test(firstDiff, alternative = 's')

#Decomposition

decECM <- decompose(ecm2, type = "additive")
plot(decECM)

classicTrend <- c(decECM$trend)
classicTrend <- classicTrend[-c(1:6)]
classicTrend <- classicTrend[-c(193:198)]
classicTrendTS <- ts(classicTrend,
                     start = c(2000, 7),
                     deltat = 1/12)
classicTrendPred <- HoltWinters(classicTrendTS,
                           gamma = FALSE)
classicTrendFcst <- predict(classicTrendPred,
                       n.ahead = 18)
classicPred <- ts(classicTrendFcst[7:18]+decECM$figure,
                  start = c(2017, 1),
                  deltat = 1/12)

accuracy(classicPred, k)


stlECM <- stl(ecm2,
              s.window = "periodic",
              robust = TRUE)
plot(stlECM)

stlFcst <- forecast(stlECM, h=12)

accuracy(stlFcst, k)

# Holt Winters
k <- scan(file = "C:/Users/admin/Desktop/MADSAD/MPST/Trabalho/lest12months.csv")
k <- ts(k, start=c(2017, 1), deltat = 1/12)

HW_ECM <- ets(ecm2, model="ZZZ")

HWfcst <- forecast(HW_ECM, h = 12)

autoplot(HWfcst, ylab = 'Values in millions of dollars')

HW_ACC <- accuracy(HWfcst, k)
HW_ACC

HW_residuals <- residuals(HWfcst)

acf(HW_residuals, 
    lag.max = 36,
    main="a) Sample ACF for the residuals of the HW forecast")

pacf(HW_residuals, 
     lag.max = 36,
     main="b) Sample PACF for the residuals of the HW forecast")

Box.test(HW_residuals, type = "Ljung-Box")

qqnorm(HW_residuals,
       main="QQPlot for residuals")
qqline(HW_residuals)



## Arima
#differentiations

secondDiff <- diff(firstDiff, 12)

acf(ts(secondDiff, freq=1), 
    main="a) Sample ACF for the First Seasonal Differentiation of the ecm time series",
    lag.max = 36)

pacf(ts(secondDiff, freq=1), 
     main="b) Sample PACF for the First Seasonal Differentiation of the ecm time series",
     lag.max = 36)

# (0,1,2)x(2,1,2)
fit1 <- arima(ecm2, 
      order=c(0,1,2), 
      seasonal=list(order=c(2,1,2), period=12)
      )

# (0,1,2)x(1,1,1)
fit2 <- arima(ecm2, 
      order=c(0,1,2), 
      seasonal=list(order=c(1,1,1), period=12)
)

# (0,1,2)x(0,1,2)
arima(ecm2, 
      order=c(0,1,2), 
      seasonal=list(order=c(0,1,2), period=12)
)

# (1,1,1)x(2,1,2)
arima(ecm2, 
      order=c(1,1,1), 
      seasonal=list(order=c(2,1,2), period=12)
)

# (1,1,1)x(1,1,2)
arima(ecm2, 
      order=c(1,1,1), 
      seasonal=list(order=c(1,1,2), period=12)
)

# (1,1,1)x(0,1,2)
arima(ecm2, 
      order=c(1,1,1), 
      seasonal=list(order=c(0,1,2), period=12)
)

# (2,1,1)x(2,1,2)
arima(ecm2, 
      order=c(2,1,1), 
      seasonal=list(order=c(2,1,2), period=12)
)

# (2,1,1)x(1,1,2)
arima(ecm2, 
      order=c(2,1,1), 
      seasonal=list(order=c(1,1,2), period=12)
)

# (2,1,1)x(0,1,2)
fit3 <- arima(ecm2, 
      order=c(2,1,1), 
      seasonal=list(order=c(0,1,2), period=12)
)

#Round 2

# (0,1,2)x(1,1,2)
arima(ecm2, 
      order=c(0,1,2), 
      seasonal=list(order=c(1,1,2), period=12)
)

# (0,1,2)x(2,1,1)
arima(ecm2, 
      order=c(0,1,2), 
      seasonal=list(order=c(2,1,1), period=12)
)

# (0,1,2)x(2,1,0)
arima(ecm2, 
      order=c(0,1,2), 
      seasonal=list(order=c(2,1,0), period=12)
)

# (1,1,2)x(0,1,2)
arima(ecm2, 
      order=c(1,1,2), 
      seasonal=list(order=c(0,1,2), period=12)
)

# (2,1,2)x(0,1,2)
arima(ecm2, 
      order=c(2,1,2), 
      seasonal=list(order=c(0,1,2), period=12)
)

# (2,1,0)x(0,1,2)
fit4 <- arima(ecm2, 
      order=c(2,1,0), 
      seasonal=list(order=c(0,1,2), period=12)
)

# overfitting
# (0,1,3)x(2,1,2)
arima(ecm2, 
     order=c(0,1,3), 
     seasonal=list(order=c(2,1,2), period=12)
)

# (0,1,3)x(1,1,1)
arima(ecm2, 
      order=c(0,1,3), 
      seasonal=list(order=c(1,1,1), period=12)
)

# (3,1,1)x(0,1,2)
arima(ecm2, 
      order=c(3,1,1), 
      seasonal=list(order=c(0,1,2), period=12)
)

# (3,1,0)x(0,1,2)
arima(ecm2, 
      order=c(3,1,0), 
      seasonal=list(order=c(0,1,2), period=12)
)


Box.test(fit1$residuals, type = 'Ljung')
Box.test(fit2$residuals, type = 'Ljung')
Box.test(fit3$residuals, type = 'Ljung')
Box.test(fit4$residuals, type = 'Ljung')

# Normality tests for model 0.1
plot(fit1$residuals, 
     main="a) Plot for residuals of model 0.1")

acf(fit1$residuals, 
    lag.max = 36,
    main="b) Sample ACF for residuals of model 0.1")

qqnorm(fit1$residuals,
       main="c) QQPlot for residuals of model 0.1")
qqline(fit1$residuals)


# Normality tests for model 0.2
plot(fit2$residuals,
     main="a) Plot for residuals of model 0.2")
acf(fit2$residuals, 
    lag.max = 36,
    main="b) Sample ACF for residuals of model 0.2")
qqnorm(fit2$residuals,
       main="c) QQPlot for residuals of model 0.2")
qqline(fit2$residuals)


# Normality tests for model 2.3
plot(fit3$residuals,
     main="a) Plot for residuals of model 2.3")
acf(fit3$residuals, 
    lag.max = 36,
    main="b) Sample ACF for residuals of model 2.3")
qqnorm(fit3$residuals,
       main="c) QQPlot for residuals of model 2.3")
qqline(fit3$residuals)

# Normality tests for model 2.5
plot(fit4$residuals,
     main="a) Plot for residuals of model 2.5")
acf(fit4$residuals, 
    lag.max = 36,
    main="b) Sample ACF for residuals of model 2.5")
qqnorm(fit4$residuals,
       main="c) QQPlot for residuals of model 2.5")
qqline(fit4$residuals)

# Information Criteria

# Model 0.1

AIC(fit1)
BIC(fit1)

p <- length(fit1$coef) + 1
s <- length(fit1$res) - fit1$arma[6] - fit1$arma[7]*fit1$arma[5]
AIC(fit1) + 2*p*(s/(s-p-1)-1)

# Model 0.2
AIC(fit2)
BIC(fit2)

p <- length(fit2$coef) + 1
s <- length(fit2$res) - fit2$arma[6] - fit2$arma[7]*fit2$arma[5]
AIC(fit2) + 2*p*(s/(s-p-1)-1)

# Model 2.3

AIC(fit3)
BIC(fit3)

p <- length(fit3$coef) + 1
s <- length(fit3$res) - fit3$arma[6] - fit3$arma[7]*fit3$arma[5]
AIC(fit3) + 2*p*(s/(s-p-1)-1)

# Model 2.5

AIC(fit4)
BIC(fit4)

p <- length(fit4$coef) + 1
s <- length(fit4$res) - fit4$arma[6] - fit4$arma[7]*fit4$arma[5]
AIC(fit4) + 2*p*(s/(s-p-1)-1)

# FORECAST

SARfcst1 <- forecast(fit1, h=12)
accuracy(SARfcst1, k)

SARfcst2 <- forecast(fit2, h=12)
accuracy(SARfcst2, k)

SARfcst4 <- forecast(fit4, h=12)
accuracy(SARfcst4, k)


plot(SARfcst2,
     ylab = "Values in millions of dolars",
     xlab = "Time")

plot(SARfcst3,
     ylab = "Values in millions of dolars",
     xlab = "Time")

write.csv(SARfcst2, 
          file = "C:/Users/admin/Desktop/MADSAD/MPST/Trabalho/model02.csv")

write.csv(SARfcst3, 
          file = "C:/Users/admin/Desktop/MADSAD/MPST/Trabalho/model23.csv")

