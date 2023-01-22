rm(list=ls())
library(readr)
library(ggplot2)
library(fpp3)
library(zoo)
library(xts)
library(tidyquant)
library(urca)
library(global_economy)
library(tsibbledata)
library(tsibble)
library(tibble)
library(dplyr)
library(tidyr)
library(lubridate)
library(feasts)
library(fable)
library(CPI)
library(priceR)
library(tidyverse)
library(strucchange)
library(cowplot)

# Load the data and call it 'empdata'
data <- read_csv("/Users/konradschulte/Documents/OneDrive - Inviso/Uni/Master/2. Semester/PA/Written Product/Boliga_CPH (final).csv", col_names = TRUE)
data <- data[133:252,]

summary(data)

Housing.infl <- data %>%
  mutate(Month = yearmonth(as.character(Month))) %>%
  as_tsibble(index = Month)

# Deflate Data
inflation<- read_csv("/Users/konradschulte/Documents/OneDrive - Inviso/Uni/Master/2. Semester/PA/Written Product/Inflation_clean.csv", col_names = FALSE)
inflation$Year = substr(inflation$X1,1,4)
inflation$Month = c(rep(c(1,2,3,4,5,6,7,8,9,10,11,12),times=21),c(1,2,3,4,5))
inflation$Day = rep(1,times=257)
inflation$Date <- as.yearmon(paste(inflation$Year, inflation$Month, inflation$Day), "%Y %m %d")
inflation$Value = as.numeric(substr(inflation$X1,6,10))

data$Date = as.yearmon(paste(data$Month))

data %>%
  left_join(inflation, by = "Date") %>%
  mutate(adj.Price = Price / Value * 100) -> join

data$Price <- join$adj.Price
data <- subset(data, select = -c(Date))
# Transform data to time series
Housing <- data %>%
  mutate(Month = yearmonth(as.character(Month))) %>%
  as_tsibble(index = Month)

################################################################################
# 1) EDA and Preprocessing                                                     
################################################################################



# 1.1) Non linearity
# Plot data
norm.d <- Housing %>%
  autoplot(Price) +
  labs(y = "Real Price (in DKK)")

log.d <-  Housing.log %>%
  autoplot(Price) +
  labs(y = "Real Price (in DKK)")

plot_grid(norm.d,  log.d, ncol = 2)
plot(real)
# This is not really linear data -> log transformations needed

Housing %>%
  features(Price, features=guerrero)
# guerrero = -0.258 confirms that log transformation is necessary

#Log transformation
Housing.log <- Housing
Housing.log$Price <- log(Housing.log$Price)

Housing.log %>%
  features(Price, features=guerrero)
# ??? Is that even a proof for the need for log transformation?

#------------------------------------------------------------------------------#

# 1.2) Trend
# deterministic/stochastic, detection: ts plot, decompostition, ADF
ts.plot(Housing.log)

# Decomposition (The additive model is useful when the seasonal variation is relatively constant over time.)

# STL estimates seasonality in an additive way.
dcmp_stl <- Housing.log %>%
  model(STL(Price)) %>%
  components()

dcmp_stl %>% autoplot()
(autoplot(decompose(as.ts(Housing.log), type="multiplicative"))
  + xlab("Year")
  + ggtitle("Classical Multiplicative Decomposition"))
# Trend very clear, seasonality seems to be there as well but be cautious,
# model forces seasonality into data!
# ??? Other decomposition?

#a <- stl(Housing.log,s.window="periodic")
#apply(a$time.series, 2, var) / var(Housing.log$Price)

# ACF of log transformed data
Housing.log %>%
  ACF(Price,lag_max=24) %>%
  autoplot() +
  labs( title = "ACF")

# ACF - all lags significant, slowly decaying, indicating: 
# autocorrelation > trend (non stationarity since the trend is strong and affects future?!)

# Box Pierce Test
Housing.log %>%
  features(Price, box_pierce, lag = 24, dof = 2)

# Also confirms autocorrelation (= trend) -> need for differencing to detect seasonality

# ADF
summary(ur.df(Housing.log$Price,type="trend"))
# Result? STRICT!
# t-value of tau3 > critical value (1pct)
# In the ADF test, the t-stat for tau3 is above the significance level at 10%, 
# therefore I accept the null that the variable has a unit root (-> non stationarity).

# ADF: larger, smaller, smaller
# -> present unit root + present drift + present (det.?) trend
# -> non stationary

# Conclusion: We have a trend: DESCRIBE
# Trend seems to be stochastic (see Word)

#------------------------------------------------------------------------------#

# 1.3) Seasonality
Housing.log%>%
  gg_season(Price)
# plots & decomposition
# Results? No seasonality, neither plots nor decomposition seem to show that

# Proof:
unitroot_nsdiffs(
  Housing$Price,
  alpha = 0.05,
  unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],
  differences = 0:2
)

# 1.4) Unit Root
# ADF/KPSS

summary(ur.df(Housing.log$Price,type="trend"))
# ADF: larger, smaller, smaller
# -> present unit root + present drift + present trend
# -> non stationarity


# KPSS
summary(ur.kpss(Housing.log$Price, type = "tau"))
# KPSS: smaller

# Proof for taking first difference (from feasts package):
unitroot_ndiffs(
  Housing$Price,
  alpha = 0.05,
  unitroot_fn = ~unitroot_kpss(.)["kpss_pvalue"],
  differences = 0:2
)

# Now, we have to take the first difference to make it stationary
Housing.1d <- Housing.log
Housing.1d %>%
  mutate(Price = Price - lag(Price)) -> Housing.1d
Housing.1d <- subset(Housing.1d, Price!="NA")

# ADF 
summary(ur.df(Housing.1d$Price,type="trend",selectlags = "AIC"))
# ADF(trend): smaller, larger, larger
# -> absent unit root + absent drift + absent trend

summary(ur.df(Housing.1d$Price,type="drift",selectlags = "AIC"))
# ADF(drift): smaller, larger
# -> absent unit root + absent drift

summary(ur.df(Housing.1d$Price,type="none",selectlags = "AIC"))
# ADF(none): smaller
# -> absent unit root

# KPSS
summary(ur.kpss(Housing.1d$Price, type = "tau"))
summary(ur.kpss(Housing.1d$Price, type = "mu"))
# KPSS: smaller
# -> absent unit root (= stationarity)

# ACF & PACF
plot.a <- Housing.1d %>%
  ACF(Price,lag_max=24) %>%
  autoplot() +
  labs( title = "ACF")
plot.b <- Housing.1d %>%
  PACF(Price,lag_max=24) %>%
  autoplot() +
  labs( title = "PACF")
plot_grid(plot.a, plot.b, ncol = 1)
# Evaluate: 
# The autocorrelation function confirms that the trend is not present anymore. 
# No sign of seasonality (significance of lags in ACF neglectable)
# One lag is significant, but it is not enough to clearly conclude that there is autocorrelation.
# 1 significant in ACF, 3 in PACF
# AR model:) (only if ACF decreasing and PACF 1/2 significant spikes)


# [OLD: MA MODEL :> (only one significant spike in ACF and waveish but diminishing)
#             (PACF some significant spikes in the beginning but decreasing)
# MA Model: order = sig. spikes in ACF (1) -> MA(1)]

#------------------------------------------------------------------------------#

# 1.5) Structural Break
sb_data <- Housing.1d %>%
  mutate(
    Lag0 = Price,
    Lag1 = lag(Price),
    Lag12 = lag(Price,12),
    Month = Month)

# QLR test with monthly lags
qlr1 <- Fstats(Lag0 ~ Lag1 + Lag12, data = sb_data, from = 0.15)
test1 <- sctest(qlr1, type = "supF")
test1

breakpoints(qlr1, alpha = 0.05)

plot(qlr1, alpha = 0.05, main = "QLR Test")
lines(breakpoints(qlr1))
# This indicates that there are no structural breaks 

################################################################################
# 2) Split Sample
################################################################################

train <- Housing.log %>%
  filter_index(. ~ "2020 Jan")
test <- Housing.log %>%
  filter_index("2020 Jan" ~ .)

################################################################################
# 3) Models
################################################################################

models <- train %>%
  model(
    # ETS
    auto.ets = ETS(Price),
    my.ets = ETS(Price ~ error("A") + trend("A") + season("N")),
    
    # ARIMA
    auto.arima = ARIMA(Price),
    my.arima1 = ARIMA(Price ~ pdq(0,1,2) + PDQ(0,0,0)),
    my.arima2 = ARIMA(Price ~ pdq(0,1,2) + PDQ(0,0,1))
  )

# Report for Auto ETS
report(models%>%select(auto.ets))

# alpha = 0.47: is near 0.5, therefore it weighs all observations comparably heavy and does 
# not give the last observation a much stronger weight
# beta = 0.01: slope has a strong inertia (trend is quite irrelevant)
# gamma = 0.0003: gamma is very small, suggesting a negligible seasonality if any

# Report for guessed ETS
report(models%>%select(my.ets))

# alpha = 0.45: is near 0.5, therefore it weighs all observations comparably heavy and does 
# not give the last observation a much stronger weight
# beta = 0.0002: slope has an even stronger inertia (trend is quite irrelevant)
# no gamma since no seasonality assumed

# Report for Auto ARIMA (won't be evaluated)
report(models%>%select(auto.arima))

# Report for guessed ARIMA (won't be evaluated)
report(models%>%select(my.arima1))
report(models%>%select(my.arima2))

################################################################################
# 4) Evaluate Residuals 
################################################################################

# 4.1) for Auto ETS
models %>%
  select(auto.ets) %>%
  gg_tsresiduals(type = "innovation")

# + no heteroskedasticity
# + only one significant spike
# - no normal distribution 


# Ljung Box Test (we want HIGH p-value -> NULL not rejected, then resid. independent)
models %>%
  select(auto.ets) %>%
  residuals()  %>%
  features(.resid, features = ljung_box, lag = 24, dof = 8)

# + Residuals pass Ljung Box Test (p-value > 0.05, NULL not rejected)

#------------------------------------------------------------------------------#

# 4.2) for guessed ETS
models %>%
  select(my.ets) %>%
  gg_tsresiduals(type = "innovation")

# - probably heteroskedasticity?
# + only one significant spike
# + quite normally distributed

# Ljung Box Test
models %>%
  select(my.ets) %>%
  residuals()  %>%
  features(.resid, features = ljung_box, lag = 24, dof = 8)

# - Null of Ljung Box is rejected!!! -> with 12 lags? Ex.3
# Try testing for joint auto-correlation up to length 12 and see whether
# the test results change??? (Ex.3)

#------------------------------------------------------------------------------#

# 4.3) for Auto ARIMA
models %>%
  select(auto.arima) %>%
  gg_tsresiduals(type = "innovation")

# + probably no heteroskedasticity?
# + only one significant spike
# + quite normally distributed

# Ljung Box Test
models %>%
  select(auto.arima) %>%
  residuals()  %>%
  features(.resid, features = ljung_box, lag = 24, dof = 19)

# + Residuals pass Ljung Box Test

#------------------------------------------------------------------------------#

# 4.4) for guessed ARIMA
models %>%
  select(my.arima1) %>%
  gg_tsresiduals(type = "innovation")

# - probably heteroskedasticity?
# + only one significant spike
# + quite normally distributed

models %>%
  select(my.arima2) %>%
  gg_tsresiduals(type = "innovation")

# Ljung Box Test
models %>%
  select(my.arima) %>%
  residuals()  %>%
  features(.resid, features = ljung_box, lag = 24, dof = 19)

# - Residuals do not pass Ljung Box Test
# Try testing for joint auto-correlation up to length 12 and see whether
# the test results change??? (Ex.3)
models %>%
  select(my.arima) %>%
  residuals()  %>%
  features(.resid) -> res
res$Lag0 <- res$.resid
res$Lag1 <- lag(res$.resid)
autoplot(ACF(res, var=Lag0))

################################################################################
# 5) Forecast Test Set
################################################################################

# Forecast Auto ETS on test set
plot.1 <- models %>%
  select(auto.ets) %>%
  forecast(test) %>%
  autoplot(Housing) +
  labs(title = "Auto ETS model")
print(plot.1)

# Forecast guessed ETS on test set
plot.2 <- models %>%
  select(my.ets) %>%
  forecast(test) %>%
  autoplot(Housing) +
  labs(title = "Forecasts from guessed ETS model")
print(plot.2)

# Forecast Auto ARIMA on test set
plot.3 <- models %>%
  select(auto.arima) %>%
  forecast(test) %>%
  autoplot(Housing) +
  labs(title = "Auto ARIMA model")
print(plot.3)

# Forecast both models for test set
plot.4 <- models %>%
  select(auto.arima,auto.ets) %>%
  forecast(test) %>%
  autoplot(test,level = NULL)

plot_grid(plot.1, plot.3,ncol = 1)
print(plot.4)

# Forecast of all models for test set (with train set)
models %>%
  forecast(test) %>%
  autoplot(Housing,level = NULL)



################################################################################
# 6) Evaluate Forecast with Accuracy Measures
################################################################################

train <- Housing %>%
  filter_index(. ~ "2020 Jan")
test <- Housing %>%
  filter_index("2020 Jan" ~ .)

models %>%
  forecast(test) %>%
  accuracy(Housing)

models %>%
  forecast(train) %>%
  accuracy(Housing)

accuracy(models$auto.ets)
AIC_ets = ets(train, ic = "aic" , allow.multiplicative.trend = TRUE, "AAdA")
ETS

################################################################################
# 7) Forecasting the next 2 years (with inflation)
################################################################################

best_model <- Housing %>%
  model(auto.ets = ETS(Price))

infl.rate = 1.07
fc.my.arima <- best_model %>%
  select(auto.ets) %>%
  forecast(h=24)
fc.my.arima$Price <- fc.my.arima$Price*infl.rate
plot.5 <- fc.my.arima %>%
  autoplot(Housing.infl) 
print(plot.5)



---------

install.packages("forecast")
library(forecast)
models %>%
  select(auto.ets) %>%
  residuals() %>%
  checkresiduals(features(.resid))
checkresiduals(models$auto.ets$residuals)





--------------------------------------------------------------------------------







# Forecast guessed ETS on test set
plot.2 <- models %>%
  select(my.ets) %>%
  forecast(h=24) %>%
  autoplot(Housing) +
  labs(title = "Forecasts from guessed ETS model")
print(plot.2)

# Forecast Auto ARIMA on test set
plot.3 <- models %>%
  select(auto.arima) %>%
  forecast(h=24) %>%
  autoplot(Housing) +
  labs(title = "Forecasts from Auto ARIMA model")
print(plot.3)

# Forecast guessed ARIMA on test set
plot.4 <- models %>%
  select(my.arima) %>%
  forecast(h=60) %>%
  autoplot(Housing) +
  labs(title = "Forecasts from guessed ARIMA model")
print(plot.4)

# COPY CAT
arima = train %>%
  model(ARIMA(Price,ic='aic'))
arima <- train %>%
  model(ARIMA(train$Price, d = 1, D = 1))
summary(arima)
        
color1 = c("firebrick1","darkolivegreen4", "cornflowerblue", "gold1", "black")
overall_plot <- autoplot(Housing, series = "Actual Data") +
  autolayer(models$auto.ets, series = "Auto ETS Forecast", PI = FALSE) +
  autolayer(models$my.ets, series = "Guessed ETS Forecast", PI = FALSE) +
  autolayer(models$auto.arima, series = "Auto ARIMA Forecast", PI = FALSE) +
  autolayer(models$my.arima, series = "Guessed ARIMA Forecast", PI = FALSE) +
  xlab("Year") +
  ylab("Global CO2 Emission") +
  ggtitle("Forecasted Development of Global CO2")
overall_plot + scale_color_manual(values = color1)



test <- models %>%
  select(auto.ets) %>%
  forecast(test) %>%
  autoplot(test)






# OLD DEFLATION

data$Year <- year(as.character(data$Month))
data$Month2 <- month(as.character(data$Month))

dk_economy <- global_economy %>%
  filter(Code == "DNK")

data %>%
  left_join(dk_economy, by = "Year") %>%
  mutate(adj.Price = Price / CPI * 100) -> join

# Plot ACF Test 
Housing.1d %>%
  ACF(Price,lag_max=239) %>% 
  autoplot() + ggtitle("ACF Housing Prices")

data$Year <- year(as.character(data$Month))
data$Month2 <- month(as.character(data$Month))

dk_economy <- global_economy %>%
  filter(Code == "DNK")

data %>%
  left_join(dk_economy, by = "Year") %>%
  mutate(adj.Price = Price / CPI * 100) -> joinHousing %>%
  ACF(log.Price) %>% 
  autoplot() + ggtitle("ACF LOG")

Housing%>%
  mutate(growth.Price = log(Price) - lag(log(Price))) -> Housing

Housing %>%
  ACF(growth.Price,lag_max=48) %>% 
  autoplot() + ggtitle("ACF LOG")
# We see autocorrelation and increasing trend

# Seasonal impact
ts_length <- length(as.ts(Housing))
freq <- frequency(as.ts(Housing))
periods <- ts_length%/%freq
## Index of cumulative month
index <- seq(1, ts_length, by = freq) - 1
## Get mean by month
mean_month <- numeric(freq)
for (i in 1:freq) {
  mean_month[i] <- mean(as.ts(Housing)[index + i], na.rm = TRUE)
}
mean_month <- mean_month - mean(mean_month)
plot.ts(mean_month, ylab = "Seasonal effect", xlab = "Month", cex = 1)

# Quarterly data?
"""
Housing <- Housing_monthly %>% 
  mutate(Month = ym(Month)) %>% 
  as_tsibble(index = Month) %>% 
  index_by(year_quarter = ~ yearquarter(.)) %>% 
  #summarise(Price = median(Price)) %>%
  summarise(Price = median(Adjusted_Price))
"""
