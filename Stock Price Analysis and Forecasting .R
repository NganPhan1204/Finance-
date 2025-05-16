library(quantmod)    # For downloading stock data
library(tidyquant)   # Tidy-friendly wrapper for financial data
library(dplyr)       # Data manipulation
library(lubridate)   # Date handling
library(ggplot2)                # Plotting
library(PerformanceAnalytics) # Return, volatility, Sharpe ratio
library(Rcpp)
library(rlang)
library(forecast)      # ARIMA, exponential smoothing
library(prophet)       # Facebook Prophet model
library(tibbletime)    # Time series tibble extension
library(timetk)        # Time series tools for forecasting
#Stock Stickers######
tickers <- c("AAPL", "MSFT","JNJ","TSLA","XOM")
start_date <- as.Date("2015-05-01")
end_date <-Sys.Date()
getSymbols(tickers, src="yahoo", from = start_date, to = end_date)
head(AAPL)
head(MSFT)
head(JNJ)
head(TSLA)
head(XOM)
adj_close_xts <- do.call(merge, lapply(tickers, function(sym) Ad(get(sym))))
colnames(adj_close_xts) <- tickers
head(adj_close_xts)

###Return and Sharpe ratio########
returns<- Return.calculate(adj_close_xts)
returns<-na.omit(returns)
head(returns)
sharpe_ratios <- SharpeRatio.annualized(returns, Rf = 0.02/252)
head(sharpe_ratios)

##Time-Series
#AAPL
aapl_ts <- ts(adj_close_xts$AAPL, frequency = 252)  # Daily with ~252 trading days/year
decomp_aapl <- decompose(aapl_ts, type = "multiplicative")
plot(decomp_aapl)
title( main = "Decomposition - AAPL", line = -1)
# MSFT
msft_ts <- ts(adj_close_xts$MSFT, frequency = 252)
decomp_msft <- decompose(msft_ts, type = "multiplicative")
plot(decomp_msft)
title(main = "Decomposition - MSFT", line=-1)
# JNJ
jnj_ts <- ts(adj_close_xts$JNJ, frequency = 252)
decomp_jnj <- decompose(jnj_ts, type = "multiplicative")
plot(decomp_jnj) 
title(main = "Decomposition - JNJ", line=-1)
# TSLA
tsla_ts <- ts(adj_close_xts$TSLA, frequency = 252)
decomp_tsla <- decompose(tsla_ts, type = "multiplicative")
plot(decomp_tsla) 
title(main = "Decomposition - TSLA", line =-1)


##Forecasting Price Using ARIMA####
#AAPL###
aapl_ts <- ts(adj_close_xts$AAPL, frequency = 252)
model_aapl <- auto.arima(aapl_ts)
forecast_aapl <- forecast(model_aapl, h = 30)  # Forecast next 30 days
autoplot(forecast_aapl)+ ggtitle("ARIMA Forecast-AAPL")
# MSFT
msft_ts <- ts(adj_close_xts$MSFT, frequency = 252)
model_msft <- auto.arima(msft_ts)
forecast_msft <- forecast(model_msft, h = 30)
autoplot(forecast_msft) + ggtitle("ARIMA Forecast - MSFT")
# JNJ
jnj_ts <- ts(adj_close_xts$JNJ, frequency = 252)
model_jnj <- auto.arima(jnj_ts)
forecast_jnj <- forecast(model_jnj, h = 30)
autoplot(forecast_jnj) + ggtitle("ARIMA Forecast - JNJ")
# TSLA
tsla_ts <- ts(adj_close_xts$TSLA, frequency = 252)
model_tsla <- auto.arima(tsla_ts)
forecast_tsla <- forecast(model_tsla, h = 30)
autoplot(forecast_tsla) + ggtitle("ARIMA Forecast - TSLA")
# XOM
xom_ts <- ts(adj_close_xts$XOM, frequency = 252)
model_xom <- auto.arima(xom_ts)
forecast_xom <- forecast(model_xom, h = 30)
autoplot(forecast_xom) + ggtitle("ARIMA Forecast - XOM")

#### Forecasting with Facebook Prophet###
library(prophet)
aapl_df <- data.frame(ds = index(adj_close_xts), y = as.numeric(adj_close_xts$AAPL))
model_aapl <- prophet(aapl_df, daily.seasonality = TRUE)
future_appl<- make_future_dataframe(model_aapl, periods = 30)
forecast_aapl <- predict(model_aapl, future_appl)
plot(model_aapl, forecast_aapl)
prophet_plot_components(model_aapl, forecast_aapl)

# MSFT
msft_df <- data.frame(ds = index(adj_close_xts), y = as.numeric(adj_close_xts$MSFT))
model_msft <- prophet(msft_df, daily.seasonality = TRUE)
future_msft <- make_future_dataframe(model_msft, periods = 30)
forecast_msft <- predict(model_msft, future_msft)
plot(model_msft, forecast_msft)
prophet_plot_components(model_msft, forecast_msft)

# JNJ
jnj_df <- data.frame(ds = index(adj_close_xts), y = as.numeric(adj_close_xts$JNJ))
model_jnj <- prophet(jnj_df, daily.seasonality = TRUE)
future_jnj <- make_future_dataframe(model_jnj, periods = 30)
forecast_jnj <- predict(model_jnj, future_jnj)
plot(model_jnj, forecast_jnj)
prophet_plot_components(model_jnj, forecast_jnj)

# TSLA
tsla_df <- data.frame(ds = index(adj_close_xts), y = as.numeric(adj_close_xts$TSLA))
model_tsla <- prophet(tsla_df, daily.seasonality = TRUE)
future_tsla <- make_future_dataframe(model_tsla, periods = 30)
forecast_tsla <- predict(model_tsla, future_tsla)
plot(model_tsla, forecast_tsla)
prophet_plot_components(model_tsla, forecast_tsla)

# XOM
xom_df <- data.frame(ds = index(adj_close_xts), y = as.numeric(adj_close_xts$XOM))
model_xom <- prophet(xom_df, daily.seasonality = TRUE)
future_xom <- make_future_dataframe(model_xom, periods = 30)
forecast_xom <- predict(model_xom, future_xom)
plot(model_xom, forecast_xom)
prophet_plot_components(model_xom, forecast_xom)

#SMA##########
#AAPL
aapl_df_plot <- tibble(date = index(adj_close_xts), price = adj_close_xts$AAPL) %>%
  mutate(SMA_50 = SMA(price, n = 50),
         SMA_200 = SMA(price, n = 200))
ggplot(aapl_df_plot, aes(x = date)) +
  geom_line(aes(y = price), color = "black") +
  geom_line(aes(y = SMA_50), color = "blue") +
  geom_line(aes(y = SMA_200), color = "red") +
  labs(title = "AAPL with 50 & 200-Day Moving Averages")

#MSFT
msft_df_plot <- tibble(date =index(adj_close_xts), price= adj_close_xts$MSFT) %>%
  mutate(SMA_50 = SMA (price , n=50),
         SMA_200 =SMA( price, n=200))
ggplot(msft_df_plot, aes(x = date))+
  geom_line(aes(y = price), color = "black")+
  geom_line(aes(y = SMA_50), color ="blue")+
  geom_line(aes( y = SMA_200), color = "red")+
  labs(title = "MSFT with 50 & 200 -Day Moving Average")

#JNJ
jnj_df_plot <- tibble(date = index(adj_close_xts), price = adj_close_xts$JNJ) %>%
  mutate(SMA_50 =SMA (price, n= 50),
         SMA_200 =SMA(price, n=200))
ggplot(jnj_df_plot, aes(x = date))+
  geom_line(aes(y = price), color = "black")+
  geom_line(aes(y = SMA_50), color ="blue")+
  geom_line(aes( y = SMA_200), color = "red")+
  labs(title = "JNJ with 50 & 200 -Day Moving Average")

#TSLA
tsla_df_plot <- tibble(date =index(adj_close_xts), price= adj_close_xts$TSLA) %>%
  mutate(SMA_50 = SMA (price , n=50),
         SMA_200 =SMA( price, n=200))
ggplot(tsla_df_plot, aes(x = date))+
  geom_line(aes(y = price), color = "black")+
  geom_line(aes(y = SMA_50), color ="blue")+
  geom_line(aes( y = SMA_200), color = "red")+
  labs(title = "TSLA with 50 & 200 -Day Moving Average")

#XOM
xom_df_plot <- tibble(date =index(adj_close_xts), price= adj_close_xts$XOM) %>%
  mutate(SMA_50 = SMA (price , n=50),
         SMA_200 =SMA( price, n=200))
ggplot(xom_df_plot, aes(x = date))+
  geom_line(aes(y = price), color = "black")+
  geom_line(aes(y = SMA_50), color ="blue")+
  geom_line(aes( y = SMA_200), color = "red")+
  labs(title = "XOM with 50 & 200 -Day Moving Average")