library(quantmod)
library(xts)
library(zoo)
library(shiny)
library(TTR)
library(plotly)
library(dplyr)
getSymbols(c("JPM","BAC"), src = "yahoo", from = "2019-01-01", to = "2024-12-31")
#Extract only Adjusted Close##
jpm_monthly_close <- to.monthly(JPM[, "JPM.Adjusted"], indexAt = "lastof", OHLC = FALSE)
bac_monthly_close <- to.monthly(BAC[, "BAC.Adjusted"], indexAt = "lastof", OHLC = FALSE)
##Calculate SMAs
jpm_monthly <- jpm_monthly_close
jpm_monthly$SMA5 <- SMA(jpm_monthly_close[, 1], n = 5)
jpm_monthly$SMA12 <- SMA(jpm_monthly_close[, 1], n = 12)

bac_monthly<-bac_monthly_close
bac_monthly$SMA5 <- SMA(bac_monthly_close[, 1], n = 5)
bac_monthly$SMA12 <- SMA(bac_monthly_close[, 1], n = 12)

### Bank Fundamentals####
bank_fundamentals <- data.frame(
  Company = c(rep("JPM", 6), rep("BAC", 6)),
  Year = rep(2019:2024, times = 2),
  Net_Interest_Income = c(57.25, 54.56, 52.31, 66.71, 89.27, 92.58,
                          48.89, 43.36, 42.93, 52.46, 56.93, 56.06),
  Net_Income = c(36.23, 28.99, 48.10, 37.49, 49.26, 58.13,
                 27.43, 17.89, 31.98, 27.53, 26.52, 27.13),
  EPS = c(10.75, 8.89, 15.39, 12.10, 16.25, 19.79,
          2.77, 1.88, 3.60, 3.21, 3.10, 3.25),
  ROA = c(1.3, 0.9, 1.3, 1.0, 1.3, 1.5,
          1.1, 0.6, 1.0, 0.9, 0.8, 0.8),
  ROE = c(13.9, 10.4, 16.4, 12.8, 15.0, 17.4,
          10.4, 6.6, 11.8, 10.1, 9.1, 9.2),
  NIM = c(2.1, 1.6, 1.4, 1.8, 2.3, 2.3,
          2.0, 1.5, 1.3, 1.7, 1.8, 1.7),
  Total_Assets = c(2687.40, 3384.80, 3743.60, 3665.70, 3883.20, 4008.20,
                   2444.30, 2832.20, 3180.90, 3051.40, 3180.20, 3261.50),
  Total_Equity = c(261.3, 279.4, 294.1, 292.3, 327.9, 334.8,
                   264.8, 272.9, 270.1, 273.2, 291.6, 295.6)
)
###UI
ui <- fluidPage(
  titlePanel(" Financial Dashboard: JPMorgan vs. Bank of America"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("stock_choice", "Select Stock", choices = c("JPM", "BAC"))
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        selected = "Stock Price Chart",
        
        tabPanel("Line Charts (ROA, ROE, NIM)",
                 plotlyOutput("line_roa"),
                 plotlyOutput("line_roe"),
                 plotlyOutput("line_nim")),
        
        tabPanel("Bar Charts (Financials)",
                 plotlyOutput("bar_net_income"),
                 plotlyOutput("bar_net_interest"),
                 plotlyOutput("bar_eps"),
                 plotlyOutput("bar_assets"),
                 plotlyOutput("bar_equity")),
        
        tabPanel("Stock Price Chart",
                 plotlyOutput("stock_chart"))
      )
    )
  )
)
####Server
server <- function(input, output, session) {
  
  # ----- Line Charts -----
  output$line_roa <- renderPlotly({
    plot_ly(bank_fundamentals, 
            x = ~Year, 
            y = ~ROA, 
            color = ~Company,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "ROA (%) Trend", 
             yaxis = list(title = "ROA (%)"))
  })
  
  output$line_roe <- renderPlotly({
    plot_ly(bank_fundamentals, 
            x = ~Year, 
            y = ~ROE, 
            color = ~Company,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "ROE (%) Trend", 
             yaxis = list(title = "ROE (%)"))
  })
  
  output$line_nim <- renderPlotly({
    plot_ly(bank_fundamentals,
            x = ~Year, 
            y = ~NIM, 
            color = ~Company,
            type = 'scatter', mode = 'lines+markers') %>%
      layout(title = "Net Interest Margin (%) Trend", 
             yaxis = list(title = "NIM (%)"))
  })
  
  # ----- Bar Charts -----
  plot_metric_bar <- function(df, metric, y_label, is_eps = FALSE, is_billion = TRUE) {
    y_title <- if (is_eps) "Earnings per Share"
    else if (is_billion) paste0(y_label, " (Billions USD)") else y_label
    
    plot_ly(df, x = ~as.factor(Year), y = as.formula(paste0("~", metric)),
            color = ~Company, type = 'bar', barmode = "group") %>%
      layout(title = paste0(y_label, " by Year"),
             xaxis = list(title = "Year"), yaxis = list(title = y_title))
  }
  
  output$bar_net_income <- renderPlotly({
    plot_metric_bar(bank_fundamentals, "Net_Income", "Net Income")
  })
  
  output$bar_net_interest <- renderPlotly({
    plot_metric_bar(bank_fundamentals, "Net_Interest_Income", "Net Interest Income")
  })
  
  output$bar_eps <- renderPlotly({
    plot_metric_bar(bank_fundamentals, "EPS", "EPS", is_eps = TRUE, is_billion = FALSE)
  })
  
  output$bar_assets <- renderPlotly({
    plot_metric_bar(bank_fundamentals, "Total_Assets", "Total Assets")
  })
  
  output$bar_equity <- renderPlotly({
    plot_metric_bar(bank_fundamentals, "Total_Equity", "Total Equity")
  })
   
# ----- Stock Price Area Chart -----
  plot_stock_chart <- function(stock_xts, symbol) {
# Remove rows with NA values to avoid blank charts
    stock_xts <- na.omit(stock_xts)
    
    dates <- index(stock_xts)
    prices <- stock_xts[, 1]
    sma5 <- stock_xts$SMA5
    sma12 <- stock_xts$SMA12
    
    plot_ly(x = dates, type = 'scatter', mode = 'lines') %>%
      add_trace(y = as.numeric(prices), name = paste(symbol, "Price"), fill = 'tozeroy') %>%
      add_trace(y = as.numeric(sma5), name = "SMA5", mode = "lines") %>%
      add_trace(y = as.numeric(sma12), name = "SMA12", mode = "lines") %>%
      layout(title = paste(symbol, "Stock Price with Moving Averages"),
             yaxis = list(title = "Price (USD)"),
             xaxis = list(title = "Date"))
  }
  
  output$stock_chart <- renderPlotly({ req(input$stock_choice)
    if (input$stock_choice == "JPM") {
      plot_stock_chart(jpm_monthly, "JPM")
    } else {
      plot_stock_chart(bac_monthly, "BAC")
    }
  })
}
## Shiny APP
shinyApp(ui = ui, server = server)
