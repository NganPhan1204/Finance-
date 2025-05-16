library(readr)
library(dplyr)
library(ggplot2)
library(forecast)
library(Metrics)
library(lubridate)
library(scales)
##Load CSV data 
Starbuck <- read.csv("~/Downloads/Starbuck  - Sheet1.csv", stringsAsFactors=TRUE)
### Clean and Covert revenue to numeric
Starbuck <- Starbuck %>% 
  mutate(
    Total.Revenue = as.numeric(gsub(",", "", `Total.Revenue`)),
    US.Revenue = as.numeric(gsub(",", "", `US.Revenue`)),
    International.Revenue = as.numeric(gsub(",", "", `International.Revenue`)),
    Year = ymd(paste0(Year, "-09-30"))
  )
###Plot Historical Revenue Trend###
##Total Revenue###
ggplot(Starbuck, aes(x= Year, y = Total.Revenue))+
  geom_line(color= "steelblue", linewidth= 1.2) +
  geom_point(color= "darkgreen")+
  scale_y_continuous(labels=dollar_format(scale=1, suffix = "M"))+
  labs(title = "Starbucks Revenue (2015-2024)", x= "Year", y= "Revenue (USD)")
##US_Revenue##
ggplot(Starbuck, aes(x=Year, y= US.Revenue))+
  geom_line(color= "pink", linewidth = 1) +
  geom_point(color= "darkred")+
  scale_y_continuous(labels = dollar_format(scale=1,suffix ="M"))+
  labs(title = "US Revenue 2015- 2024", x= "Year", y= "Revenue USD")
##International Revenue###
ggplot(Starbuck, aes(x=Year, y= International.Revenue))+
  geom_line(color= "blue", linewidth = 1) +
  geom_point(color= "lightblue")+
  scale_y_continuous(labels = dollar_format(scale=1,suffix ="M"))+
  labs(title = "International Revenue 2015- 2024", x= "Year", y= "Revenue USD")

### Forecast Total Revenue with Linear Regression
model_lm <-lm(Total.Revenue ~ as.numeric(Year), data= Starbuck)
future_years <- data.frame(Year=ymd(paste0(2025:2029, "-01-01")))
future_years$Total.Revenue <- predict(model_lm, newdata= future_years)
Starbuck$Type <- "Actual"
future_years$Type <-"Forecast"
forecast_lm <-bind_rows(Starbuck, future_years)

ggplot(forecast_lm, aes(x= Year,y=Total.Revenue))+
  geom_line(color ="darkgreen", size =1.2)+
  geom_point(color ="forestgreen")+
  scale_y_continuous(labels = dollar_format(scale=1,suffix ="M"))+
  geom_vline(xintercept = ymd("2024-12-31"), linetype="dashed")+
  labs(title = "Linear Regression Revenue Forecast", y="Revenue", x= "Year")
## Forecast US revenue from 2025 to 2029 
model_us <-lm(US.Revenue ~ as.numeric(Year), data= Starbuck)
future_us <- data.frame(Year=ymd(paste0(2025:2029, "-01-01")))
future_us$US.Revenue <- predict(model_us, newdata= future_us)
Starbuck$Type <- "Actual"
future_us$Type <-"Forecast"
us_forecast <-bind_rows(Starbuck %>% select(Year, US.Revenue, Type), future_us)

ggplot(us_forecast, aes(x= Year,y=US.Revenue))+
  geom_line(color ="blue", size =1.2)+
  geom_point(color ="deepskyblue")+
  scale_y_continuous(labels = dollar_format(scale=1,suffix ="M"))+
  geom_vline(xintercept = ymd("2024-12-31"), linetype="dashed")+
  labs(title = "US Revenue Forecast from 2025 to 2029", y="Revenue", x="Year")

##Forecast International Revenue##

model_intl <- lm(International.Revenue ~ as.numeric(Year), data = Starbuck)
future_intl <- data.frame(Year=ymd(paste0(2025:2029, "-01-01")))
future_intl$International.Revenue <- predict(model_intl, newdata= future_intl)
Starbuck$Type <- "Actual"
future_intl$Type <-"Forecast"
intl_forecast <-bind_rows(Starbuck %>% select(Year, International.Revenue, Type), future_intl)

ggplot(intl_forecast, aes(x= Year,y=International.Revenue))+
  geom_line(color ="hotpink", size =1.2)+
  geom_point(color ="maroon")+
  scale_y_continuous(labels = dollar_format(scale=1,suffix ="M"))+
  geom_vline(xintercept = ymd("2024-12-31"), linetype="dashed")+
  labs(title = "International Revenue Forecast from 2025 to 2029", y="Revenue", x="Year")

### ETS Forecasting####
ts_total <- ts(Starbuck$Total.Revenue, start=2015, frequency = 1)
ts_us <- ts(Starbuck$US.Revenue, start =2015, frequency = 1)
ts_intl <- ts(Starbuck$International.Revenue, start=2015, frequency = 1)

ets_total <- ets(ts_total, model = "AAN")
ets_us <- ets(ts_us, model = "AAN")
ets_intl <- ets(ts_intl, model="AAN")

fc_total <- forecast(ets_total, h = 5)
fc_us <- forecast(ets_us, h = 5)
fc_intl <- forecast(ets_intl, h = 5)

summary(ets_total)
summary(ets_us)
summary(ets_intl)

#Combine data####
ets_forecast <-ymd(paste0(2025:2029,"-01-01"))
total_df <- data.frame(
  Year=c(Starbuck$Year, ets_forecast),
  Total.Revenue =c(Starbuck$Total.Revenue, fc_total$mean),
  Type=c(rep("Actual", length(Starbuck$Total.Revenue)), rep("Forecast",5))
)

us_df <-data.frame(
  Year = c(Starbuck$Year,ets_forecast),
  US.Revenue = c(Starbuck$US.Revenue, fc_us$mean),
  Type = c(rep("Actual", length(Starbuck$US.Revenue)), rep("Forecast", 5))
)

intl_df <-data.frame(
  Year = c(Starbuck$Year, ets_forecast),
  International.Revenue = c(Starbuck$International.Revenue, fc_intl$mean),
  Type = c(rep("Actual", length(Starbuck$International.Revenue)), rep("Forecast", 5))
)

# Total Revenue Plot
ggplot(total_df, aes(x = Year, y = Total.Revenue, color = Type)) +
  geom_line(size = 1.2) + geom_point() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "Total Revenue Forecast (ETS)", y = "Revenue", x = "Year")

# US Revenue Plot
ggplot(us_df, aes(x = Year, y = US.Revenue, color = Type)) +
  geom_line(size = 1.2) + geom_point() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "US Revenue Forecast (ETS)", y = "Revenue", x = "Year")

# International Revenue Plot
ggplot(intl_df, aes(x = Year, y = International.Revenue, color = Type)) +
  geom_line(size = 1.2) + geom_point() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "International Revenue Forecast (ETS)", y = "Revenue", x = "Year")

##Scenario Modeling For Total Revenue###
base_fc <- forecast(ets_total, h = 5)
base_revenue <- base_fc$mean
bull_multiplier <- 1.02
bear_multiplier <- 0.98

bull_revenue <- base_revenue * cumprod(rep(bull_multiplier, 5))
bear_revenue <- base_revenue * cumprod(rep(bear_multiplier, 5))

scenario_df <- data.frame(
  Year = years,
   Base = as.numeric(base_revenue),
  Bull = as.numeric(bull_revenue),
  Bear = as.numeric(bear_revenue)
  )

library(tidyr)
scenario_long <- pivot_longer(scenario_df, cols = -Year, names_to = "Scenario", values_to = "Revenue")
ggplot(scenario_long, aes(x = Year, y = Revenue, color = Scenario)) +
  geom_line(size = 1.2) +
  geom_point() +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "Revenue Forecast Scenarios", y = "Revenue", x = "Year")

##Ribbon Chart (Shaded Scenario Range)##
library(dplyr)
ribbon_df <- scenario_long %>%
  pivot_wider(names_from = Scenario, values_from = Revenue)
ggplot(ribbon_df, aes(x = Year)) +
  geom_ribbon(aes(ymin = Bear, ymax = Bull), fill = "lightblue", alpha = 0.4) +
  geom_line(aes(y = Base), color = "steelblue", size = 1.5) +
  geom_point(aes(y = Base), color = "steelblue") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "Revenue Forecast with Scenario Range",
       subtitle = "Bull and Bear Scenarios as Uncertainty Band",
       y = "Revenue", x = "Year")

##Bar chart 
ggplot(scenario_long, aes(x = factor(Year), y = Revenue, fill = Scenario)) +
geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "Revenue Forecast by Scenario",
       y = "Revenue", x = "Year") +
  theme_minimal()

##Faceted line chart
ggplot(scenario_long, aes(x = Year, y = Revenue)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "black") +
  facet_wrap(~ Scenario, ncol = 1) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "Revenue Forecast Scenarios (Faceted View)",
       y = "Revenue", x = "Year")

##Store analysis ##
Starbuck <- Starbuck %>% 
           mutate(
             Total.Store.Count = as.numeric(gsub(",", "", `Total.Store.Count`)),
             US.Company.Operated.Stores = as.numeric(gsub(",", "", `US.Company.Operated.Stores`)),
             US.Licensed.Stores= as.numeric(gsub(",", "", `US.Licensed.Stores`)),
             International.COS=as.numeric(gsub(",","",`International.COS`)),
             International.Licensed.Stores=as.numeric(gsub(",","",`International.Licensed.Stores`)),
             Gross.Profit.Margin = as.numeric(gsub("%", "", `Gross.Profit.Margin`)) / 100,
             Gross.Profit= as.numeric(gsub(",","",`Gross.Profit`)))

Starbuck <- Starbuck %>%
  mutate(
    Rev.per.Store = `Total.Revenue` / `Total.Store.Count`)       
ggplot(Starbuck, aes(x = Year)) +
  geom_line(aes(y = Rev.per.Store, color = "Total")) +
  scale_y_continuous(labels = scales::dollar_format(scale = 1, suffix = "M")) +
  labs(title = "Revenue per Store (2015–2024)",
       y = "Revenue per Store",
       x = "Year",
       color = "Store Type")

##Forecasting#####
ts_rev_per_store <-ts(Starbuck$Rev.per.Store, start = 2015,frequency = 1)
model_rev_store <-ets(ts_rev_per_store)
fc_rev_store <-forecast(model_rev_store, h=5)
autoplot(fc_rev_store)+
  labs(tittle ="Forecast: Revenue per Store", y="Revenue per Store", x="Year")