library(DBI)
library(RSQLite)
library(dplyr)
library(dbplyr)
library(readr)
library(scales)
library(readr)
con <- dbConnect(RSQLite::SQLite(), ":memory:")
df <- read.csv("~/Downloads/bank_transactions_data_2.csv")
# Copy data into SQL database
dbWriteTable(con, "transactions", df, overwrite = TRUE)
# Create dplyr reference to the SQL table
transactions_tbl <- tbl(con, "transactions")
# Preview
transactions_tbl %>% head()%>% collect()
###HIGH-VALUE & SHORT-DURATION TRANSACTIONS
hvsd <- dbGetQuery(con, "
            SELECT TransactionID,AccountID, TransactionAmount, TransactionDuration,
                      Location, Channel 
            FROM transactions
            Where TransactionAmount >1000
            And TransactionDuration < 30;" )

###Multiple Login Attempts###### 
Loginattempts <- dbGetQuery(con," 
              SELECT TransactionID, AccountID, LoginAttempts,Location, Channel
                  FROM transactions
                WHERE  LoginAttempts >=3")

####Fraud Transactions per Location####
fraund_by_location <- dbGetQuery(con, "
                      SELECT Location, COUNT(*) AS FraudCount
                FROM transactions
                  WHERE TransactionAmount >1000
                    AND TransactionDuration <20
                  Group by Location
                  Order by FraudCount DESC;")

##Account Balance Drop After transaction ####
balance_risk <-dbGetQuery(con, "
              SELECT TransactionID,AccountID, TransactionAmount, AccountBalance, (TransactionAmount / AccountBalance) AS RiskRatio
               FROM transactions
                  Where AccountBalance >0
                    AND (TransactionAmount / AccountBalance) >= 0.9 ;" )  

###Fraund suspicioss by channel ####
channel_suspicious <- dbGetQuery(con,"
                  SELECT TransactionID, TransactionAmount, Channel
                 FROM transactions
                    WHERE TransactionAmount > 1000 AND Channel = 'Online'
                   ORDER BY TransactionAmount DESC;")

##average amount by channel###
average <- dbGetQuery(con,"SELECT Channel, COUNT(*) AS Count, AVG(TransactionAmount) AS AvgAmount
                         FROM transactions
                       GROUP BY Channel;")

library(ggplot2)
ggplot(df, aes(x= TransactionDuration, y=TransactionAmount))+
  geom_point(alpha =0.5)+
  scale_y_log10()+
  theme_minimal()+
  labs(title ="Transaction Amount vs Duration", x="Duration (sec)", y="Amount ($)")

