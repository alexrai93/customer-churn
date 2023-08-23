### Options ----------- 

options(scipen=999)

### Load Packages ---------

library(odbc)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggthemes)
library(survival)
library(ggfortify)
library(partykit)
library(ggparty)
### Load SQL Connection ---------

con <- dbConnect(odbc(),
                 Driver = "SQL Server",
                 Server = "dwhsqlprod\\dwhsqlprod",
                 Database = "dwh_schema",
                 Trusted_Connection =" True"
)

### Query Database -------

SalesQuery <- dbSendQuery(con, 
                          "
SELECT Membership_num AS MemberID, E.Date AS TrsDate, sum(transline_amount) AS Sales
FROM FACT_TRANSLINE T WITH (NOLOCK)
JOIN DIM_DATE E ON E.date_id = T.Date_id
JOIN DIM_MEMBER_HIST H ON H.member_id = T.Member_id

WHERE finyear >= 2022 AND record_type = 1 AND Membership_num NOT IN (43, 76) AND LOB_ID = 1
GROUP BY Membership_num, E.Date
")

SalesData <- dbFetch(SalesQuery)
SalesData$MemberID <- as.numeric(SalesData$MemberID)
SalesData$TrsDate <- ymd(SalesData$TrsDate)

### Survival Analysis

## Filter out less loyal customers
BigSales <- filter(WeeklySales, PromoYear >= 2021) %>% 
  group_by(LOBKey, MemberID) %>% summarize(Trips = n(), Sales = mean(Sales)) %>% filter(LOBKey == 1)

mean(BigSales$Trips)
mean(BigSales$Sales)

Duration <- SalesData %>% 
  filter(MemberID != 43, MemberID != 76, MemberID > 10000) %>%
  group_by(MemberID, TrsDate) %>% summarize(Count = n()) %>%
  arrange(MemberID, TrsDate) %>% 
  group_by(MemberID) %>% 
  slice_head(n=2)

StartDate <- min(Duration$TrsDate)
EndDate <- max(Duration$TrsDate)

FinalDuration <- Duration %>% mutate(Durations=length(seq(min(TrsDate), max(TrsDate), by="day"))-1) %>% 
  mutate(Purchase=if_else(Durations==0, 0, 1)) %>% 
  mutate(Durations = if_else(Durations == 0,length(seq(StartDate, EndDate, by="day"))-1, Durations))

FinalDuration <- FinalDuration %>% 
  group_by(MemberID) %>% 
  summarize(Duration=max(Durations), Purchase=max(Purchase))

  surv <- survfit(Surv(time=FinalDuration$Duration, event=FinalDuration$Purchase) ~ 1)
  
  summary(surv, times = c(1,7, 31, 360))
  
  autoplot(surv) + 
  scale_fill_discrete(name="") + labs(title="Probability of Another Sale", 
                                      subtitle="Food/WSB Ecomm June 2023", y="Probability", x="Number of Days Elapsed") +
  scale_x_continuous(breaks=seq(0,720, by=30)) # + scale_y_reverse() 

### Tree Model on Final Duration
  
TargetCust <- read.csv("AppTargetingFood.csv")
TargetCust <- select(TargetCust, -1)
TargetCust <- inner_join(FinalDuration, TargetCust, by="MemberID")  
TargetVar <- TargetCust %>% select(-MemberID, -TotalQ, -TotalScore, -Cluster, -Freqs)
TargetVar <- TargetCust %>% select(Duration, Purchase, Cluster)
mod <- partykit::ctree(Surv(time=TargetVar$Duration, event=TargetVar$Purchase) ~ ., data=TargetVar)
plot(mod)
mod
unique(TargetVar$Cluster)
mod2 <- mod[[101]]
plot(mod2)
