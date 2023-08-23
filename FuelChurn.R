### Options ----------- 

options(scipen=999)

### Load Packages ---------

library(odbc)
library(DBI)
library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(ggthemes)

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
SELECT lob_id AS LOBKey, Membership_num AS MemberID, LV, E.Date AS TrsDate, SUM(transline_amount) AS Sales FROM FACT_TRANSLINE T With (NoLock)

JOIN DIM_DATE E ON E.date_id = T.Date_id
JOIN DIM_MEMBER_HIST H WITH (NOLOCK) ON H.member_id = T.Member_id
JOIN auxiliarydb.dbo.MX_Members M ON M.MemberID = H.Membership_num
LEFT OUTER JOIN DIM_ITEM I ON I.item_id = T.item_id
WHERE E.DATE BETWEEN '7-31-2022' AND '8-1-2023' AND record_type = 1 AND Membership_num NOT IN (43, 76) AND record_type  = 1 AND transline_amount > 0
AND lob_id = 3
GROUP BY lob_id, membership_num, LV, E.date

")

SalesData <- dbFetch(SalesQuery)
SalesData$MemberID <- as.numeric(SalesData$MemberID)
SalesData$TrsDate <- ymd(SalesData$TrsDate)

### Survival Analysis

library(survival)
library(ggfortify)

### Survival Analysis - SINGLE LOB ONLY

SmallSales <- group_by(SalesData, LOBKey, MemberID) %>% summarize(Sales = sum(Sales)) %>% filter(Sales < 0)

Duration <- SalesData %>% 
  filter(MemberID != 43, MemberID != 76,
         TrsDate >= ymd("2021-07-30")) %>%
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

summary(surv, times = c(1, 7, 31,200, 300))

autoplot(surv) +  guides(color=F) +
  scale_fill_discrete(name="") + labs(title="Probability of Next Fuel Purchase", 
                                      subtitle="Given days elapsed July 2022-2023", y="Probability", x="Number of Days Elapsed") +
  
  scale_x_continuous(breaks=seq(0,720, by=30)) #+ scale_y_reverse()

