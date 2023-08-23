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
WHERE E.DATE BETWEEN '10-31-2021' AND '11-1-2022' AND record_type = 1 AND Membership_num NOT IN (43, 76) AND record_type  = 1 AND Cashier > 999 
AND lob_id IN (1,2)
GROUP BY lob_id, membership_num, LV, E.date

")

SalesData <- dbFetch(SalesQuery)
SalesData$MemberID <- as.numeric(SalesData$MemberID)
SalesData$TrsDate <- ymd(SalesData$TrsDate)

## First time users

FirstUsers <- dbSendQuery(
  con,
  "
SELECT membership_num AS MemberID, MIN(Transline_Datetime) AS FirstUser FROM FACT_TRANSLINE T WITH (NOLOCK)
JOIN DIM_MEMBER_HIST H ON H.member_id = T.member_id
JOIN auxiliarydb.dbo.MX_MEMBERS M ON M.memberID = H.membership_num
WHERE record_type = 1 AND cashier > 999 AND isEmployee = 'N' AND LOB_ID = 1
GROUP BY membership_num
HAVING MIN(Transline_Datetime) BETWEEN '10-31-2021' AND '11-1-2022'
"
)
FirstUsers <- dbFetch(FirstUsers)
FirstUsers$MemberID <- as.numeric(FirstUsers$MemberID)
SalesData <- filter(SalesData, MemberID %in% FirstUsers$MemberID)

### Survival Analysis

library(survival)
library(ggfortify)

WeeklySales <- dbSendQuery(con, 
                          "
SELECT lob_id AS LOBKey, Membership_num AS MemberID, E.promoyear AS PromoYear, E.promoweek AS PromoWeek, transaction_code AS TrsCode,
SUM(transline_amount) AS Sales FROM FACT_TRANSLINE T With (NoLock)

JOIN DIM_DATE E ON E.date_id = T.Date_id
JOIN DIM_MEMBER_HIST H WITH (NOLOCK) ON H.member_id = T.Member_id

WHERE E.Date BETWEEN '10-1-2021' AND '11-1-2022' AND record_type = 1 AND Membership_num NOT IN (43, 76) AND record_type IN (1,4,5) AND Cashier > 999 AND lob_id IN (1,2)
GROUP BY lob_id, membership_num, E.PromoYear, E.PromoWeek, transaction_code

")

WeeklySales <- dbFetch(WeeklySales)
WeeklySales$MemberID <- as.numeric(WeeklySales$MemberID)

WeeklySales <- filter(WeeklySales, MemberID %in% FirstUsers$MemberID)

WeeklyTrips <- group_by(WeeklySales, LOBKey, MemberID, PromoYear) %>% 
  summarize(Sales = sum(Sales), Trips = n())

mean(WeeklyTrips[WeeklyTrips$LOBKey==2 & WeeklyTrips$PromoYear == 2022,]$Trips) #2.74 WSB, 5.97 Food

mean(WeeklySales[WeeklyTrips$LOBKey==1 & WeeklyTrips$PromoYear == 2022,]$Sales) #157.25 WSB, 154.26 Food

mean(filter(WeeklyTrips, LOBKey == 2, PromoYear == 2022)$Sales)

length(unique(filter(WeeklyTrips, LOBKey == 1, PromoYear == 2022)$MemberID))


### Survival Analysis - SINGLE LOB ONLY

SmallSales <- group_by(SalesData, LOBKey, MemberID) %>% summarize(Sales = sum(Sales)) %>% filter(Sales > 0)

Duration <- SalesData %>% 
  filter(MemberID != 43, MemberID != 76,
         TrsDate >= ymd("2022-05-17")) %>%
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

summary(surv, times = c(1, 7, 31,300))

autoplot(surv) + geom_hline(yintercept=0.25, color="dodgerblue4", linetype=2) + guides(color=F) +
  scale_fill_discrete(name="") + labs(title="Probability of Next Purchase by Line of Business", 
                                      subtitle="All Lines of Business", y="Probability", x="Number of Days Elapsed") +
  annotate(geom="text", x=315, y=0.22, col="dodgerblue4", label="75% Repurchase Probability") + 
  scale_x_continuous(breaks=seq(0,720, by=30)) + scale_y_reverse()

