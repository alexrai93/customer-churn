library(survival)
library(ggfortify)

### Survival Analysis

## Filter out less loyal customers
BigSales <- filter(WeeklySales, PromoYear >= 2021) %>% 
  group_by(LOBKey, MemberID) %>% summarize(Trips = n(), Sales = mean(Sales)) %>% filter(LOBKey == 1)

mean(BigSales$Trips)
mean(BigSales$Sales)

Duration <- SalesData %>% 
  filter(MemberID != 43, MemberID != 76, MemberID > 10000) %>%
  group_by(MemberID, LOBKey, TrsDate) %>% summarize(Count = n()) %>%
  arrange(MemberID, TrsDate) %>% 
  group_by(MemberID) %>% 
  slice_head(n=2)

StartDate <- min(Duration$TrsDate)
EndDate <- max(Duration$TrsDate)

FinalDuration <- Duration %>% mutate(Durations=length(seq(min(TrsDate), max(TrsDate), by="day"))-1) %>% 
  mutate(Purchase=if_else(Durations==0, 0, 1)) %>% 
  mutate(Durations = if_else(Durations == 0,length(seq(StartDate, EndDate, by="day"))-1, Durations))

FinalDuration <- FinalDuration %>% 
  group_by(LOBKey, MemberID) %>% 
  summarize(Duration=max(Durations), Purchase=max(Purchase))

  surv <- survfit(Surv(time=FinalDuration$Duration, event=FinalDuration$Purchase) ~ FinalDuration$LOBKey)
  
  summary(surv, times = c(1,7, 31, 360))
  
  autoplot(surv) + 
  scale_fill_discrete(name="") + labs(title="Probability of Another Sale", 
                                      subtitle="Food/WSB Ecomm June 2023", y="Probability", x="Number of Days Elapsed") +
  scale_x_continuous(breaks=seq(0,720, by=30)) # + scale_y_reverse() 

### Geometric Distribution

# pgeom(1, 0.436)
# qgeom(0.5, 1-0.876)
# dgeom(7, 0.876)

#### Number of transactions in 1 year, value of sales
# SaleYear <- filter(SalesData, TrsDate >= ymd("2022-06-20"), MemberID >= 1000) %>% group_by(LOBKey, MemberID) %>% 
#   summarize(Sales = sum(Sales, na.rm=T), Trips = n())

## Data from second query in Ecomm
FoodYear <- filter(FinalDuration, LOBKey == 1)
WSBYear <- filter(FinalDuration, LOBKey == 2)

# Weekly Sales Food = 151, WSB = 178
# Weekly Trips Food = 1.076, WSB = 1.070

mean(FoodYear$Sales) #163
mean(FoodYear$Trips) #1.076

mean(WSBYear$Sales) #190
mean(WSBYear$Trips) #1.070

length(unique(FoodYear$MemberID)) #7877
length(unique(WSBYear$MemberID)) #3148

0.618 * 7877 * 6.33
