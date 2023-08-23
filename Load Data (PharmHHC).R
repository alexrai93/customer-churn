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
SELECT Membership_num AS MemberID, E.Date AS TrsDate, Sales
FROM (

	SELECT 6 AS Lob_id, store_id, member_id, transaction_code, -1 AS POSDept_id, -1 AS Item_id, 1 AS record_type, sale_amt AS Sales, date_id FROM FACT_PH_TRANSLINE P With (NoLock) 
	UNION
	SELECT lob_id, store_id, member_id, transaction_code, POSDept_id, T.item_id, record_type, transline_amount AS Sales, date_id FROM FACT_TRANSLINE T With (NoLock)
	LEFT JOIN DIM_ITEM I ON I.item_id = T.item_id
	WHERE record_type  = 1

) T
JOIN DIM_DATE E ON E.date_id = T.Date_id
JOIN DIM_MEMBER_HIST H ON H.member_id = T.Member_id

WHERE finyear >= 2022 AND record_type = 1 AND Membership_num NOT IN (43, 76)

")

SalesData <- dbFetch(SalesQuery)
SalesData$MemberID <- as.numeric(SalesData$MemberID)
SalesData$TrsDate <- ymd(SalesData$TrsDate)
