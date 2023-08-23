
TestNoMem <- dbSendQuery(con,"
SELECT PromoYear, Membership_num, minor_cat_desc, SUM(Transline_Amount) AS Sales FROM FACT_TRANSLINE T WITH (NOLOCK)
JOIN DIM_MEMBER_HIST H WITH (NOLOCK) ON H.member_id = T.member_id
LEFT OUTER JOIN DIM_ITEM I WITH (NOLOCK) ON I.item_id = T.item_id
JOIN DIM_DATE E ON E.date_id = T.date_id
WHERE PromoYear IN (2022, 2023) AND promoweek BETWEEN 9 AND 37 AND record_type IN (1,4,5) AND lob_id = 2
AND T.item_id IN (select item_id from DIM_ITEM WHERE major_cat_desc LIKE '%WINE%' AND minor_cat_desc LIKE '%PRE 750%')
GROUP BY PromoYear, Membership_num, minor_cat_desc
")
TestNoMem <- dbFetch(TestNoMem)

TestNoMem$Membership_num <- as.numeric(TestNoMem$Membership_num)
TestNoMem <- filter(TestNoMem, TestNoMem$Membership_num %in% c(43,76))

TestNoMem <- group_by(TestNoMem, PromoYear, minor_cat_desc) %>% summarise(Sales=sum(Sales))
write.csv(TestNoMem, "NonMemPivot.csv")
