  library(stats)
  library(dplyr) 
  library(tidyr)
  library(mice)
  library(tidyverse)
  library(modelr)     
  library(broom) 
  library(car)
  library(naniar)
  library(janitor)
  library(onewaytests)
  library(sinkr)

load("~/Desktop/R_hands_on/Search engine/sales_ah_1Y_[all_states].Rda")

#------------------------------------------  Prepare "raw_data_df"  ---------------------------------------------
raw_data_df =
  sales_ah %>%
  select(PRODUCT,
         SHIPTO_CUSTOMER_ID,
         SHIPTO_CUSTOMER_NAME,
         QUOTA_GROUPING,
         PRODUCT_GROUPING,
         GROSS_SALES,
         TIER,
         PRODUCT_SPECIES,
         REGION_CODE,
         STATE,
         GL_PERIOD,
         INVOICE_DATE,
         SALES_REP,
         PRIMARY_COT,
         CORP_ACCOUNT_NM,
         GL_MONTH,
         GL_QRT,
         ZIP) %>%
  filter(PRODUCT_GROUPING != 0) %>%
  mutate(QUOTA_GROUPING = PRODUCT_GROUPING) %>%
  mutate(QUOTA_GROUPING = gsub(pattern = " ", replacement = "_", QUOTA_GROUPING),
         QUOTA_GROUPING = gsub(pattern = "-", replacement = "_", QUOTA_GROUPING),
         QUOTA_GROUPING = gsub(pattern = "_&_", replacement = "_", QUOTA_GROUPING)) %>%
  mutate(SHIPTO_CUSTOMER_NAME = gsub("THE", "", SHIPTO_CUSTOMER_NAME)) %>%
  mutate(SHIPTO_CUSTOMER_NAME = gsub("  ", " ", SHIPTO_CUSTOMER_NAME)) %>%
  mutate(SHIPTO_CUSTOMER_NAME = trimws(SHIPTO_CUSTOMER_NAME))


#------------------------------------------- Prepare pre-datas for LeveneTest & WelchTest  ----------------------
table <- raw_data_df %>% select(SHIPTO_CUSTOMER_ID, GL_MONTH, GL_QRT,QUOTA_GROUPING, GROSS_SALES)
table$times <- 1
table$GL_MONTH <- str_remove(table$GL_MONTH, "^0+")


#following "table1_month" and "table2_month" dataframes are essentials and will be used for LeveneTest & WelchTest

table1_month = 
  table %>% 
  select(SHIPTO_CUSTOMER_ID,
         GL_MONTH,
         GL_QRT,
         QUOTA_GROUPING,
         GROSS_SALES,times) %>% 
  group_by(SHIPTO_CUSTOMER_ID,
           GL_MONTH,
           GL_QRT,
           QUOTA_GROUPING) %>% 
  summarize(GROSS_SALES = sum(GROSS_SALES), times = sum(times)) %>% 
  ungroup() %>% 
  unique()

table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

table2_month = 
  table1_month %>% 
  select(SHIPTO_CUSTOMER_ID) %>% 
  unique() %>% 
  crossing(table1_month %>% 
             select(GL_MONTH,GL_QRT) %>% 
             unique()) %>% 
  left_join(table1_month, 
            by = c(
                   "SHIPTO_CUSTOMER_ID",
                   "GL_MONTH","GL_QRT")) %>% 
  left_join(table1_month %>% 
              select(QUOTA_GROUPING) %>% 
              unique() %>% 
              crossing(table1_month %>% 
                         select(GL_MONTH) %>% 
                         unique()) %>% 
              mutate(GROUP_TIME_FRAME = paste0(QUOTA_GROUPING, "_", GL_MONTH)) %>% 
              unique(), 
            by = c("QUOTA_GROUPING", "GL_MONTH") ) %>%
  left_join(table1_month %>% 
              select(QUOTA_GROUPING) %>% 
              unique() %>% 
              crossing(table1_month %>% 
                         select(GL_QRT) %>% 
                         unique()) %>% 
              mutate(GROUP_TIME_FRAME = paste0(QUOTA_GROUPING, "_", GL_QRT)) %>% 
              unique(), 
            by = c("QUOTA_GROUPING", "GL_QRT") )

table2_month <-  table2_month %>% rename(GROUP_TIME_FRAME = GROUP_TIME_FRAME.x)
table2_month <-  table2_month %>% rename(GROUP_TIME_FRAME_QRT = GROUP_TIME_FRAME.y)


# "customers_sales"  will be used later for join operation 
customers_sales = 
  table1_month %>% 
  select(SHIPTO_CUSTOMER_ID,
         GROSS_SALES) %>% 
  group_by(SHIPTO_CUSTOMER_ID
  ) %>% 
  summarize(GROSS_SALES = sum(GROSS_SALES)) %>% 
  ungroup() %>% 
  unique()


# Gathering All Product Names
product_names <- unique(raw_data_df$QUOTA_GROUPING)
