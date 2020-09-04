library(stats)
library(ggplot2)
library(ggfortify)
library(dplyr) 
library(tidyr)
library(mice)
library(VIM)

load("~/Desktop/R_hands_on/Search engine/sales_ah_1Y_[all_states].Rda")

#------------------------------------------ Prepare "raw_data_df"  ----------------------------------------------
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

#------------------------------------------ Getting Ready the Data (df) for K-Means  ----------------------------
q_time_df_aux = 
  raw_data_df %>% 
  select(SHIPTO_CUSTOMER_ID,
         SHIPTO_CUSTOMER_NAME, 
         GL_QRT,
         QUOTA_GROUPING,
         GROSS_SALES) %>% 
  group_by(SHIPTO_CUSTOMER_NAME,
           SHIPTO_CUSTOMER_ID,
           GL_QRT,
           QUOTA_GROUPING) %>% 
  summarize(GROSS_SALES = sum(GROSS_SALES)) %>% 
  ungroup() %>% 
  unique()
  
