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
