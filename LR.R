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

#------------------------------------------- LeveneTest / Monthyly Purchase $$$ - on Normal Data ---------------- 

# GL_MONTH - from numeric to factor
table1_month$GL_MONTH <- as.factor(table1_month$GL_MONTH)

for (val in product_names)
{
df = 
  table1_month %>%
  filter(QUOTA_GROUPING == val) %>%
  group_by(SHIPTO_CUSTOMER_ID, GL_MONTH) %>%
  summarize(GROSS_SALES = sum(GROSS_SALES, na.rm = T)) %>%
  filter(GROSS_SALES != -Inf)

df$GL_MONTH <- as.factor(df$GL_MONTH)

lTest <- leveneTest(GROSS_SALES ~ GL_MONTH, df , center=mean)
print(paste(val,"--",lTest[1,3]))
}

'confidence level : 0.05
 Products having indifferent variances: NULL'

#------------------------------------------- LeveneTest / Monthyly Purchase $$$ - on Log Data -------------------

# GL_MONTH - from numeric to factor
table1_month$GL_MONTH <- as.factor(table1_month$GL_MONTH)

for (val in product_names)
{
df = 
  raw_data_df %>%
  select(SHIPTO_CUSTOMER_ID,
         QUOTA_GROUPING,
         GROSS_SALES,
         GL_MONTH) %>%
  filter(QUOTA_GROUPING == val) %>%
  group_by(SHIPTO_CUSTOMER_ID, GL_MONTH) %>%
  summarize(GROSS_SALES = log10(sum(GROSS_SALES, na.rm = T))) %>%
  filter(GROSS_SALES != -Inf)

df$GL_MONTH <- as.factor(df$GL_MONTH)

lTest <- leveneTest(GROSS_SALES ~ GL_MONTH, df , center=mean)
print(paste(val,"--",lTest[1,3]))
}

'confidence level: 0.05
 Products having indifferent variances: Sure_Petcare'


#-------------------------------- WelchTest / Monthyly Purchase $$$  - 12 month ----------------------=----------

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

for (val in product_names)
{
  df = 
    table1_month %>%
    filter(QUOTA_GROUPING == val) 
  
  welch_test <- welch.test(GROSS_SALES ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
  print(paste(val,"--",welch_test[3]))
}

'confidence level: 0.05
 Products having indifferent variances: Sure_Petcare'

#-------------------------------- WelchTest / Monthyly Purchase $$$  - first 6 months ---------------------------

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

for (val in product_names)
{
  df = 
    table1_month %>%
    filter(QUOTA_GROUPING == val, GL_MONTH < 7) 
  
  welch_test <- welch.test(GROSS_SALES ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
  print(paste(val,"--",welch_test[3]))
}


'confidence level: 0.05
 Products having indifferent variances: Sure_Petcare'

#-------------------------------- WelchTest / Monthyly Purchase $$$  - second 6 months --------------------------

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

for (val in product_names)
{
  df = 
    table1_month %>%
    filter(QUOTA_GROUPING == val, GL_MONTH > 6) 
  
  welch_test <- welch.test(GROSS_SALES ~ GL_MONTH, df,na.rm = TRUE, verbose=FALSE)
  print(paste(val,"--",welch_test[3]))
}

'confidence level: 0.05
 Products having similar means: Sure_Petcare / Ophthalmics_Antibiotics / Essentials'

#-------------------------------- WelchTest / Monthyly Purchase $$$  - Quarterly --------------------------------

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

# printing Products having p-value > 0.05 (confidence level)
for(x in c(0,3,6,9)){
  print(switch(x+1,"<< January - March >>",".",".",  "<< April - June >>",".",".",
                   "<< July - September >>",".",".", "<< October - December >>"))
  for (val in product_names)
  {
    df = 
      table1_month %>%
      filter(QUOTA_GROUPING == val,GL_MONTH > x ,GL_MONTH < x + 4) 
    
    welch_test <- welch.test(GROSS_SALES ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
    if(welch_test[3] > 0.05){
      print(paste(val))
    }
  }
}

'confidence level: 0.05
 Products having similar means across months 
 January - March    : Sure_Petcare
 April - June       : Endocrine_Products, Essentials, Sure_Petcare
 July - September   : Essentials, Ophthalmics_Antibiotics, Sure_Petcare
 October - December : Essentials, Ophthalmics_Antibiotics, Sure_Petcare '

#-------------------------------- WelchTest / Monthly Times - 12 month  -----------------------------------------  

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

for (val in product_names)
{
  df = 
    table1_month %>%
    filter(QUOTA_GROUPING == val) 
  
  welch_test_times <- welch.test(times ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
  print(paste(val,"--",welch_test_times[3]))
}

'confidence level: 0.05
 Products having indifferent variances: Sure_Petcare'

#-------------------------------- WelchTest / Monthly Times - first 6 months ------------------------------------  

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)
