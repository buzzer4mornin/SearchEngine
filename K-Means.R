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

q_time_map_table = 
  q_time_df_aux %>% 
  select(SHIPTO_CUSTOMER_NAME, 
         SHIPTO_CUSTOMER_ID) %>% 
  unique() %>% 
  crossing(q_time_df_aux %>% 
             select(GL_QRT) %>% 
             unique()) %>% 
  left_join(q_time_df_aux, 
            by = c("SHIPTO_CUSTOMER_NAME", 
                   "SHIPTO_CUSTOMER_ID",
                   "GL_QRT")) %>% 
  left_join(q_time_df_aux %>% 
              select(QUOTA_GROUPING) %>% 
              unique() %>% 
              crossing(q_time_df_aux %>% 
                         select(GL_QRT) %>% 
                         unique()) %>% 
              mutate(GROUP_TIME_FRAME = paste0(QUOTA_GROUPING, "_", GL_QRT)) %>% 
              unique(), 
            by = c("QUOTA_GROUPING", "GL_QRT") )

  
df = 
  q_time_map_table %>% 
  select(SHIPTO_CUSTOMER_ID,
         SHIPTO_CUSTOMER_NAME,
         GROUP_TIME_FRAME,
         GROSS_SALES) %>% 
  unique() %>% 
  spread(key = "GROUP_TIME_FRAME", 
         value = GROSS_SALES) %>%
  select(-"<NA>")


# -------------------------  START
 

#df[is.na(df)] <- 0 # THIS or Following

SHIPTO_CUSTOMER_ID <- df$SHIPTO_CUSTOMER_ID
df<- df %>% select(-SHIPTO_CUSTOMER_ID, -SHIPTO_CUSTOMER_NAME)
df <- as.matrix(df)

#best seed is 2
#for (s in c(1:10)){
#print(s)
set.seed(2)
RES <- dineof(df, n.max = NULL, ref.pos = NULL, delta.rms = 1e-05,method = "svds")
#}

df <- RES$Xa
df <- as.data.frame(df)
df <- cbind(SHIPTO_CUSTOMER_ID, df)

# ------------------------- END

df <- aggregate(cbind(df$Canine_Vaccines_Q1,df$Canine_Vaccines_Q2,df$Canine_Vaccines_Q3,df$Canine_Vaccines_Q4,
                      df$Endocrine_Products_Q1,df$Endocrine_Products_Q2,df$Endocrine_Products_Q3,df$Endocrine_Products_Q4,
                      df$Essentials_Q1,df$Essentials_Q2,df$Essentials_Q3,df$Essentials_Q4,
                      df$Feline_Vaccines_Q1,df$Feline_Vaccines_Q2,df$Feline_Vaccines_Q3,df$Feline_Vaccines_Q4,
                      df$Home_Again_Q1, df$Home_Again_Q2, df$Home_Again_Q3, df$Home_Again_Q4,
                      df$Ophthalmics_Antibiotics_Q1,df$Ophthalmics_Antibiotics_Q2,df$Ophthalmics_Antibiotics_Q3,df$Ophthalmics_Antibiotics_Q4,
                      df$Otics_Q1,df$Otics_Q2,df$Otics_Q3,df$Otics_Q4,
                      df$Parasiticides_Q1,df$Parasiticides_Q2,df$Parasiticides_Q3,df$Parasiticides_Q4,
                      df$Rabies_Q1,df$Rabies_Q2,df$Rabies_Q3,df$Rabies_Q4,
                      df$Sure_Petcare_Q1,df$Sure_Petcare_Q2,df$Sure_Petcare_Q3,df$Sure_Petcare_Q4), by=list(SHIPTO_CUSTOMER_ID=df$SHIPTO_CUSTOMER_ID), FUN=sum)
