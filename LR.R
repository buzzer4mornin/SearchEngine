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

for (val in product_names)
{
  df = 
    table1_month %>%
    filter(QUOTA_GROUPING == val, GL_MONTH < 7) 
  
  welch_test_times <- welch.test(times ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
  print(paste(val,"--",welch_test_times[3]))
}

'confidence level: 0.05
 Products having indifferent variances: Sure_Petcare'

#-------------------------------- WelchTest / Monthly Times - second 6 months -----------------------------------  

# GL_MONTH - from factor to numeric
table1_month$GL_MONTH <- as.numeric(table1_month$GL_MONTH)

for (val in product_names)
{
  df = 
    table1_month %>%
    filter(QUOTA_GROUPING == val, GL_MONTH > 6) 
  
  welch_test_times <- welch.test(times ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
  print(paste(val,"--",welch_test_times[3]))
}

'confidence level: 0.05
Products having indifferent variances: Sure_Petcare / Ophthalmics_Antibiotics / Essentials'

#-------------------------------- WelchTest / Monthyly Times   - Quarterly --------------------------------------

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
    
    welch_test <- welch.test(times ~ GL_MONTH, df,na.rm = TRUE,verbose=FALSE)
    if(welch_test[3] > 0.05){
      print(paste(val))
    }
  }
}

'confidence level: 0.05
 Products having similar means across months 
 January - March    : Rabies, Sure_Petcare
 April - June       : Home_Again, Essentials, Ophthalmics_Antibiotics, Sure_Petcare
 July - September   : Otics, Essentials, Ophthalmics_Antibiotics, Sure_Petcare
 October - December : Rabies, Essentialsl, Ophthalmics_Antibiotics, Sure_Petcare '

#-------------------------------- Welch's t-test - 2 groups comparison ------------------------------------------

# GL_MONTH - from factor to numeric
table2_month$GL_MONTH <- as.numeric(table2_month$GL_MONTH)

Monthly_Purchase = 
  table2_month %>% 
  select(SHIPTO_CUSTOMER_ID,
         GROUP_TIME_FRAME,
         GROSS_SALES) %>% 
  unique() %>% 
  spread(key = "GROUP_TIME_FRAME", 
         value = GROSS_SALES) %>%
  select(-"<NA>") 

Monthly_Purchase$SHIPTO_CUSTOMER_NAME <- NULL
Monthly_Purchase[is.na(Monthly_Purchase)] <- 0
Monthly_Purchase <- aggregate(cbind(Monthly_Purchase[,c(2:121)]), 
                              by=list(SHIPTO_CUSTOMER_ID=Monthly_Purchase$SHIPTO_CUSTOMER_ID), FUN=sum)

Monthly_Purchase <- Monthly_Purchase %>% replace_with_na_all(condition = ~.x == 0)

months <- c(1:11)

# printing matching couples
for (product in product_names)
{
  for(i in months){
    product_a = paste(product, i, sep="_")
    #print(product_a)
    while(i <= 11){
      product_b = paste(product, i+1, sep="_")
      #print(product_b)
      col1 <-  Monthly_Purchase %>% select(product_a)
      col2 <-  Monthly_Purchase %>% select(product_b)
      result <- t.test(col1,col2, alternative="two.sided", var.equal=FALSE)
      if(result[3] >= 0.05){
      print(paste(product_a,"vs",product_b,"--",result[3]))
        }
      i = i+1
    }
    }
}


#------------------------------------------- LR Monthly - Before welch test  ----------------------------------------------

'------ Linear Regression on Monthly_Times dataframe. (0 values filled instead of NAs) ------'

Monthly_Times = 
  table2_month %>% 
  select(SHIPTO_CUSTOMER_ID,
         GROUP_TIME_FRAME,
         times) %>% 
  unique() %>% 
  spread(key = "GROUP_TIME_FRAME", 
         value = times)

Monthly_Times[is.na(Monthly_Times)] <- 0
Monthly_Times <- aggregate(cbind(Monthly_Times[,c(2:121)]), 
                           by=list(SHIPTO_CUSTOMER_ID=Monthly_Times$SHIPTO_CUSTOMER_ID), FUN=sum)


Monthly_Times <- inner_join(Monthly_Times,customers_sales)
Monthly_Times$SHIPTO_CUSTOMER_ID <- NULL
Monthly_Times[1:121]<- lapply(Monthly_Times[1:121], as.numeric)


model1 <- lm(GROSS_SALES ~ .,
             data=Monthly_Times)
summary(model1)
#Multiple R-squared:  0.6104,	Adjusted R-squared:  0.6074 

#------------------------------------------- LR Monthly - After welch test (adjustment according to Monthyly Purchase $$$  - Quarterly) --------------------

#First run "LR before welch test" section to get Monthly_Times dataframe ready...
'Products having similar means across months 
 January - March    : Sure_Petcare
 April - June       : Endocrine_Products, Essentials, Sure_Petcare
 July - September   : Essentials, Ophthalmics_Antibiotics, Sure_Petcare
 October - December : Essentials, Ophthalmics_Antibiotics, Sure_Petcare '

Monthly_Times <- mutate(Monthly_Times ,Sure_Petcare_mean = rowMeans(select(Monthly_Times,Sure_Petcare_1,Sure_Petcare_2,
                                                                           Sure_Petcare_3,Sure_Petcare_4,
                                                                           Sure_Petcare_5,Sure_Petcare_6,
                                                                           Sure_Petcare_7,Sure_Petcare_8,
                                                                           Sure_Petcare_9,Sure_Petcare_10,
                                                                           Sure_Petcare_11,Sure_Petcare_12),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Endocrine_Products_mean = rowMeans(select(Monthly_Times,Endocrine_Products_4,Endocrine_Products_5,
                                                                                 Endocrine_Products_6),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Essentials_mean = rowMeans(select(Monthly_Times,Essentials_4,Essentials_5,
                                                                         Essentials_6,Essentials_7,
                                                                         Essentials_8,Essentials_9,
                                                                         Essentials_10,Essentials_11,
                                                                         Essentials_12),na.rm = TRUE))


Monthly_Times <- mutate(Monthly_Times ,Ophthalmics_Antibiotics_mean = rowMeans(select(Monthly_Times,Ophthalmics_Antibiotics_7,
                                                                                      Ophthalmics_Antibiotics_8,Ophthalmics_Antibiotics_9,
                                                                                      Ophthalmics_Antibiotics_10,Ophthalmics_Antibiotics_11,
                                                                                      Ophthalmics_Antibiotics_12),na.rm = TRUE))


Monthly_Times <- select(Monthly_Times,-starts_with("Sure_Petcare"))
Monthly_Times <- Monthly_Times %>% select(-Endocrine_Products_4,-Endocrine_Products_5,-Endocrine_Products_6)
Monthly_Times <- Monthly_Times %>% select(-Essentials_4,-Essentials_5,
                                          -Essentials_6,-Essentials_7,
                                          -Essentials_8,-Essentials_9,
                                          -Essentials_10,-Essentials_11,
                                          -Essentials_12)

Monthly_Times <- Monthly_Times %>% select(-Ophthalmics_Antibiotics_7,
                                          -Ophthalmics_Antibiotics_8,-Ophthalmics_Antibiotics_9,
                                          -Ophthalmics_Antibiotics_10,-Ophthalmics_Antibiotics_11,
                                          -Ophthalmics_Antibiotics_12)

model2 <- lm(GROSS_SALES ~ .,
             data=Monthly_Times)
summary(model2)
#Multiple R-squared:  0.6073,	Adjusted R-squared:  0.605 

#------------------------------------------- LR Monthly - After welch test (adjustment according to Monthyly Times   - Quarterly) --------------------------

#First run "LR before welch test" section to get Monthly_Times dataframe ready...
'Products having similar means across months 
 January - March    : Rabies, Sure_Petcare
 April - June       : Home_Again, Essentials, Ophthalmics_Antibiotics, Sure_Petcare
 July - September   : Otics, Essentials, Ophthalmics_Antibiotics, Sure_Petcare
 October - December : Rabies, Essentialsl, Ophthalmics_Antibiotics, Sure_Petcare '

Monthly_Times <- mutate(Monthly_Times ,Sure_Petcare_mean = rowMeans(select(Monthly_Times,Sure_Petcare_1,Sure_Petcare_2,
                                                                           Sure_Petcare_3,Sure_Petcare_4,
                                                                           Sure_Petcare_5,Sure_Petcare_6,
                                                                           Sure_Petcare_7,Sure_Petcare_8,
                                                                           Sure_Petcare_9,Sure_Petcare_10,
                                                                           Sure_Petcare_11,Sure_Petcare_12),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Rabies_mean = rowMeans(select(Monthly_Times,Rabies_1,Rabies_2,
                                                                     Rabies_3,Rabies_10,Rabies_11,Rabies_12),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Home_Again_mean = rowMeans(select(Monthly_Times,Home_Again_4,Home_Again_5,
                                                                         Home_Again_6),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Otics_mean = rowMeans(select(Monthly_Times,Otics_7,Otics_8,
                                                                    Otics_9),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Essentials_mean = rowMeans(select(Monthly_Times,Essentials_4,Essentials_5,
                                                                         Essentials_6,Essentials_7,
                                                                         Essentials_8,Essentials_9,
                                                                         Essentials_10,Essentials_11,
                                                                         Essentials_12),na.rm = TRUE))

Monthly_Times <- mutate(Monthly_Times ,Ophthalmics_Antibiotics_mean = rowMeans(select(Monthly_Times,Ophthalmics_Antibiotics_4,Ophthalmics_Antibiotics_5,
                                                                                      Ophthalmics_Antibiotics_6,Ophthalmics_Antibiotics_7,
                                                                                      Ophthalmics_Antibiotics_8,Ophthalmics_Antibiotics_9,
                                                                                      Ophthalmics_Antibiotics_10,Ophthalmics_Antibiotics_11,
                                                                                      Ophthalmics_Antibiotics_12),na.rm = TRUE))

Monthly_Times <- select(Monthly_Times,-starts_with("Sure_Petcare"))
Monthly_Times <- Monthly_Times %>% select(-Rabies_1,-Rabies_2,-Rabies_3,-Rabies_10,-Rabies_11,-Rabies_12)
Monthly_Times <- Monthly_Times %>% select(-Home_Again_4,-Home_Again_5,-Home_Again_6)
Monthly_Times <- Monthly_Times %>% select(-Otics_7,-Otics_8,-Otics_9)
Monthly_Times <- Monthly_Times %>% select(-Essentials_4,-Essentials_5,
                                          -Essentials_6,-Essentials_7,
                                          -Essentials_8,-Essentials_9,
                                          -Essentials_10,-Essentials_11,
                                          -Essentials_12)

Monthly_Times <- Monthly_Times %>% select(-Ophthalmics_Antibiotics_4,-Ophthalmics_Antibiotics_5,
                                          -Ophthalmics_Antibiotics_6,-Ophthalmics_Antibiotics_7,
                                          -Ophthalmics_Antibiotics_8,-Ophthalmics_Antibiotics_9,
                                          -Ophthalmics_Antibiotics_10,-Ophthalmics_Antibiotics_11,
                                          -Ophthalmics_Antibiotics_12)

model3 <- lm(GROSS_SALES ~ .,
             data=Monthly_Times)
summary(model3)
#Multiple R-squared:  0.604,	Adjusted R-squared:  0.6018

#------------------------------------------- LR Monthly - Stepwise Regression after WelchTest ----------------------------------
'Stepwise Regression is run on linear regression model of previous section: """LR Monthly - After welch test (adjustment according to Monthyly Times   - Quarterly)"""'

step(model3, direction = "both")

model4 <- lm(formula = GROSS_SALES ~ Canine_Vaccines_1 + Canine_Vaccines_10 + 
               Canine_Vaccines_11 + Canine_Vaccines_12 + Canine_Vaccines_2 + 
               Canine_Vaccines_3 + Canine_Vaccines_4 + Canine_Vaccines_5 + 
               Canine_Vaccines_6 + Canine_Vaccines_7 + Canine_Vaccines_8 + 
               Canine_Vaccines_9 + Endocrine_Products_1 + Endocrine_Products_10 + 
               Endocrine_Products_12 + Endocrine_Products_3 + Endocrine_Products_6 + 
               Endocrine_Products_7 + Endocrine_Products_8 + Essentials_1 + 
               Essentials_2 + Feline_Vaccines_1 + Feline_Vaccines_10 + Feline_Vaccines_11 + 
               Feline_Vaccines_12 + Feline_Vaccines_2 + Feline_Vaccines_3 + 
               Feline_Vaccines_4 + Feline_Vaccines_5 + Feline_Vaccines_7 + 
               Feline_Vaccines_8 + Feline_Vaccines_9 + Home_Again_1 + Home_Again_10 + 
               Home_Again_11 + Home_Again_12 + Home_Again_2 + Home_Again_9 + 
               Ophthalmics_Antibiotics_1 + Otics_1 + Otics_10 + Otics_11 + 
               Otics_4 + Otics_5 + Otics_6 + Parasiticides_1 + Parasiticides_10 + 
               Parasiticides_11 + Parasiticides_12 + Parasiticides_2 + Parasiticides_3 + 
               Parasiticides_4 + Parasiticides_5 + Parasiticides_6 + Parasiticides_7 + 
               Parasiticides_8 + Parasiticides_9 + Rabies_5 + Rabies_6 + 
               Rabies_8 + Rabies_9 + Rabies_mean + Home_Again_mean + Otics_mean + 
               Essentials_mean + Ophthalmics_Antibiotics_mean, data = Monthly_Times) # 66 features
summary(model4)
# Multiple R-squared:  0.6037,	Adjusted R-squared:  0.602

#------------------------------------------- LR Monthly - with Dineof() -----------------------------------------
'--------               Running dineof() to fill NA values in Monthly_Times dataframe                          
 --------                    Then running Linear Model on data to see the results                              
 --------      NOTE: First run "LR Monthly - Before welch test" section to get Monthly_Times dataframe ready'

Monthly_Times <- Monthly_Times %>% select(-GROSS_SALES)
Monthly_Times <- Monthly_Times %>% replace_with_na_all(condition = ~.x == 0)
Monthly_Times <- as.matrix(Monthly_Times)

# --- Best seed is 3 --- 
#for (s in 1:10){
set.seed(3)
#print(s)
RES <- dineof(Monthly_Times, n.max = NULL, ref.pos = NULL, delta.rms = 1e-05, method = "svds")
#}

dineof_df = as.data.frame(RES$Xa)
Monthly_Times <- as.data.frame(Monthly_Times)
dineof_df$GROSS_SALES <- customers_sales$GROSS_SALES

model5 <- lm(GROSS_SALES ~ .,
             data=dineof_df)
summary(model5)
#Multiple R-squared:  ~ 0.4828,	Adjusted R-squared:  ~ 0.4787

#------------------------------------------- LR Monthly - After PCA -----------------------------------------
'---------------------               Passing dineof results to PCA                   --------------------
 ------------               Then running Linear Model from Principal Components            -------------'

pca <- prcomp(RES$Xa, center= TRUE, scale = TRUE)
pca.var <- pca$sdev^2
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)
barplot(pca.var.per, main="Scree Plot", xlab="Principal Component", ylab="Percent Variation")
plot(pca$x[,1],pca$x[,2])

pca_df <- as.data.frame(pca$x)
pca_df <- pca_df[,1:4]  #selecting first 4 Principal Components
pca_df$GROSS_SALES <- customers_sales$GROSS_SALES

model6 <- lm(GROSS_SALES ~ .,
             data=pca_df)
summary(model6)
#Multiple R-squared:  0.3033,	Adjusted R-squared:  0.3031 

#------------------------------------------- LR Quarterly -------------------------------------------------------
Quarterly_Times <- table2_month

Quarterly_Times <- Quarterly_Times %>% 
  select(SHIPTO_CUSTOMER_ID,GROUP_TIME_FRAME_QRT,times)
  group_by(SHIPTO_CUSTOMER_ID,
           GROUP_TIME_FRAME_QRT) %>%
  summarize(times = sum(times, na.rm = T))

Quarterly_Times <- Quarterly_Times %>% 
  select(SHIPTO_CUSTOMER_ID,
        GROUP_TIME_FRAME_QRT,
        times) %>%
  group_by(SHIPTO_CUSTOMER_ID,
           GROUP_TIME_FRAME_QRT) %>%
  summarize(times = sum(times, na.rm = T))

Quarterly_Times = 
  Quarterly_Times %>% 
  select(SHIPTO_CUSTOMER_ID,
         GROUP_TIME_FRAME_QRT,
         times) %>% 
  unique() %>% 
  spread(key = "GROUP_TIME_FRAME_QRT", 
         value = times)

Quarterly_Times[is.na(Quarterly_Times)] <- 0
Quarterly_Times <- aggregate(cbind(Quarterly_Times[,c(2:41)]), 
                           by=list(SHIPTO_CUSTOMER_ID=Quarterly_Times$SHIPTO_CUSTOMER_ID), FUN=sum)

Quarterly_Times <- inner_join(Quarterly_Times,customers_sales)
Quarterly_Times$SHIPTO_CUSTOMER_ID <- NULL
Quarterly_Times[1:41]<- lapply(Quarterly_Times[1:41], as.numeric)
