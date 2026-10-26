#------------------------------------------------------------------------------------------
### Setting up session
#------------------------------------------------------------------------------------------

library(sparklyr)
library(dplyr)
library(tidyverse)
library(DBI)
library(lubridate)
library(openxlsx) 


xl_config <- spark_config()
xl_config$spark.executor.memory <- "20g"
xl_config$spark.yarn.executor.memoryOverhead <- "2g"
xl_config$spark.executor.cores <- 5
xl_config$spark.dynamicAllocation.enabled <- "true"
xl_config$spark.dynamicAllocation.maxExecutors <- 12
xl_config$spark.sql.shuffle.partitions <- 240
#xl_config$spark.shuffle.service.enabled <- "true"

sc <- spark_connect(
  master = "yarn-client",
  app_name = "xl-session",
  config = xl_config)


#CHECK CONNECTION IS OPEN
spark_connection_is_open(sc)

#------------------------------------------------------------------------------------------
### Load functions 
#------------------------------------------------------------------------------------------
# Functions
source("project_name/folder/functions.R")

## Config file
source("project_name/folder/config.R")

##OUTPUTS FOLDER
outputs_folder <- "project_name/Results/"
#------------------------------------------------------------------------------------------
### Load whole data
#------------------------------------------------------------------------------------------


# Load in linked fainal data on all cohorts from iapt, census, hmrc
# (after running nhs_tt function for start date 15_16 and end date 18_19)

linked_data <- sparklyr::sdf_sql(sc=sc,
                       paste0("SELECT * 
                                FROM project_name.",final_linked_dataset,""))


#--------------------------------------------------------------------------#
###  Load sample
#--------------------------------------------------------------------------#

df <- readRDS(paste0(outputs_folder, sample_df_name))



#--------------------------------------------------------------------------#
###  QA on a sample 
#--------------------------------------------------------------------------#

#--- 1. Row count in df ----#
row_count_sample <- getRowCount(df, "sample_data")


#--- 2. Distinct count -----#

distinct_sample <- df %>%
  summarise(across(everything(), n_distinct))


#--- 3. Unique person per month, unique person in dataset----#

description_text <- ('QA to check if we have 1 person(census_id) per calendar month, and no duplicates')
res1<- count_var2_more_than_1(df, census_person_id, month, description_text)

description_text <- ('QA to check if we have 1 person(iaptid) per calendar month, and no duplicates')
res2 <- count_var2_more_than_1(df, census_person_id, month, description_text) 

description_text <- ('QA to check if we have 1 person(census_id) per time_to_referral, and no duplicates')
res3 <- count_var2_more_than_1(df, census_person_id, t_referral, description_text) 


# Bind results
unique_id_per_month_or_t_referral <- rbind(res1,res2,res3)




#---- 4. one referral date, t_to_referral per person ------#
# Function groups by var1 and get count of distinct for var2 

description_text <- ('QA to check if person has 1 referral date')
res4 <- unique_var_per_person(df, census_person_id, referral_date, description_text)


description_text <- ('QA to check if those who completed have unique treatment date')
res5 <- unique_var_per_person(df, census_person_id, last_therapy_date, description_text)


# Bind results
unique_referral_date_last_therapy <- rbind(res4, res5)


#---- 5. Min, max, mean ------#

# Calculate min, max, and mean for all columns using across
res_min_max_mean_summary <- df %>%
  summarise(across(everything(), list(min = min, max = max, mean = mean), .names = "{.col}_{.fn}"))


#--- 6. NAs -------#

res_NAs_sample <- df %>%
  summarise(across(everything(), 
                   ~sum(is.na(.))))



#------- 7. t_referral == 0 ----------#

# everyone needs to have t_referral value of 0 amongest other values 

df_id_count_t_referral_0s <- df %>% 
  select(census_person_id, t_referral) %>% 
  mutate(count_uniq_ids = n_distinct(census_person_id)) %>%
  filter(t_referral == 0) %>% 
  mutate(count_0s_t_referral = n()) %>%
  select(count_uniq_t_referral,count_uniq_ids) %>% distinct() %>%
  mutate(difference =count_uniq_t_referral -count_uniq_ids,
         qa_outcome = ifelse(difference == 0, 'PASS','FAIL'))


#------ 8. quint variable --------#
# check what is the logic for quint variable 
# Check if those with sum 0 for pay in the 12 months before referral have the same count as "No_income" in quint 
# Note - this is check will not be outputed 


p_12_counnt_0 <- linked_data %>%
  group_by(census_person_id) %>%
  filter(t_referral >=-12 & t_referral < 0) %>%
  summarise(pay_12_months_before = sum(pay)) %>%
 
  mutate(pay_12_months_before = 
        ifelse(is.na(pay_12_months_before),0, pay_12_months_before)) %>%

  filter(pay_12_months_before <= 0) %>%
  summarize(count_0_pay = n())



quint_no_income <- linked_data %>% sdf_drop_duplicates('census_person_id') %>% group_by(quint) %>%
  summarize(count = n()) 




#--- save all 7 results for a sample --------#
dataset_names <- list('row_count_sample' = row_count_sample,        
                      'distinct_count_sample' = distinct_sample,    
                      'id_per_month_or_t_referral' = unique_id_per_month_or_t_referral,
                      'referral_date_last_therapy' = unique_referral_date_last_therapy,
                      'min_max_mean_summary'= res_min_max_mean_summary,
                      'NAs_sample' = res_NAs_sample,
                      't_referral_0s' = df_id_count_t_referral_0s)

write.xlsx(dataset_names, paste0(outputs_folder,"QA_checks_sample_", Sys.Date(), ".xlsx"))





#-----------------------------------------------------------------------------------------------
# Data checks  -  LINKED_DATA (SPARK)
#------------------------------------------------------------------------------------------------


#--- 1. Row count in df ----#
row_count_spark <- getRowCount_spark(linked_data, "linked_data")


#--- 2. Distinct count -----#

distinct_spark <- getDistinctCount_spark(linked_data)


#--- 3. Unique person per month, unique person in dataset----#

description_text <- ('QA to check if we have 1 person(census_id) per calendar month, and no duplicates')
res1<- count_var2_more_than_1_spark(linked_data, census_person_id, month, description_text)

description_text <- ('QA to check if we have 1 person(iaptid) per calendar month, and no duplicates')
res2 <- count_var2_more_than_1_spark(linked_data, iapt_person_id, month, description_text) 

description_text <- ('QA to check if we have 1 person(census_id) per time_to_referral, and no duplicates')
res3 <- count_var2_more_than_1_spark(linked_data, census_person_id, t_referral, description_text) 


# Bind results
unique_id_per_month_or_t_referral_spark <- rbind(res1,res2,res3)



#---- 4. one referral date, t_to_referral per person ------#
# Function groups by var1 and get count of distinct for var2 

description_text <- ('QA to check if person has 1 referral date')
res4 <- unique_var_per_person_spark(linked_data, census_person_id, referral_date, description_text)


description_text <- ('QA to check if those who completed have unique treatment date')
res5 <- unique_var_per_person_spark(linked_data, census_person_id, last_therapy_date, description_text)


# Bind results
unique_referral_date_last_therapy_spark <- rbind(res4, res5)



#---- 5. Min max, mean --------# 

                              
res_min_max_mean_spark <- min_max_mean_spark(linked_data)


#--- 6. Check NAs -----#

nas_spark <- check_na(linked_data)

#------- 7. t_referral == 0 ----------#

# everyone needs to have t_referral value of 0 amongest other values 


count_unique_census_id <- linked_data %>% 
  select(census_person_id, t_referral) %>% 
  sdf_distinct(census_person_id) %>%
  summarise(count_uniq_ids =n()) 
            

count_referral_0 <- linked_data %>% 
  filter(t_referral==0) %>%
  summarise(count_uniq_t_referral = n()) 

difference_id_referral_0_spark <- merge(count_unique_census_id, count_referral_0) %>%
  mutate(difference =count_uniq_t_referral -count_uniq_ids,
         qa_outcome = ifelse(difference == 0, 'PASS','FAIL'))
  


#--- save all 7 results for a sample --------#
dataset_names_spark <- list('row_count' = row_count_spark,        
                      'distinct_count' = distinct_spark,    
                      'id_per_month_or_t_referral' = unique_id_per_month_or_t_referral_spark,
                      'referral_date_last_therapy' = unique_referral_date_last_therapy_spark,
                      #'min_max_mean_summary'= res_min_max_mean_spark,
                      'NAs_spark' = nas_spark,
                      't_referral_0s' = difference_id_referral_0_spark)


write.xlsx(dataset_names_spark, paste0(outputs_folder,"QA_checks_whole_data_", Sys.Date(), ".xlsx"))



#--------------------------------------------------------------------------
# Closing Spark sessions
#--------------------------------------------------------------------------
spark_disconnect(sc)
