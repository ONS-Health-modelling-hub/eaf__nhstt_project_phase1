#------------------------------------------------------------------------------------------
### Setting up session
#------------------------------------------------------------------------------------------

library(sparklyr)
library(dplyr)
library(tidyverse)
library(DBI) 
library(lubridate)
library(openxlsx) 
library(lfe)
library(splines)
library(survival)

xl_config <- spark_config()
xl_config$spark.executor.memory <- "20g"
xl_config$spark.yarn.executor.memoryOverhead <- "2g"
xl_config$spark.executor.cores <- 5
xl_config$spark.dynamicAllocation.enabled <- "true"
xl_config$spark.dynamicAllocation.maxExecutors <- 30
xl_config$spark.sql.shuffle.partitions <- 240
#xl_config$spark.shuffle.service.enabled <- "true"

sc <- spark_connect(
  master = "yarn-client",
  app_name = "xl-session_samples",
  config = xl_config)


#CHECK CONNECTION IS OPEN
spark_connection_is_open(sc)

#------------------------------------------------------------------------------------------
### Load/set files 
#------------------------------------------------------------------------------------------
# Functions
source("project_name/folder/functions.R")

## Config file
source("project_name/folder/config.R")

##OUTPUTS FOLDER
outputs_folder <- "project_name/Results/"

hdfs_path = 'path'

#------------------------------------------------------------------------------------------
### 1. Load data 
#------------------------------------------------------------------------------------------

df_qtr <- sparklyr::sdf_sql(sc=sc,
                       paste0("SELECT * 
                                FROM project_name.", quarterly_df ,""))

#------------------------------------------------------------------------------------------
### 2. Derive 100% sample from quarterly data for regression - DROPPED OUT OF TREATMENT
#------------------------------------------------------------------------------------------
#Save 100% of of the dropped out treatemnt groups as RDS (partitions due to memory issues)

#Females
save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment", sex_group = 'Females',
           age_band = '25-34',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment", sex_group = 'Females',
           age_band = '25-34',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment", sex_group = 'Females',
           age_band = '25-34',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment", sex_group = 'Females',
           age_band = '25-34',source1 = 'Self-referral', outputs_folder, hdfs_path, date)

save_rds_1(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment",
         sex_group = 'Females',age_band = '35-44',outputs_folder,hdfs_path, date)

save_rds_1(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment",
         sex_group = 'Females',age_band = '45-54',outputs_folder, hdfs_path, date)

save_rds_1(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment",
         sex_group = 'Females',age_band = '55-60',outputs_folder,hdfs_path,date)

#Males
save_rds_1(df_qtr, qtr_sample_vars, treatment_group = "dropped_out_of_treatment",
         sex_group = 'Males', age_band = '25-34',outputs_folder, hdfs_path, date)

save_rds_1(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment",
         sex_group = 'Males',age_band = '35-44',outputs_folder,hdfs_path, date)

save_rds_1(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment",
         sex_group = 'Males',age_band = '45-54',outputs_folder, hdfs_path, date)

save_rds_1(df_qtr,qtr_sample_vars,treatment_group = "dropped_out_of_treatment",
         sex_group = 'Males',age_band = '55-60',outputs_folder,hdfs_path,date)


#------------------------------------------------------------------------------------------
### 3. Derive sample from quarterly data for regression - COMPLETED TREATMENT
#------------------------------------------------------------------------------------------

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
         age_band = '25-34',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
         age_band = '25-34',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '25-34',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

#~~~~~~~~#


#df_qtr <- df_qtr %>% filter(age_at_referral < 27) 
#date_temp = paste0("d1",date) 
#save_rds_2(df_qtr, qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
#           age_band = '25-34',source1 = 'Self-referral', outputs_folder, hdfs_path, date_temp)


#df_qtr <- df_qtr %>% filter(age_at_referral >= 27 & age_at_referral < 30)
#date_temp = paste0("d2",date) 
#save_rds_2(df_qtr, qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
#           age_band = '25-34',source1 = 'Self-referral', outputs_folder, hdfs_path, date_temp)

#
#df_qtr <- df_qtr %>% filter(age_at_referral >= 30)
#date_temp = paste0("d3",date) 
#save_rds_2(df_qtr, qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
#           age_band = '25-34',source1 = 'Self-referral', outputs_folder, hdfs_path, date_temp)


#~~~~~~~~#

save_rds_2(df_qtr, qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '25-34',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '35-44',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '35-44',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '35-44',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

#save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
#           age_band = '35-44',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


df_qtr <- df_qtr %>% filter(age_at_referral < 40) 
date_temp = paste0("d1",date) 
save_rds_2(df_qtr, qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '35-44',source1 = 'Self-referral', outputs_folder, hdfs_path, date_temp)


df_qtr <- df_qtr %>% filter(age_at_referral >= 40)
date_temp = paste0("d2",date) 
save_rds_2(df_qtr, qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '35-44',source1 = 'Self-referral', outputs_folder, hdfs_path, date_temp)




save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '45-54',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '45-54',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '45-54',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '45-54',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '55-60',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '55-60',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '55-60',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Females',
           age_band = '55-60',source1 = 'Self-referral', outputs_folder, hdfs_path, date)



#Males
save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '25-34',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '25-34',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '25-34',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '25-34',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '35-44',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '35-44',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '35-44',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '35-44',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '45-54',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '45-54',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '45-54',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '45-54',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '55-60',source1 = 'Other', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '55-60',source1 = 'Missing or not stated', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '55-60',source1 = 'Primary Health Care', outputs_folder, hdfs_path, date)

save_rds_2(df_qtr,qtr_sample_vars,treatment_group = "completed_treatment", sex_group = 'Males',
           age_band = '55-60',source1 = 'Self-referral', outputs_folder, hdfs_path, date)


#--------------------------------------------------------------------------
# Closing Spark sessions
#--------------------------------------------------------------------------
spark_disconnect(sc)
