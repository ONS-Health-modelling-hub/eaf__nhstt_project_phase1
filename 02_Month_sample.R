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
  app_name = "xl-session",
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



#------------------------------------------------------------------------------------------
### 1. Load data 
#------------------------------------------------------------------------------------------

df_sdf <- sparklyr::sdf_sql(sc=sc,
                       paste0("SELECT * 
                                FROM project_name.",final_linked_dataset,""))

#------------------------------------------------------------------------------------------
### 2. Derive sample for regression
#------------------------------------------------------------------------------------------
#
#sample <- df_sdf %>%
#  filter(exposure_groups == "completed_treatment") %>% 
#  select(census_person_id) %>%
#  sdf_drop_duplicates() %>%
#  sdf_sample(fraction = sample_size,
#             replacement = FALSE,
#             seed = 12358)
#
#sdf_register(sample , 'sample')
#sql <- paste0("CREATE TABLE project_name.",sample_completed_df," AS SELECT * FROM sample")
#DBI::dbExecute(sc, sql)
#
#rm(sample)
#gc()


sample <- sparklyr::sdf_sql(sc=sc,
                      paste0("SELECT DISTINCT * FROM project_name.",sample_completed_df,""))

gc()

final_sample_df <- df_sdf %>%
                    select(month_sample_vars) %>%
                    inner_join(sample, by = "census_person_id") 

rm(sample)

final_sample_df <- collect(final_sample_df)
                    #memory_efficient_collect()

saveRDS(final_sample_df, paste0(outputs_folder, sample_completed_df,".RDS"))

rm(sample, final_sample_df)
gc()

#--------------------------------------------------------------------------
# sample individuals from dropped_out_of_treatment
#--------------------------------------------------------------------------
sample <- df_sdf %>%
  filter(exposure_groups == "dropped_out_of_treatment") %>% 
  select(census_person_id) %>%
  sdf_drop_duplicates() %>%
  sdf_sample(fraction = sample_size,
             replacement = FALSE,
             seed = 12358)

sdf_register(sample , 'sample')
sql <- paste0("CREATE TABLE project_name.",sample_dropped_out_df, " AS SELECT * FROM sample")
DBI::dbExecute(sc, sql)
rm(sample)

gc()

sample <- sparklyr::sdf_sql(sc=sc,
                      paste0("SELECT DISTINCT  *  FROM project_name.", sample_dropped_out_df,""))


final_sample_df <- df_sdf %>%
                    select(month_sample_vars) %>%
                    inner_join(sample, by = "census_person_id") %>% 
                    memory_efficient_collect()

saveRDS(final_sample_df, paste0(outputs_folder, sample_dropped_out_df,".RDS"))

rm(sample, final_sample_df)
gc()

#--------------------------------------------------------------------------
# Closing Spark sessions
#--------------------------------------------------------------------------
spark_disconnect(sc)
