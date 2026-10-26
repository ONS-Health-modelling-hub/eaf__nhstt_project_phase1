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
library(ggplot2)

xl_config <- spark_config()
xl_config$spark.executor.memory <- "20g"
xl_config$spark.yarn.executor.memoryOverhead <- "2g"
xl_config$spark.executor.cores <- 5
xl_config$spark.dynamicAllocation.enabled <- "true"
xl_config$spark.dynamicAllocation.maxExecutors <- 12
xl_config$spark.sql.shuffle.partitions <- 240
xl_config$spark.sql.session.timeZone = "UTC+01:00"
xl_config$spark.sql.legacy.timeParserPolicy = "LEGACY"
xl_config$spark.sql.parquet.datetimeRebaseModeInRead = "CORRECTED"

xl_config$spark.sql.parquet.int96RebaseModeInRead
xl_config$spark.sql.parquet.int96RebaseModeInRead = "CORRECTED"

sc <- spark_connect(
  master = "yarn-client",
  app_name = "xl-sessionyuyt",
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

df <- sparklyr::sdf_sql(sc=sc,
                       paste0("SELECT * 
                                FROM project_name.", quarterly_df ,""))

df <- df %>% 
  filter(ns_sec != 'Missing or not stated')
df <- df %>% 
      filter(treatment_outcomes != 'not completed treatment/missing scores')

df <- df %>% select(-exposure_groups) %>%
  mutate(

   # Create in work variable based on quarterly pay
  in_work = ifelse(pay > 0, 1, 0),

  exposure_groups = 
    ifelse(treatment_outcomes == 'improvement' | 
           treatment_outcomes == 'recovery', 'Improvement or recovery' ,
           'No change or deterioration'),

  treatment = ifelse(exposure_groups == 'Improvement or recovery', 1, 0),
  t_treatment = ceiling(t_first_therapy *  treatment/4))

df <- df %>% 
  mutate(
number_appointments = ic_count_treatment_appointments, 

number_appointments = 
    case_when(number_appointments <= 2 ~ '2',
              number_appointments > 2 & number_appointments <= 5 ~ '3 to 5',
              number_appointments > 5 & number_appointments <= 8 ~ '6 to 8',
              number_appointments > 8 & number_appointments <= 15 ~ '9 to 15',
             number_appointments > 15 ~ '16 or more',
             TRUE ~ 'Missing'))

quantiles <- sdf_quantile(df, "pay", probabilities = c(0.01,0.99))
q99 <- quantiles[[2]]

df <- df %>% mutate(pay = ifelse(pay > q99, q99, pay))
#------------------------------------------------------------------------------------------
### 2. Descriptives
#------------------------------------------------------------------------------------------

# Exposure groups counts & proportions 
res1 <- proportion_function(df, exposure_groups)
res2 <- min_max_mean_grouped(df, age_at_referral, sex)
res3 <- min_max_mean_grouped(df, age_at_referral, exposure_groups)


df_pre <- df %>% 
  select(census_person_id, t_first_therapy) %>%
  filter(t_first_therapy < 0) %>%
  group_by(census_person_id) %>%
  mutate(pre_follow_up_limit = min(t_first_therapy)) %>%
  ungroup() %>% select(-t_first_therapy)

df_post <- df %>% 
  select(census_person_id, t_first_therapy) %>%
  filter(t_first_therapy > 0) %>%
  group_by(census_person_id) %>%
  mutate(post_follow_up_limit = max(t_first_therapy)) %>% 
  ungroup() %>% select(-t_first_therapy)

df1 <- left_join(df, df_pre, by="census_person_id") %>%
  left_join(., df_post, by="census_person_id")


res4 <- df1 %>%
  summarise(min_t_therapy_Follow_Up_all = min(t_first_therapy),
            max_t_therapy_Follow_Up_all = max(t_first_therapy),

            pre_min_t_therapy_Follow_Up = max(pre_follow_up_limit),
            pre_max_t_therapy_Follow_Up = min(pre_follow_up_limit),
            pre_average_therapy_Follow_Up = mean(pre_follow_up_limit),


            post_min_t_therapy_Follow_Up = min(post_follow_up_limit),
            post_max_t_therapy_Follow_Up = max(post_follow_up_limit),
            post_average_therapy_Follow_Up = mean(post_follow_up_limit))
res4b <- df1 %>%
group_by(exposure_groups) %>%
  summarise(        
            pre_min_t_therapy_Follow_Up = max(pre_follow_up_limit),
            pre_max_t_therapy_Follow_Up = min(pre_follow_up_limit),
            pre_average_therapy_Follow_Up = mean(pre_follow_up_limit),

            post_min_t_therapy_Follow_Up = min(post_follow_up_limit),
            post_max_t_therapy_Follow_Up = max(post_follow_up_limit),
            post_average_therapy_Follow_Up = mean(post_follow_up_limit))


rm(df_pre,df_post,df1)
gc()


argument_list <- c('sex', 
                   'ethnic_group_tb', 
                   'ns_sec',
                   'disability_category_11',
                   'disability_binary',
                   'age_bands',
                   'highest_qualification',
                   'country_of_birth_agg',
                   'hh_family_composition',
                   'hh_dependent_children',
                   'number_appointments',
                   'sourcereferral',
                   'referral_employment_stat',
                   'Region',
                   'deprived',
                   'therapy_intensity',
                   'diagnosis',
                   'first_medication')


# CATEGORICAL VARIABLES - COUNTS & PROPORTIONS & BY EXPOSURE GROUPS
res5 <- proportion_for_var_list (df, argument_list)
res6 <- df %>%
  mutate(exposure_groups = ifelse(exposure_groups == 'Improvement or recovery',
                                 'recovered_improved', 'no_change_worse')) %>%
  proportion_for_exposure_by_var_list(., argument_list)
gc()

# Results showing proportions e.g. of exposure groups who are Males 
res7 <- proportion_for_var_by_exposure(df, argument_list)
gc()

# Results in wide format 
library(tidyr)
wide_table <- res6 %>% 
  pivot_wider(names_from = exposure_groups,
              values_from = c(Count,	total,	percentage)) %>%
  select(-contains("total_")) %>% 
  mutate(Recovered_improved = 
          paste(Count_recovered_improved, 
                " (", round(percentage_recovered_improved, 1), "%)",
                sep = ""),
         No_change_deterioration = 
          paste(Count_no_change_worse, 
                " (", round(percentage_no_change_worse, 1), "%)", 
                sep = "")) %>%
  select(Variable_names, Category_name, Recovered_improved, No_change_deterioration) %>% 
  rename(Characteristic = Variable_names,
         Category = Category_name)
          
# Fromatted results with proportions for whole df
proportions_formatted <- res5 %>%
  arrange(Variable_names) %>%
  mutate(Count_percentage = paste(Count, " (", round(percentage, 1), "%)", sep = "")) %>%
  rename(Characteristic = Variable_names,
         Category = Category_name,
         'Count(%)' = Count_percentage) %>%
               select(Characteristic, Category, 'Count(%)')



#------------------------------------------------------------------------------------------
### 2. Descriptives - Pay on the date of first therapy
#------------------------------------------------------------------------------------------

total_all  <- df %>% filter(t_first_therapy == 0) %>% sdf_nrow()
total_comp <- df %>% filter(t_first_therapy == 0 & 
                            exposure_groups == 'Improvement or recovery') %>% sdf_nrow()
total_drop <- df %>% filter(t_first_therapy == 0 &
                            exposure_groups == 'No change or deterioration') %>% sdf_nrow()

# pay on the quarter of the first therapy
pay_on_therapy1 <- df %>%
  filter(t_first_therapy == 0) %>%
  summarise(exposure_groups = 'All', 
            qtr_mean_pay = mean(pay),
            qtr_median_pay = median(pay),
            qtr_min = min(pay),
            qtr_max = max(pay),
            qtr_sd = sd(pay)) 


pay_on_therapy2 <- df %>% 
  filter(t_first_therapy == 0) %>%
  group_by(exposure_groups) %>%
  summarise(qtr_mean_pay = mean(pay),
            qtr_median_pay = median(pay),
            qtr_min = min(pay),
            qtr_max = max(pay),
            qtr_sd = sd(pay)) 

res_pay1_quarterly <- rbind(pay_on_therapy1,pay_on_therapy2)

# Pay for ONLY those in employment at the time of first therapy

p1 <- df %>%
  filter(in_work == 1) %>%
  filter(t_first_therapy == 0) %>%
  summarise(exposure_groups = 'All', 
            qtr_mean_pay = mean(pay),
            qtr_median_pay = median(pay),
            qtr_min = min(pay),
            qtr_max = max(pay),
            qtr_sd = sd(pay)) 


p2 <- df %>% 
  filter(in_work == 1) %>%
  filter(t_first_therapy == 0) %>%
  group_by(exposure_groups) %>%
  summarise(qtr_mean_pay = mean(pay),
            qtr_median_pay = median(pay),
            qtr_min = min(pay),
            qtr_max = max(pay),
            qtr_sd = sd(pay)) 

res_pay2_quarterly <- rbind(p1,p2)



# Employment stats on the date of first therapy

employment_on_therapy1 <- df %>%
  filter(t_first_therapy == 0) %>%
  group_by(in_work) %>% 
  summarise(exposure_groups ="All",
            n =n(),
            total = total_all) %>% 
  mutate(Percentage = (n/total)*100) 
  

employment_on_therapy2 <- df %>% 
  filter(t_first_therapy == 0 & 
         exposure_groups == 'Improvement or recovery') %>%
  group_by(exposure_groups,in_work) %>%
  summarise(n =n(),
            total = total_comp) %>% 
  mutate(Percentage = (n/total)*100) 

employment_on_therapy3 <- df %>% 
  filter(t_first_therapy == 0 & 
         exposure_groups == 'No change or deterioration') %>%
  group_by(exposure_groups,in_work) %>%
  summarise(n =n(),
            total = total_drop) %>% 
  mutate(Percentage = (n/total)*100)

employment_res1 <- rbind(employment_on_therapy1,employment_on_therapy2,employment_on_therapy3)



#########################################################################
# Divide by 3 to make pay monthly (depending what we need to report Q/M)
df1 <- df %>% mutate(pay = pay/3)

# pay on the quarter of the first therapy
pay_on_therapy1 <- df1 %>%
  filter(t_first_therapy == 0) %>%
  summarise(exposure_groups = 'All', 
            mean_pay = mean(pay),
            median_pay = median(pay),
            min = min(pay),
            max = max(pay),
            sd = sd(pay)) 


pay_on_therapy2 <- df1 %>% 
  filter(t_first_therapy == 0) %>%
  group_by(exposure_groups) %>%
  summarise(mean_pay = mean(pay),
            median_pay = median(pay),
            min = min(pay),
            max = max(pay),
            sd = sd(pay)) 

res_pay1_monthly <- rbind(pay_on_therapy1,pay_on_therapy2)

# Pay for ONLY those in employment at the time of first therapy
# pay on the quarter of the first therapy
pay_on_therapy3 <- df1 %>%
  filter(in_work == 1) %>%
  filter(t_first_therapy == 0) %>%
  summarise(exposure_groups = 'All', 
            mean_pay = mean(pay),
            median_pay = median(pay),
            min = min(pay),
            max = max(pay),
            sd = sd(pay)) 


pay_on_therapy4 <- df1 %>% 
  filter(in_work == 1) %>%
  filter(t_first_therapy == 0) %>%
  group_by(exposure_groups) %>%
  summarise(mean_pay = mean(pay),
            median_pay = median(pay),
            min = min(pay),
            max = max(pay),
            sd = sd(pay)) 

res_pay2_monthly <- rbind(pay_on_therapy3,pay_on_therapy4)
########################################################################



#--- save all 8 results for linked_data --------#
dataset_names <- list('exposure_groups' = res1, 
                      'age_distribution' = res2,
                      'age_bands_counts' = res3,
                      'sub_A_follow_up_stats' = res4,
                      'sub_A_follow_up_stats_exp_g' = res4b,
                      'proportion_census' = res5,
                      'proportion_exposure_census'= res6,
                      'proportion_census_exposure' = res7,
                      'comparisons_wide_table' = wide_table,
                      'proportion_all_formatted' = proportions_formatted,
                      'qtr_pay_on_1ther_all' = res_pay1_quarterly,
                      'qtr_pay_1ther_employed'=res_pay2_quarterly,
                      'employment_on_1ther' =employment_res1,
                      'm_pay_1ther_all'= res_pay1_monthly,
                      'm_pay_1ther_employed'= res_pay2_monthly)

write.xlsx(dataset_names, 
           paste0(outputs_folder,"4_Descriptive_statistics_SUB_ANALYSIS_", Sys.Date(), ".xlsx"))




#------------------------------------------------------------------------------------------
### Graphs 
#------------------------------------------------------------------------------------------

df1 <- df %>% mutate(pay = pay/3)


### GRAPH 2 -  Monthly pay average of all broken down by exposure groups
temp_df_2 <- df1 %>% 
              mutate(pay = as.numeric(pay)) %>%
              group_by(reference_date, exposure_groups) %>% 
              summarise(average_monthly_pay = mean(pay))
temp_df_2 <- collect(temp_df_2)
temp_df_2$date_label <- sub("(\\b\\d{4}-)(\\d{1}\\b)", "\\10\\2", temp_df_2$reference_date) 
  
# plot graph
g2 <- temp_df_2 %>% 
  ggplot(aes(x = date_label, 
             y = average_monthly_pay, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Monthly average pay broken down by exposure groups",
       x ="Date", 
       y = "Average monthly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')


temp_df_3 <- df1 %>% 
              mutate(pay = as.numeric(pay)) %>%
              group_by(t_first_therapy, exposure_groups) %>% 
              summarise(average_monthly_pay = mean(pay))  
# plot graph
g3 <- temp_df_3 %>% 
  ggplot(aes(x = t_first_therapy, 
             y = average_monthly_pay, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Monthly average pay broken down by exposure groups",
       x ="Time to first therapy in quarters", 
       y = "Average monthly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')

####
IPWs
####

## data at time 0
df_0 <- df %>% filter(t_first_therapy == 0)

model_treatment_all <- df_0 %>%
  ml_logistic_regression(treatment ~ 
                         referral_employment_stat + 
                         #ns(age_at_referral,df=5,
                         #   Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                         first_medication +
                         sourcereferral+
                         Region +
                         deprived +
                         highest_qualification + 
                         hh_dependent_children +
                         ns_sec)


df_0$p <- predict(model_treatment_all, df_0, type="response")
df_0$ipw <- ifelse(df_0$treatment == 1,
                    1 /df_0$p,
                    1/(1-df_0$p))

# join to main dataset & apply sampling weight
df <- df %>%
  left_join(select(df_0,  
                   census_person_id,ipw), by="census_person_id") %>%
  mutate(ipw  = ifelse(!is.na(ipw),  ipw*sampling_weight, ipw))

rm(df_0, model_treatment_all)
gc()
