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

#------------------------------------------------------------------------------------------
### Load/set files 
#------------------------------------------------------------------------------------------

# Functions
source("project_name/folder/functions.R")

## Config file
source("project_name/folder/config.R")

##OUTPUTS FOLDER
outputs_folder <- "project_name/Results/"


#HDFS folder with RDS files
hdfs_path = 'path'


#Define variables for fixed effect, exposure and controls 
fixed_list <- "census_person_id + t_first_therapy"
exposure_list <- "t_treatment_fct"
control_list <- "+ ns(qtr_date_format,2) + ns(quarterly_age,2) + qtr"

#------------------------------------------------------------------------------------------
### 1. Load sampled data 
#------------------------------------------------------------------------------------------

## Initialize an empty list to store dataframes 
df_list <- list() 

# Loop through each table name and apply the function 

for (table_name in table_names) { 
  
  # Store the result in the list
  df_list[[table_name]] <- load_rds_function(hdfs_path, 
                                             paste0(table_name, date),
                                             outputs_folder)
} 

# Combine all dataframes into one big dataframe 

df <- do.call(rbind, df_list) 
rm(df_list)

#------------------------------------------------------------------------------------------
### 2. Join datasets - add quarterly age and age bands
#------------------------------------------------------------------------------------------

df <- df %>%  mutate(
  qtr_date_format = as.Date(paste0(reference_date,"-01")))

df$qtr <- substr(df$reference_date,6,6)

treat_out <- readRDS(paste0(outputs_folder, "treatment_outcomes.RDS"))
appts <- readRDS(paste0(outputs_folder, 'number_appointments_religion_marital_stat.RDS'))

df <- left_join(df,treat_out, by="census_person_id") %>%
  left_join(.,appts, by="census_person_id")

# The 'Missing or not stated' category in ns_sec and qualifications is 2 so we remove it

df <- df %>% 
  filter(ns_sec != 'Missing or not stated') 

#------------------------------------------------------------------------------------------
### 2. Add additional variables
#------------------------------------------------------------------------------------------
 # Add quantiles
q = quantile(df$pay, prob=c(0.01, 0.99))

df <- df %>%  mutate(
   
   # Create in work variable based on quarterly pay
   in_work = ifelse(pay > 0, 1, 0),
   pay = case_when(pay < 0 ~ 0, 
                         pay > q[[2]]~ q[[2]],
                         TRUE ~ pay),
   treatment = ifelse(exposure_groups == 'completed_treatment', 1, 0),
   sampling_weight = ifelse(exposure_groups == 'completed_treatment', 1/qtr_sample, 1),

   t_treatment = ceiling(t_first_therapy *  treatment/4),
   t_treatment_fct = relevel(as.factor(t_treatment), ref="0"))

df$pay <- df$pay/3 
rm(q)

df <- df %>% 
mutate(
  treatment_outcome = 
    case_when(treatment_outcomes == 'improvement' | 
              treatment_outcomes == 'recovery' ~ 'Improvement or recovery' ,
              treatment_outcomes == 'deterioration' | 
              treatment_outcomes == 'no change' ~ 'No change or deterioration',
              TRUE ~ treatment_outcomes)) 

#------------------------------------------------------------------------------------------
### 3. Calculate Inverse Probability Weights (IPWs)
#------------------------------------------------------------------------------------------
#df <- df %>% select(-ipw)

## data at time 0
df_0 <- df %>% filter(t_first_therapy == 0)

model_treatment_all <- glm(treatment ~ 
                           referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec,
                                       
                             family=binomial,
                             data = df_0)


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



#------------------------------------------------------------------------------------------
### 4. Regression models
#------------------------------------------------------------------------------------------


##########################################################################
#                 Model 1a & 1b. No weights
# outcome = pay & employment
# exposure/varying vars = time to referral (as a factor) 
# control/varying vars =  control for month & age  & pandemic period
#                          
# fixed effect = time to referral and person 
# se cluster = person
##########################################################################

##
### monthly pay - no IPWs
fixed_effect_function(data = filter(df),
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = '1a_Qtr_Main_model_earnings_no_ipw',
                      y_aes ="Effect",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                      x_upper = 7,
                      min = -50, 
                      max = 40,
                      y_break = 10,
                      plot_type="bar",
                      title = "Effects of talking therapies on monthly earnings",
                      vline =0)


#  Being employed - no IPWs
fixed_effect_function(data = filter(df),
                      outcome = 'in_work',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = '1b_Qtr_Main_model_employment_no_ipw',
                      y_aes ="Effect",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                      x_upper = 7,
                      min = -1.0, 
                      max = 2.5,
                      y_break = 0.5,
                      plot_type="bar",
                      title = "Effects of talking therapies on employment",
                      vline =0)

gc()


#  Being employed - no IPWs
fixed_effect_function(data = filter(df,pay>0),
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = '1c_Qtr_Main_model_pay_no_ipw_IN_EMPLOYMENT_',
                      y_aes ="Effect",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                      x_upper = 7,
                      min = -70, 
                      max = 40,
                      y_break = 10,
                      plot_type="bar",
                      title = "Effects of talking therapies on monthly earnings",
                      vline =0)

gc()
##########################################################################
#                 Model 2a & 2b. Main models with IPWs
# outcome = pay & employment
# exposure/varying vars = time to referral (as a factor) 
# control/varying vars =  control for month & age  & pandemic period                          
# fixed effect = time to referral and person 
# se cluster = person
##########################################################################


fixed_effect_function(data = filter(df, !is.na(ipw)),
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight = "ipw",
                      cluster = 'census_person_id',
                      name = '2a_Qtr_Main_model_earnings_IPW_',
                      y_aes ="Effect",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                     x_upper = 7,
                      min = -50, 
                      max = 40,
                      y_break = 10,
                     plot_type = "bar",
                     title = "Effects of talking therapies on monthly earnings", 
                     vline = 0)
gc()

#  Being employed
fixed_effect_function(data = filter(df, !is.na(ipw)),
                      outcome = 'in_work',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight="ipw",
                      cluster = 'census_person_id',
                      name = '2b_Qtr_Main_model_employment_IPW_',
                      y_aes ="Effect",
                      x_aes ="Years since first therapy",
                     x_lower = -4,
                     x_upper = 7,
                      min = -1.0, 
                      max = 2.5,
                      y_break = 0.5,
                     plot_type="bar",
                      title = "Effects of talking therapies on employment",
                     vline =0)

gc()

fixed_effect_function(data = filter(df, pay>0),
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight = "ipw",
                      cluster = 'census_person_id',
                      name = '2c_Qtr_Main_model_earnings_IPW_IN_EMPLOYMENT_',
                      y_aes ="Effect",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                     x_upper = 7,
                      min = -70, 
                      max = 40,
                      y_break = 10,
                     plot_type = "bar",
                     title = "Effects of talking therapies on monthly earnings", 
                     vline = 0)
gc()



#------------------------------------------------------------------------------------------
### 4. BREAKDOWNS BY CHARACTERISTICS
#------------------------------------------------------------------------------------------
# some variable names are too long for graphs so shorten the names

df <- df %>% select(-ipw)

df <- df %>% 
  mutate(referral_employment_stat = 
case_when(
  referral_employment_stat == "Long-term sick/disabled" ~ "Not working, sick or disabled",
  referral_employment_stat == "Unemployed, not seeking work" ~ "Not working, not seeking work",
  referral_employment_stat == "Unemployed, seeking work" ~ "Not working, seeking work", 
  TRUE ~ referral_employment_stat),       
         ns_sec = 
  case_when(ns_sec == "Higher managerial, administrative and professional occupations" ~ 'Higher managerial',
            ns_sec == "Lower managerial, administrative and professional occupations" ~ 'Lower managerial',
            ns_sec == "Intermediate occupations" ~ "Intermediate occupations",
            ns_sec == "Small employers and own account workers" ~ "Small employers",
            ns_sec == "Lower supervisory and technical occupations"~ 'Lower supervisory',
            ns_sec == "Semi-routine occupations" ~ "Semi-routine occupations",
            ns_sec == "Routine occupations"~ "Routine occupations",
            ns_sec == "Never worked" ~ "Never worked",
            ns_sec ==  "Long-term unemployed" ~ "Long-term unemployed",
            ns_sec == "Full-Time students" ~ "Full-Time students",
            ns_sec == "Not classifiable"  ~ "Not classifiable",
            TRUE ~ "Missing or not stated"))
df <- df %>% 
  mutate(diagnosis = 
   case_when(diagnosis == 'Panic disorder [episodic paroxysmal anxiety]' ~ 'Panic disorder',
             diagnosis == 'Other anxiety or stress related disorder' ~ 'Other anxiety',
             diagnosis == 'Specific (isolated) phobias' ~ 'Specific phobias',
             diagnosis == 'Post-traumatic stress disorder' ~ 'PTSD',
             diagnosis == 'Generalized anxiety disorder' ~'GAD',
             diagnosis == 'Obsessive-compulsive disorder' ~ 'OCD',
             diagnosis == 'Mixed anxiety and depressive disorder' ~ 'Mixed anxiety & depression',
                  TRUE ~ diagnosis))
df <- df %>% 
  mutate(deprived = 
         case_when(deprived == 'Deprived, 1 dimension' ~ 'Deprived (1)',
                   deprived == 'Deprived, 2 dimensions' ~ 'Deprived (2)',
                   deprived == 'Deprived, 3 dimensions' ~ 'Deprived (3)',
                   deprived == 'Deprived, 4 dimensions' ~ 'Deprived (4)',
                   TRUE ~ deprived))


################################################################################
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "sex",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by sex", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "sex",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by sex", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##Age bands
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "age_bands",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by age bands", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")
gc()


#Employment
regression_with_breakdowns_function(data = filter(df),
              category = "age_bands",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by age bands", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Ethnicity 
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "ethnic_group_tb",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by ethnic group", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "ethnic_group_tb",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by ethnic group", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### country_of_birth_agg 
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "first_medication",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by first medication", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "first_medication",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by first medication", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### country_of_birth_agg 
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "diagnosis",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by diagnosis", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "diagnosis",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by diagnosis", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### referral_employment_stat 
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "referral_employment_stat",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by employment status at referral", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "referral_employment_stat",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by employment status at referral", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### deprived 
#Earnings
regression_with_breakdowns_function(data = filter(df, deprived != 'Missing or not stated'),
              category = "deprived",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by deprivation", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df, deprived != 'Missing or not stated'),
              category = "deprived",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by deprivation", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### country_of_birth_agg 
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "highest_qualification",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by highest qualification", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "highest_qualification",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by highest qualification ", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           hh_dependent_children+
                           ns_sec")

gc()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Region 
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "Region",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by regions", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "Region",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by regions ", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

### therapy_intensity
#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "therapy_intensity",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by therapy intensity", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "therapy_intensity",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by therapy intensity", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "ns_sec",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by NSSEC", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children")

gc()

#Earnings
regression_with_breakdowns_function(data = filter(df),
              category = "ns_sec",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by NSSEC", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children")

gc()

# Sub-analysis treatment outcomes
#Earnings
#df1<- df %>% filter(pay>0)
regression_with_breakdowns_function(data = filter(df, treatment_outcome != 'not completed treatment/missing scores'),
              category = "treatment_outcome",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on earnings by treatment outcomes", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()

#Employment
regression_with_breakdowns_function(data= filter(df, treatment_outcome != 'not completed treatment/missing scores'),
              category = "treatment_outcome",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect",
              x_aes ="Years since first therapy",
              x_lower = -4,
              x_upper = 7,
              title = "Effects on probability of employment by treatment outcomes", 
              plot_type="bar",
              vline = 0,
             ipw_vars = "referral_employment_stat + 
                           ns(age_at_referral,df=5,
                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
                          therapy_intensity + 
                           first_medication +
                           sourcereferral+
                           Region +
                           deprived +
                           highest_qualification+
                           hh_dependent_children+
                           ns_sec")

gc()
####################
