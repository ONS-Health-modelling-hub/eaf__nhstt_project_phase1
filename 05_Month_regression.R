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


#------------------------------------------------------------------------------------------
### 1. Load sampled data 
#------------------------------------------------------------------------------------------

completed_sample <- readRDS(paste0(outputs_folder,sample_completed_df, ".RDS"))

started_sample <- readRDS(paste0(outputs_folder, sample_dropped_out_df,".RDS"))

rf_df <- readRDS(paste0(outputs_folder, "randomForest_ipw.RDS"))


#ipw_rf
#------------------------------------------------------------------------------------------
### 2. Join datasets 
#------------------------------------------------------------------------------------------

df <- bind_rows(completed_sample, started_sample) %>%  
      left_join(., rf_df, by = "census_person_id") %>% 
  
  # create calendar month variable (no year)

  mutate(cal_month = month(month),
         cal_month = month.abb[cal_month],
         cal_month = as.factor(cal_month),
         pandemic = ifelse(month >= '2020-03-01',1,0)) 


rm(completed_sample, started_sample, rf_df)
gc()


#------------------------------------------------------------------------------------------
### 2. Add additional edits for regression 
#------------------------------------------------------------------------------------------

# Add quantiles
q = quantile(df$pay, prob=c(0.01, 0.99))

 df <- df %>%
  mutate(pay = case_when(pay < 0 ~ 0, 
                         pay > q[[2]]~ q[[2]],
                         TRUE ~ pay),
         
         # Create in work variable based on monthly pay
         in_work = ifelse(pay > 0, 1, 0),
         
         treatment = ifelse(exposure_groups == 'completed_treatment', 1, 0),
         t_treatment3 = ceiling(t_referral *  treatment /12),
         t_treatment_fct3 = relevel(as.factor(t_treatment3), ref="0")) #-1  
         #sampling_weight = ifelse(exposure_groups == 'completed_treatment', 1/0.2, 1))


df <- df %>%
  group_by(census_person_id) %>%
  mutate(min_t_referral = min(t_referral)) %>%

  mutate(not_working_0_temp = ifelse(t_referral == 0, in_work, 0),
         not_working_12_temp = ifelse(t_referral == - 12 , in_work, 0),
         not_working_24_temp = ifelse(t_referral == - 24, in_work, 0),
         not_working_36_temp = ifelse(t_referral == - 36, in_work, 0),
         not_working_48_temp = ifelse(t_referral == - 48, in_work, 0),
         earnings_0_temp = ifelse(t_referral == 0, pay, 0),
         earnings_0 = max(earnings_0_temp),
         not_working_0 = max(not_working_0_temp)) %>% 

  mutate(not_working_12_temp = max(not_working_12_temp),
         not_working_24_temp = max(not_working_24_temp),
         not_working_36_temp = max(not_working_36_temp),
         not_working_48_temp = max(not_working_48_temp)) %>% ungroup()



df <- df %>% 
  group_by(census_person_id) %>%
  mutate(not_working_12 = 
         ifelse(not_working_12_temp == 1, "Working",
                ifelse(not_working_12_temp == 0 & min_t_referral <= -12, "Not working",
                       "Missing"))) %>% 
  mutate(not_working_24 = 
       ifelse(not_working_24_temp == 1, "Working",
              ifelse(not_working_24_temp == 0 & min_t_referral <= -24, "Not working",
                     "Missing"))) %>% 
  mutate(not_working_36 = 
       ifelse(not_working_36_temp == 1, "Working",
              ifelse(not_working_36_temp == 0 & min_t_referral <= -36, "Not working",
                     "Missing"))) %>% 
  mutate(not_working_48 = 
       ifelse(not_working_48_temp == 1, "Working",
              ifelse(not_working_48_temp == 0 & min_t_referral <= -48, "Not working",
                     "Missing"))) %>% ungroup() %>%
  select(-not_working_0_temp, -not_working_12_temp, 
          -not_working_24_temp, -not_working_36_temp, 
          -not_working_48_temp , -earnings_0_temp )


age_df <- df %>%
  filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") %>% 
  # create age bands for sample 
  mutate(age_bands = 
         case_when(age_monthly < 25 ~ "18-24",
                   age_monthly >= 25 & age_monthly < 35 ~ "25-34",
                   age_monthly >= 35 & age_monthly < 45 ~ "35-44",
                   age_monthly >= 45 & age_monthly < 55 ~ "45-54",
                   age_monthly >= 55 ~ "55-69"))
age_df <- age_df %>% select(census_person_id, age_bands)

df <- left_join(df, age_df, by = "census_person_id")
rm(age_df)
gc()


#------------------------------------------------------------------------------------------
### 3. Calculating propensity to be treated (IPWs)
#------------------------------------------------------------------------------------------

df <- df %>% select(-ipw,-ipw1, -ipw2)

# data at time 0
df_0 <- df %>%
  filter(t_referral == 0)

quintiles <- quantile(filter(df_0, earnings_0> 0)$earnings_0, p=c(0.2,0.4,0.6,0.8))

df_0 <- df_0 %>%
  mutate(earnings_0_q =  cut(earnings_0, c(-Inf, 0, quintiles, Inf)))

gc()

#~~~~~~~~~~~~~~~~~~~~Main IPWs - currently being used ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
model_treatment_all <- glm(treatment ~ sex * age_bands + 
                             hh_dependent_children +
                             highest_qualification + 
                             referral_employment_stat +
                             deprived,
                             family=binomial,
                             data = df_0)

df_0$p <- predict(model_treatment_all, df_0, type="response")
df_0$ipw <- ifelse(df_0$treatment == 1,
                    1 /df_0$p,
                    1/(1-df_0$p))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Experimental IPWs 1 
model_treatment_all <- glm(treatment ~ sex * (not_working_0 +
                                              not_working_12 +
                                              not_working_24 +
                                              earnings_0_q) +
                                       ns(month, 5)*Region + 
                                       highest_qualification +
                                       age_bands + 
                                       deprived + 
                                       hh_dependent_children,
                           family=binomial,
                           data = df_0)

tidy(model_treatment_all) %>% print()

df_0$p <- predict(model_treatment_all, df_0, type="response")
df_0$ipw1 <- ifelse(df_0$treatment == 1,
                    1 /df_0$p,
                    1/(1-df_0$p))

# Experimental IPWs 2
model_treatment_all <- glm(treatment ~ sex * ns(age_monthly,5) + 
                                       sex *(not_working_0 +
                                             not_working_12 +
                                             not_working_24 +
                                             earnings_0_q) +
                                       ns(month, 5)*Region + 
                                       highest_qualification +
                                       deprived + 
                                       sourcereferral,
                           family=binomial,
                           data = df_0)



tidy(model_treatment_all) %>% print()

df_0$p <- predict(model_treatment_all, df_0, type="response")
df_0$ipw2 <- ifelse(df_0$treatment == 1,
                    1 /df_0$p,
                    1/(1-df_0$p))

# join to main dataset & apply sampling weight
df <- df %>%
  left_join(select(df_0,  
                   census_person_id,ipw, ipw1, ipw2), by="census_person_id") #%>%
#  mutate(ipw  = ifelse(!is.na(ipw),  ipw*sampling_weight,  ipw),
#         ipw1 = ifelse(!is.na(ipw1), ipw1*sampling_weight, ipw1),
#         ipw2 = ifelse(!is.na(ipw2), ipw2*sampling_weight, ipw2))

rm(df_0, model_treatment_all)
gc()


#------------------------------------------------------------------------------------------
### 3. Regression models
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

fixed_list <- "census_person_id + t_referral"
exposure_list <- "t_treatment_fct3"
control_list <- "+ ns(month,2) + ns(age_monthly,2) + pandemic"


# No weights 

# monthly pay - no IPWs
fixed_effect_function(data = filter(df),
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = '1a_Main_model_earnings_no_ipw',
                      y_aes ="Effect on monthly income",
                      x_aes ="Years since referral",
                      x_lower = -4,
                      x_upper = 5,
                      plot_type="bar",
                      title = "Effects of talking therapies on earnings",
                      vline =0)
gc()

#  Being employed - no IPWs
fixed_effect_function(data = filter(df),
                      outcome = 'in_work',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = '1b_Main_model_employment_no_ipw',
                      y_aes ="Probability of being in work",
                      x_aes ="Years since referral",
                      x_lower = -4,
                      x_upper = 5,
                      plot_type="bar",
                      title = "Effects of talking therapies on employment",
                      vline =0)

gc()

##########################################################################
#                 Model 2a & 2b. Main models with IPWs
# outcome = pay & employment
# exposure/varying vars = time to referral (as a factor) 
# control/varying vars =  control for month & age  & pandemic period
#                          
# fixed effect = time to referral and person 
# se cluster = person
##########################################################################


fixed_effect_function(data = filter(df, !is.na(ipw_rf)),
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight = "ipw_rf",
                      cluster = 'census_person_id',
                      name = '2a_Main_model_earnings_with_ipwRandonForest_',
                      y_aes = "Effect on monthly income",
                      x_aes ="Years since referral",
                      x_lower = -4,
                     x_upper = 5,
                     plot_type = "bar",
                     title = "Effects of talking therapies on earnings with IPWs from randomForest", 
                     vline = 0)
gc()

#  Being employed
fixed_effect_function(data = filter(df, !is.na(ipw_rf)),
                      outcome = 'in_work',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight="ipw_rf",
                      cluster = 'census_person_id',
                      name = '2b_Main_model_employment_with_ipwRandonForest_',
                      y_aes ="Probability of being in work",
                      x_aes ="Years since referral",
                     x_lower = -4,
                     x_upper = 5,
                     plot_type="bar",
                      title = "Effects of talking therapies on employment with IPWs from randomForest",
                     vline =0)

gc()



##########################################################################
#                 Models brokedown by different characteristics
#                         All models below have IPWs
# a. sex 
# b. ethnic_group_tb
# c. country_of_birth_agg
# d. deprived
# e. age_bands
# f. referral_employment_stat
# g. highest_qualification
# h. ns_sec
##########################################################################


# some variable names are too long for graphs so shorten the names

df <- df %>% 
  mutate(referral_employment_stat = 
         ifelse(referral_employment_stat == "Long-term sick/disabled", 
                "Long term sick or disabled",referral_employment_stat), 
                
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
            ns_sec == "Missing or not stated" ~ "Missing or not stated"),
       
  provdiag_short = 
      case_when(provdiag_long == 'Unspecified mental disorder'	~'Unspecified mental disorder',
                provdiag_long =='Mood [affective] disorders'	~'Mood disorders',
                provdiag_long =='Neurotic, stress-related and somatoform disorders'	~ 'Anxiety disorders',
                TRUE ~ 'Other'))



#-------------------------------------------------------------------------
### Sex
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "sex",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by sex: ", 
              plot_type="bar",
              vline = 0)

gc()
#Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "sex",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by sex: ", 
              plot_type="bar",
              vline = 0)

gc()

#-------------------------------------------------------------------------
### ethnic_group_tb
#-------------------------------------------------------------------------

#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "ethnic_group_tb",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by ethnic group ", 
              plot_type="bar",
              vline = 0)
gc()

#  Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "ethnic_group_tb",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by ethnic group", 
              plot_type="bar",
              vline = 0)

gc()
#-------------------------------------------------------------------------
### Country_of_birth_agg
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "country_of_birth_agg",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by country of birth: \n", 
              plot_type="bar",
              vline = 0)
gc()

#  Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "country_of_birth_agg",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by country of birth:\n", 
              plot_type="bar",
              vline = 0)
gc()

#-------------------------------------------------------------------------
### Deprivation
#-------------------------------------------------------------------------

#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "deprived",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by deprivation: ", 
              plot_type="bar",
              vline = 0)
gc()

#  Being employed

regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "deprived",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by deprivation: \n", 
              plot_type="bar",
              vline = 0)


gc()

#-------------------------------------------------------------------------
### Age bands
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "age_bands",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by age bands: ", 
              plot_type="bar",
              vline = 0)
gc()
#  Being employed

regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "age_bands",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by age bands: \n", 
              plot_type="bar",
              vline = 0)

gc()


#-------------------------------------------------------------------------
### Employment stat. at referral 
#-------------------------------------------------------------------------

#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "referral_employment_stat",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by employment status: ", 
              plot_type="bar",
              vline = 0)
gc()
#  Being employed

regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "referral_employment_stat",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by employment status:\n", 
              plot_type="bar",
              vline = 0)

gc()

#-------------------------------------------------------------------------
### Highest qualification
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "highest_qualification",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by quilifications: ", 
              plot_type="bar",
              vline = 0)
gc()
#Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "highest_qualification",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by quilifications:\n", 
              plot_type="bar",
              vline = 0)
gc()

#-------------------------------------------------------------------------
### NS-SEC
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "ns_sec",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by NS-SEC: \n", 
              plot_type="bar",
              vline = 0)
gc()
#Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "ns_sec",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by NS-SEC:\n", 
              plot_type="bar",
              vline = 0)

gc()

#-------------------------------------------------------------------------
### Provisional diagnosis - short 
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "provdiag_short",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by provisional diagnosis: \n ", 
              plot_type="bar",
              vline = 0)
gc()
#Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "provdiag_short",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by provisional diagnosis: \n", 
              plot_type="bar",
              vline = 0)

gc()


#-------------------------------------------------------------------------
### hh_dependent_children
#-------------------------------------------------------------------------
#Earnings
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "hh_dependent_children",
              outcome = 'pay',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Effect on monthly income",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Effects on earnings by number of children: \n ", 
              plot_type="bar",
              vline = 0)
gc()
#Being employed
regression_with_breakdowns_function(data = filter(df, !is.na(ipw)),
              category = "hh_dependent_children",
              outcome = 'in_work',
              exposure_var = exposure_list,
              control_vars = control_list,
              fixed_effect_vars = fixed_list,
              weight = "ipw",
              cluster = 'census_person_id',
              y_aes ="Probability of being in work",
              x_aes ="Years since referral",
              x_lower = -4,
              x_upper = 5,
              title = "Probability of being employed by number of children: \n", 
              plot_type="bar",
              vline = 0)

gc()



























































############################################################################################
#                                   SENSITIVITY ANALYSIS
############################################################################################

#------------------------------------------------------------------------------------------
### Join datasets for SENSITIVITY ANALYSIS
#------------------------------------------------------------------------------------------

df_sensitivity_analysis <- bind_rows(completed_sample, started_sample, left_before_sample)


df_sensitivity_analysis <- df_sensitivity_analysis %>%
  mutate(in_work = ifelse(pay < 0 | pay > 0, 1,0),
         treatment = ifelse(exposure_groups == 'completed_treatment', 1, 0),
         t_treatment = ceiling(t_last_therapy/12),
         #t_treatment = ceiling(t_first_therapy/12),
         t_treatment = ifelse(is.na(t_treatment),0,  t_treatment),
         t_referral_pos= ifelse(t_referral < 0, 0, t_referral),
         #t_treatment_pos= ifelse(t_treatment < 0, 0, t_treatment),
         t_treatment_fct = relevel(as.factor(t_treatment), ref="0"),      
         
         post_treatment =  ifelse(treatment == 1 & t_treatment > 0,1,0),
    # set negative payments to 0
         pay = case_when(pay < 0 ~ 0, 
                       pay > q[[2]]~ q[[2]],
                       TRUE ~ pay)) 
        
df_sensitivity_analysis <- df_sensitivity_analysis %>%
mutate(treatment_stage = 
       case_when(t_referral >= 0 &
                 (t_first_therapy <= 0| is.na(t_first_therapy)) ~ "Referred",
                 #t_referral < -36 ~ "Before referral : year 4+",
                 #t_referral >= -36 & t_referral < -24  ~ "Before referral : year 3",
                 #t_referral < -12 & t_referral >= -24 ~ "Before referral : year 2",
                 #t_referral <0 & t_referral >= -12 ~ "Before referral : year 1",
                 t_first_therapy >=1 & (t_last_therapy <1 | is.na(t_last_therapy))   ~ "Being treated",
                 t_last_therapy >=1 &  t_last_therapy < 12 ~ "Completed - year 1",
                 t_last_therapy >=12 &  t_last_therapy < 24 ~ "Completed - year 2",
                 t_last_therapy >=24 &  t_last_therapy < 36 ~ "Completed - year 3",
                 t_last_therapy >=36 &  t_last_therapy < 48 ~ "Completed - year 4",
                 t_last_therapy >=48  ~ "Completed - year 5+",
                 TRUE ~ "Before referral"),
       treatment_stage = relevel(as.factor(treatment_stage), ref="Referred"))

#------------------------------------------------------------------------------------------
## calculating propensity to be treated
#------------------------------------------------------------------------------------------


# data at time 0
df_0 <- df_sensitivity_analysis %>%
filter(t_referral ==0)

# model treatment completion

model_treatment <- glm(treatment ~ sex *( ns(age_monthly,5) + ethnic_group_tb + highest_qualification + unemployed_12_month_before) , 
                       family=binomial,
                      data =df_0)
summary(model_treatment)
gc()

# predict the probability to complete the treatment
df_0$p <- predict(model_treatment, df_0, type="response")

# calculate the IPW
df_0$ipw <- ifelse(df_0$treatment == 1,1 / df_0$p, 1/(1-df_0$p ))



ggplot(data=df_0) + aes(x=p, fill=as.factor(treatment))+
geom_histogram()

# join to main dataset
df_sensitivity_analysis <- df_sensitivity_analysis %>%
left_join(select(df_0,  
                 census_person_id,  ipw), by="census_person_id")


rm(df_0, model_treatment)
gc()

##########################################################################
#                 Model SA1. SENSITIVITY ANALYSIS
# outcome = pay
# exposure/varying vars = time to treatment (as a factor) 
# control/varying vars =  control for month & age  
#                          
# fixed effect = time to referral and person 
# se cluster = person
##########################################################################

fixed_list <- "census_person_id + t_referral"
exposure_list <- "t_treatment_fct"
control_list <- "+ ns(month,2) + ns(age_monthly,2)"

fixed_effect_function(data = df_sensitivity_analysis,
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = 'SA1a_model_earnings_adjusted',
                      y_aes ="Effect on monthly income",
                      x_aes ="Years since end of treatment")



##########################################################################
#              Model SA2- Same as above but with weights
##########################################################################

#with inverse probability weighting (ipw)
fixed_effect_function(data = df_sensitivity_analysis,
                      outcome = 'pay',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight="ipw",
                      cluster = 'census_person_id',
                      name = 'SA1b_model_earnings_adjusted_with_weights',
                      y_aes ="Effect on monthly income",
                      x_aes ="Years since end of treatment")



##########################################################################
#              Model 3a. Probability of being in work
# Conditional logistic regression 
#
# outcome = in_work
# exposure/varying vars = time to treatment (as a factor) 
# control/varying vars =  control for month & age  
#                          

##########################################################################


fixed_list <- "census_person_id + t_referral"
exposure_list <- "t_treatment_fct"
control_list <- "+ ns(month,2) + ns(age_monthly,2)"


#with inverse probability weighting (ipw)
fixed_effect_function(data = df_sensitivity_analysis,
                      outcome = 'in_work',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      cluster = 'census_person_id',
                      name = 'SA2a_model_in_work_adjusted',
                      y_aes ="Effect on monthly income",
                      x_aes ="Years since end of treatment")


##########################################################################
#              Model 3b - Same as above but with weights
##########################################################################


#with inverse probability weighting (ipw)
fixed_effect_function(data = df_sensitivity_analysis,
                      outcome = 'in_work',
                      exposure_var = exposure_list,
                      control_vars = control_list,
                      fixed_effect_vars = fixed_list,
                      weight="ipw",
                      cluster = 'census_person_id',
                      name = 'SA2b_model_in_work_adjusted_with_weights',
                      y_aes ="Effect on monthly income",
                      x_aes ="Years since end of treatment")
