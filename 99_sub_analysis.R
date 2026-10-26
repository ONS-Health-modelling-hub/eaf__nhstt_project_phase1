#In this subanalysis we change our exposure groups to recovered vs not recovered

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
                                             outputs_folder,
                                             hdfs)
} 

# Combine all dataframes into one big dataframe 

df <- do.call(rbind, df_list) 
rm(df_list)

df <- df %>%  mutate(
  qtr_date_format = as.Date(paste0(reference_date,"-01")))

df$qtr <- substr(df$reference_date,6,6)
#------------------------------------------------------------------------------------------
### 1. Load treatment outcomes variable
#------------------------------------------------------------------------------------------


treat_out <- readRDS(paste0(outputs_folder, "treatment_outcomes.RDS"))
appts <- readRDS(paste0(outputs_folder, 'number_appointments_religion_marital_stat.RDS'))

# The 'Missing or not stated' category in ns_sec and qualifications is 2 so we remove it

df <- df %>% 
  filter(ns_sec != 'Missing or not stated') 

df <- left_join(df,treat_out, by="census_person_id") %>% 
      filter(treatment_outcomes != 'not completed treatment/missing scores') %>%
      left_join(.,appts, by="census_person_id")

# check counts
df %>% filter(t_referral ==0) %>%
group_by(ic_count_treatment_appointments) %>% 
summarise(count=n()) %>%
arrange(ic_count_treatment_appointments) %>%
head(100) %>% print()

#------------------------------------------------------------------------------------------
### 2. Add additional variables
#------------------------------------------------------------------------------------------

# Add quantiles
q = quantile(df$pay, prob=c(0.01, 0.99))

df <- df %>% select(-exposure_groups) %>%
  mutate(
   
   # Create in work variable based on quarterly pay
  in_work = ifelse(pay > 0, 1, 0),
  pay = case_when(pay < 0 ~ 0, 
                   pay > q[[2]]~ q[[2]],
                   TRUE ~ pay),
  exposure_groups = 
    ifelse(treatment_outcomes == 'improvement' | 
           treatment_outcomes == 'recovery', 'Improvement or recovery' ,
           'No change or deterioration'),
    
  treatment = ifelse(exposure_groups == 'Improvement or recovery', 1, 0),
  t_treatment = ceiling(t_first_therapy *  treatment/4),
  t_treatment_fct = relevel(as.factor(t_treatment), ref="0"))

df$pay <- df$pay/3 
rm(q)

# There are no appointments of less than 2 in this dataset - everyone should have assessment + 1st therapy session
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

#------------------------------------------------------------------------------------------
# graph - general mean income by tretament group
#------------------------------------------------------------------------------------------

### GRAPH 1 - 
temp_df_1 <- df %>% 
              mutate(pay = as.numeric(pay)) %>%
              group_by(reference_date, exposure_groups) %>% 
              summarise(average_monthly_pay = mean(pay)) %>% arrange(reference_date)
  
# plot graph
g1 <- temp_df_1 %>% 
  ggplot(aes(x = reference_date, 
             y = average_monthly_pay, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Quarterly average pay broken down by treatment outcomes",
       x ="Date", 
       y = "Average quarterly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')



### GRAPH 2 - 
temp_df_2 <- df %>% 
              mutate(pay = as.numeric(pay)) %>%
              group_by(reference_date, exposure_groups,sex) %>% 
              summarise(average_monthly_pay = mean(pay)) %>% arrange(reference_date)
  
# plot graph
g2 <- temp_df_2 %>% 
  ggplot(aes(x = reference_date, 
             y = average_monthly_pay, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Quarterly average pay broken down by treatment outcomes",
       x ="Date", 
       y = "Average quarterly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:') +
  facet_wrap(~sex)


### GRAPH 3 - 
temp_df_3 <- df %>% 
              mutate(pay = as.numeric(pay)) %>%
              group_by(t_referral, exposure_groups) %>% 
              summarise(average_monthly_pay = mean(pay))  
# plot graph
g3 <- temp_df_3 %>% 
  ggplot(aes(x = t_referral, 
             y = average_monthly_pay, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Quarterly average pay broken down by treatment outcomes",
       x ="Time to first therapy in quarters", 
       y = "Average quarterly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')

### GRAPH 4 - 
temp_df_4 <- df %>% 
              mutate(pay = as.numeric(pay)) %>%
              group_by(t_referral, exposure_groups,sex) %>% 
              summarise(average_monthly_pay = mean(pay))  
# plot graph
g4 <- temp_df_4 %>% 
  ggplot(aes(x = t_referral, 
             y = average_monthly_pay, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Quarterly average pay broken down by treatment outcomes",
       x ="Time to first therapy in quarters", 
       y = "Average quarterly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')+
  facet_wrap(~sex)


#--- save all 8 results for linked_data --------#
dataset_names <- list('pay_quarter_treatmentg' = temp_df_1, 
                      'pay_quarter_treatmentg_sex' = temp_df_2,
                      'pay_by_treatment_therapy' = temp_df_3,
                      'pay_by_treatment_therapy_sex' = temp_df_4)

write.xlsx(dataset_names, 
           paste0(outputs_folder,"Sub_analysis3_seperate_graphs_data_", Sys.Date(), ".xlsx"))


# save all plots in PDF file
pdf(paste0(outputs_folder,"Sub_analysis3_seperate_graphs_", Sys.Date(), ".pdf"), 
                       width = 10, height = 4)
print(g1)
print(g2)
print(g3)
print(g4)

dev.off()



#------------------------------------------------------------------------------------------
### 3. Calculate Inverse Probability Weights (IPWs)
#------------------------------------------------------------------------------------------
df <- df %>% select(-ipw)

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
                   census_person_id,ipw), by="census_person_id")

rm(df_0, model_treatment_all)
gc()
#------------------------------------------------------------------------------------------
# 3. Regression 
#------------------------------------------------------------------------------------------

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
                      name = '2a_sub_analysis_work_',
                      y_aes ="Effect (£)",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                     x_upper = 7,
                      min = -30, 
                      max = 60,
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
                      name = '2b_sub_analysis3_pay_',
                      y_aes ="Effect (%)",
                      x_aes ="Years since first therapy",
                     x_lower = -4,
                     x_upper = 7,
                      min = -0.5, 
                      max = 3.5,
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
                      name = '2c_sub_analysis_earnings_IPW_IN_EMPLOYMENT_',
                      y_aes ="Effect (£)",
                      x_aes ="Years since first therapy",
                      x_lower = -4,
                     x_upper = 7,
                      min = -30, 
                      max = 60,
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
#Earnings
regression_with_breakdowns_function(data = filter(df),
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



#Employment
regression_with_breakdowns_function(data = filter(df),
              category = "number_appointments",
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

###########################################################################################
###########################################################################################
###########################################################################################
###########################################################################################

df <- left_join(df,treat_out, by="census_person_id") %>% 
      left_join(.,appts, by="census_person_id")

# check counts
df %>% filter(t_referral ==0) %>%
group_by(ic_count_treatment_appointments) %>% 
summarise(count=n()) %>%
arrange(ic_count_treatment_appointments) %>%
head(100) %>% print()

#------------------------------------------------------------------------------------------
### 2. Add additional variables
#------------------------------------------------------------------------------------------

# Add quantiles
q = quantile(df$pay, prob=c(0.01, 0.99))

df <- df %>% select(-exposure_groups) %>%
  mutate(
   
   # Create in work variable based on quarterly pay
  in_work = ifelse(pay > 0, 1, 0),
  pay = case_when(pay < 0 ~ 0, 
                   pay > q[[2]]~ q[[2]],
                   TRUE ~ pay),
  exposure_groups = 
    ifelse(ic_count_treatment_appointments <= 4, 
           'Less or equal to 4',
           'More than 4'),
    
  treatment = ifelse(exposure_groups == 'More than 4', 1, 0),
  t_treatment = ceiling(t_first_therapy *  treatment/4),
  t_treatment_fct = relevel(as.factor(t_treatment), ref="0"))

df$pay <- df$pay/3 
rm(q)













#
##########################################
##regression breakdowns
#df <- df %>% select(-ipw)
#
##Earnings
#regression_with_breakdowns_function(data = filter(df),
#              category = "treatment_outcome",
#              outcome = 'pay',
#              exposure_var = exposure_list,
#              control_vars = control_list,
#              fixed_effect_vars = fixed_list,
#              weight = "ipw",
#              cluster = 'census_person_id',
#              y_aes ="Effect",
#              x_aes ="Years since first therapy",
#              x_lower = -4,
#              x_upper = 7,
#              title = "Effects on earnings by treatment outcomes", 
#              plot_type="bar",
#              vline = 0,
#             ipw_vars = "referral_employment_stat + 
#                           ns(age_at_referral,df=5,
#                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
#                          therapy_intensity + 
#                           first_medication +
#                           sourcereferral+
#                           Region +
#                           deprived +
#                           highest_qualification+
#                           hh_dependent_children+
#                           ns_sec")
#
#gc()
#
##Employment
#regression_with_breakdowns_function(data = filter(df),
#              category = "treatment_outcome",
#              outcome = 'in_work',
#              exposure_var = exposure_list,
#              control_vars = control_list,
#              fixed_effect_vars = fixed_list,
#              weight = "ipw",
#              cluster = 'census_person_id',
#              y_aes ="Effect",
#              x_aes ="Years since first therapy",
#              x_lower = -4,
#              x_upper = 7,
#              title = "Effects on probability of employment by treatment outcomes", 
#              plot_type="bar",
#              vline = 0,
#             ipw_vars = "referral_employment_stat + 
#                           ns(age_at_referral,df=5,
#                              Boundary.knots = quantile(age_at_referral,c(.10,.90))) +
#                          therapy_intensity + 
#                           first_medication +
#                           sourcereferral+
#                           Region +
#                           deprived +
#                           highest_qualification+
#                           hh_dependent_children+
#                           ns_sec")
#
#gc()
