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

df$pay_monthly <- df$pay/3 
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
df <- df %>% left_join(select(df_0,census_person_id,ipw), by="census_person_id")
rm(df_0, model_treatment_all)

#------------------------------------------------------------------------------------------
### 4. Create a pay with IPWs variable (monthly)
#------------------------------------------------------------------------------------------

df <- df %>% mutate(pay_ipw = pay_monthly*ipw,
                    employment_ipw = in_work*ipw)


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
### 5. Summary statistics on pay and employment  
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

### Average pay (monthly and quarterly), employment (%) and monthly pay with IPWs by: 

# 1. Time to first therapy: 

summary_1 <- df %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(t_first_therapy) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>% 
  arrange(t_first_therapy)  

# 2. Time to first therapy broken down by exposure groups:


summary_2 <- df %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(t_first_therapy, exposure_groups) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>% 
  arrange(t_first_therapy,exposure_groups)  

# 3. Calendar time (quarter and year):

summary_3 <- df %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(reference_date) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>%
  arrange(reference_date)  

# 4. Calendar time (quarter and year) broken down by exposure groups:

summary_4 <- df %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(reference_date,exposure_groups) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>%
  arrange(reference_date,exposure_groups)  


### Graphs: 
# pivot data for easier plotting
temp_pivot <- summary_2 %>% 
  select(t_first_therapy, 
         exposure_groups, 
         average_monthly_pay,
         average_M_pay_IPW) %>%
  pivot_longer(cols= c(average_monthly_pay, average_M_pay_IPW),
               names_to="pay_type",
               values_to ="pay_value")


vars_n <- c('average_monthly_pay' = 'Pay without IPWs',
           'average_M_pay_IPW' = 'Pay with IPWs')

plot_1_pay_ipw <- temp_pivot %>% 
  ggplot(aes(x = t_first_therapy, 
             y = pay_value, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(x ="Time to first therapy in quarters", 
       y = "Average monthly pay (£)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(pay_type~., labeller =as_labeller(vars_n))+ 
  scale_color_discrete(labels = c('Completed treatment', 'Dropped out of treatment'), 
                     name = "Exposure group:")


plot_1_pay_ipw

ggsave(paste0(outputs_folder,"aim1_pay_with_and_without_ipws_",Sys.Date(),".pdf"))

####################################################################################
####################################################################################
################# Sub-analysis- recovered vs not recovered #########################
####################################################################################
####################################################################################

# sub analysis 
df1 <- df %>% select(-exposure_groups,
                     -ipw, 
                     -pay_ipw,
                     -employment_ipw,
                     -treatment, 
                     -t_treatment) %>%
  filter(treatment_outcome == 'Improvement or recovery' |
         treatment_outcome == 'No change or deterioration') %>%
  mutate(exposure_groups = treatment_outcome,
         treatment = ifelse(exposure_groups == 'Improvement or recovery', 1, 0),
         t_treatment = ceiling(t_first_therapy *  treatment/4))

#------------------------------------------------------------------------------------------
### 6. Calculate Inverse Probability Weights(IPWs) for the recovered vs not-recovered
#------------------------------------------------------------------------------------------

## data at time 0
df_0 <- df1 %>% filter(t_first_therapy == 0)

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
df1 <- df1 %>% left_join(select(df_0, census_person_id, ipw), by="census_person_id")
rm(df_0, model_treatment_all)

#------------------------------------------------------------------------------------------
### 7. Create a pay with IPWs variable (monthly)
#------------------------------------------------------------------------------------------

df1 <- df1 %>% mutate(pay_ipw = pay_monthly*ipw,
                    employment_ipw = in_work*ipw)


#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------
### 8. Summary statistics on pay and employment for the recovered vs not-recovered
#------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------

summary_5 <- df1 %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(t_first_therapy) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>% 
  arrange(t_first_therapy)  

summary_6 <- df1 %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(t_first_therapy, exposure_groups) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>% 
  arrange(t_first_therapy,exposure_groups)  

summary_7 <- df1 %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(reference_date) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>%
  arrange(reference_date)  

summary_8 <- df1 %>% 
  mutate(pay = as.numeric(pay)) %>%
  group_by(reference_date,exposure_groups) %>% 
  summarise(average_monthly_pay = mean(pay_monthly),
            average_M_pay_IPW = sum(pay_ipw) / sum(ipw),
            average_monthly_pay_in_work = mean(pay_monthly[in_work==1]),
            average_quarterly_pay = mean(pay),
            average_qtr_pay_in_work = mean(pay[in_work==1]) ,
            count_of_people = n(),
            in_work_count= sum(in_work)) %>%
  mutate(employment_percentage = 
         in_work_count/count_of_people*100) %>%
  arrange(reference_date,exposure_groups) 


### Graphs: 
# pivot data for easier plotting
temp_pivot2 <- summary_6 %>% 
  select(t_first_therapy, 
         exposure_groups, 
         average_monthly_pay,
         average_M_pay_IPW) %>%
  pivot_longer(cols= c(average_monthly_pay, average_M_pay_IPW),
               names_to="pay_type",
               values_to ="pay_value")

plot_2_pay_ipw <- temp_pivot2 %>% 
  ggplot(aes(x = t_first_therapy, 
             y = pay_value, 
             colour = exposure_groups,
             group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(x ="Time to first therapy in quarters", 
       y = "Average monthly pay (£)") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  facet_grid(pay_type~., labeller =as_labeller(vars_n))+ 
  scale_color_discrete(labels = c('Improvement or recovery','No change or deterioration'), 
                     name = "Exposure group:")


plot_2_pay_ipw

ggsave(paste0(outputs_folder,"aim2_pay_with_and_without_ipws_",Sys.Date(),".pdf"))


#------------------------------------------------------------------------------------------
### 9. Save all data in an Excel file
#------------------------------------------------------------------------------------------

#--- save all 8 results for linked_data --------#
dataset_names <- list('Time2_main_all'= summary_1,
                      'Time2_main_expGgrps'= summary_2,
                      'graph_pay_pay_with_ipws_aim1' = temp_pivot,
                      'Quarters_main_all' = summary_3,
                      'Quarters_main_expGrp' = summary_4,
                      
                      'Time2_sub_all'= summary_5,                    
                      'Time2_sub_exp_grps'= summary_6,
                      'graph_pay_pay_with_ipws_aim2' = temp_pivot2,
                      'Quarters_sub_all' = summary_7,
                      'Quarters_sub_expGrp' = summary_8)

write.xlsx(dataset_names, 
           paste0(outputs_folder,"5_Pay_employment_summary_stats_inc_ipws_", Sys.Date(), ".xlsx"))


#------------------------------------------------------------------------------------------
### 10. End of the script
#------------------------------------------------------------------------------------------
