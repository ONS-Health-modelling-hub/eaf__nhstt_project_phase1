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
xl_config$spark.dynamicAllocation.maxExecutors <- 12
xl_config$spark.sql.shuffle.partitions <- 240
xl_config$spark.sql.session.timeZone = "UTC+01:00"
xl_config$spark.sql.legacy.timeParserPolicy = "LEGACY"
xl_config$spark.sql.parquet.datetimeRebaseModeInRead = "CORRECTED"


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
                                FROM project_name.",final_linked_dataset,"")) 

#------------------------------------------------------------------------------------------
### 2. Quarterly data
#------------------------------------------------------------------------------------------

### POPULATION FLOW CHART
temp_df <- df %>% filter(t_referral == 0)
sample_flow <- tibble(stage = "Final data for treated & non-treated group",
                      count = sdf_nrow(temp_df),
                      description = "Data from previous monthly flow chart")


df <- df %>%
  mutate(t_0 = ifelse(age_at_referral >= 25,1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = max(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)


df <- df %>%
  mutate(t_0 = ifelse(age_at_referral <= 60 ,1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = max(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)


df <- df %>%
    filter(age_monthly >= 21 &  age_monthly <= 64) 


### POPULATION FLOW CHART
temp_df <- df %>% filter(t_referral == 0)
sample_flow <- sample_flow %>% dplyr::bind_rows(
    tibble(stage = "Only participants aged 25 to 60 at referral",
                      count = sdf_nrow(temp_df),
                      description = "Follow up to 21 and 64"))

  
df <- df %>%
    filter(referral_employment_stat != "Retired") %>%
    filter(Region != "Wales")

### POPULATION FLOW CHART
temp_df <- df %>% filter(t_referral == 0)
sample_flow <- sample_flow %>% dplyr::bind_rows(
    tibble(stage = "Removed participants who were retired at the time of referral or were in Wales at the time of Census",
                      count = sdf_nrow(temp_df),
                      description = "Also removed individuals with Missing category in nssec as they were disclosive"))

rm(temp_df)

df <- df %>%
 mutate(reference_quarter = quarter(month),
        reference_year = year(month),
        reference_date = paste0(reference_year,"-", reference_quarter),
        
      
        referral_quarter = quarter(referral_date),
        referral_year = year(referral_date),
        referral_date =paste0(referral_year,"-", referral_quarter),

        
        first_therapy_quarter = quarter(first_therapy_date),
        first_therapy_year = year(first_therapy_date),
        first_therapy_date = paste0(first_therapy_year,"-", first_therapy_quarter ),
        
        last_therapy_quarter = quarter(last_therapy_date),
        last_therapy_year = year(last_therapy_date),
        last_therapy_date = paste0(last_therapy_year,"-", last_therapy_quarter),
        
        t_referral = (reference_year - referral_year)* 4 + 
                      (reference_quarter - referral_quarter),
       
       t_first_therapy = (reference_year - first_therapy_year)* 4 + 
                         (reference_quarter - first_therapy_quarter),
       
       t_last_therapy = (reference_year - last_therapy_year)* 4 + 
                        (reference_quarter - last_therapy_quarter)) %>%

  # remove monthly variables
  select(-cpih_deflator_2023,
         -age_monthly,
         -month,
         -t_last_appointment,
         -month_label,
         -ref_date,
         -age_at_referral) 

# calculate quarterly pay by summing the monthly payments 
df <- df %>%
  group_by(reference_date, census_person_id) %>%
  mutate(pay = sum(pay),
         original_pay = sum(original_pay),
         pay_no_deflator = sum(pay_no_deflator)) %>% 
  sdf_drop_duplicates()


df <- df %>%
  mutate(t_0 = ifelse(t_referral == 0,1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = sum(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)

df <- df %>%
  mutate(t_0 = ifelse(t_first_therapy == 0,1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = sum(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)

df <- df %>% mutate(
  dob_year = as.numeric(substring(dob_quarter,1,4)),
  dob_qtr = as.numeric(substring(dob_quarter,6,7)), 

  ref_year = as.numeric(substring(reference_date,1,4)),
  ref_qtr = as.numeric(substring(reference_date,6,6)),

  t_birthday_qtr = (ref_year - dob_year)* 4 + 
                   (ref_qtr - dob_qtr), 
  quarterly_age = t_birthday_qtr/4,
  quarterly_age = floor(quarterly_age),
  quarterly_age = ifelse(quarterly_age == 20, 21,quarterly_age), 
  quarterly_age = ifelse(quarterly_age == 70, 69,quarterly_age), 


  qtr_date_format = as.Date(paste0(ref_year,"-0", ref_qtr,"-01"))) %>% 
  select(-c("dob_qtr","dob_year","t_birthday_qtr","ref_year","ref_qtr"))


# age_at_referral_qtr 
df <- df %>%
    group_by(census_person_id) %>%
    mutate(age_at_referral = ifelse(t_referral == 0, quarterly_age, 0),
           age_at_referral = max(age_at_referral)) %>%
    ungroup() %>% mutate(
    age_at_referral = ifelse(age_at_referral == 24, 25,age_at_referral),
    age_at_referral = ifelse(age_at_referral == 61, 60,age_at_referral),

    age_bands = 
             case_when(
                       age_at_referral >= 25 & age_at_referral < 35 ~ "25-34",
                       age_at_referral >= 35 & age_at_referral < 45 ~ "35-44",
                       age_at_referral >= 45 & age_at_referral < 55 ~ "45-54",
                       age_at_referral >= 55 ~ "55-60"))


### POPULATION FLOW CHART
temp_df <- df %>% filter(t_referral == 0)
sample_flow <- sample_flow %>% dplyr::bind_rows(
    tibble(stage = "Final quarterly population",
                      count = sdf_nrow(temp_df),
                      description = "Min of 2 years prior follow up, 25-60"))

rm(temp_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # check for duplicates
  df %>%
  group_by(census_person_id, reference_date) %>%
  summarize(count = n()) %>% 
  mutate(duplicate_flag = ifelse(count > 1,1,0)) %>% ungroup() %>%
  summarise(count_of_duplicates = sum(duplicate_flag)) 

  df %>% arrange(census_person_id,reference_date) %>% head(300) %>% collect() %>% print()
  
  df %>% sdf_nrow()
  df %>% sdf_distinct(census_person_id) %>% sdf_nrow()  

#--------------------------------------------------------------------------
# Save final quarterly data
#--------------------------------------------------------------------------
  
sql <- paste0("DROP TABLE IF EXISTS project_name.",quarterly_df, "")
DBI::dbExecute(sc, sql)

# Save temp data for checks 
sdf_register(df , 'df')
sql <- paste0("CREATE TABLE project_name.",quarterly_df, " AS SELECT * FROM df")
DBI::dbExecute(sc, sql)




# save sample flow 
write.csv(sample_flow, 
        paste0(outputs_folder, "Population_flow_chart_2_",Sys.Date(), ".csv"))


#--------------------------------------------------------------------------
# Add quantiles and summary graphs 
#--------------------------------------------------------------------------
quantiles <- sdf_quantile(df, "pay", probabilities = c(0.01,0.99))
q99 <- quantiles[[2]]

df <- df %>% mutate(pay = ifelse(pay > q99, q99, pay),
                    in_work = ifelse(pay > 0, 1, 0),
                    treatment = ifelse(exposure_groups == 'completed_treatment', 1, 0))

#------------------------------------------------------------------------------------------
### Descriptives and summary stats - whole population
#------------------------------------------------------------------------------------------
                      
# Exposure groups counts & proportions 

res1 <- proportion_function(df, exposure_groups)
gc()

# Distribution of age & Age bands counts & proportions
res2 <- proportion_function(df, age_at_referral) %>% arrange(age_at_referral)
res3 <- proportion_function(df, age_bands) %>% arrange(age_bands)
gc()

#  Min, max and average
res4a <- min_max_mean(df, age_at_referral)
res4b <- min_max_mean(df, pay)
res4c <- min_max_mean_grouped(df, pay, sex)
res4d <- min_max_mean_grouped(df, age_at_referral, sex)
res4dd <- min_max_mean_grouped(df, age_at_referral, exposure_groups)
res4e <- min_max_mean_2_grouped(df, pay, sex, exposure_groups)
res4f <- min_max_mean_2_grouped(df, age_at_referral, sex, exposure_groups)
gc()

# CATEGORICAL VARIABLES - COUNTS & PROPORTIONS & BY EXPOSURE GROUPS
res5 <- proportion_for_var_list (df, argument_list)
res6 <- proportion_for_exposure_by_var_list(df, argument_list)
gc()

# Results showing proportions e.g. of exposure groups who are Males 
res7 <- proportion_for_var_by_exposure(df, argument_list)
gc()

# Results in wide format 
wide_table <- res6 %>% 
  pivot_wider(names_from = exposure_groups,
              values_from = c(Count, total, percentage)) %>%
  select(-contains("total_")) %>% 
  arrange(Variable_names) %>%
  mutate(Completed_treatment = 
          paste(Count_completed_treatment, 
                " (", round(percentage_completed_treatment, 1), "%)",
                sep = ""),
         Dropped_out_of_treatment = 
          paste(Count_dropped_out_of_treatment, 
                " (", round(percentage_dropped_out_of_treatment, 1), "%)", 
                sep = "")) %>%
  select(Variable_names, Category_name, Completed_treatment, Dropped_out_of_treatment) %>% 
  rename(Characteristic = Variable_names,
         Category = Category_name,
         'Completed treatment' = Completed_treatment,
         'Dropped out of treatment' = Dropped_out_of_treatment)
          
gc()

# Fromatted results with proportions for whole df
proportions_formatted <- res5 %>%
  arrange(Variable_names) %>%
  mutate(Count_percentage = paste(Count, " (", round(percentage, 1), "%)", sep = "")) %>%
  rename(Characteristic = Variable_names,
         Category = Category_name,
         'Count(%)' = Count_percentage) %>%
               select(Characteristic, Category, 'Count(%)')
gc()


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


df <- left_join(df, df_pre, by="census_person_id") %>%
  left_join(., df_post, by="census_person_id")


res8a <- df %>%
  summarise(min_t_therapy_Follow_Up_all = min(t_first_therapy),
            max_t_therapy_Follow_Up_all = max(t_first_therapy),
            
            pre_min_t_therapy_Follow_Up = max(pre_follow_up_limit),
            pre_max_t_therapy_Follow_Up = min(pre_follow_up_limit),
            pre_average_therapy_Follow_Up = mean(pre_follow_up_limit),
            
            
            post_min_t_therapy_Follow_Up = min(post_follow_up_limit),
            post_max_t_therapy_Follow_Up = max(post_follow_up_limit),
            post_average_therapy_Follow_Up = mean(post_follow_up_limit))



res8b <- df %>%
group_by(exposure_groups) %>%
  summarise(        
            pre_min_t_therapy_Follow_Up = max(pre_follow_up_limit),
            pre_max_t_therapy_Follow_Up = min(pre_follow_up_limit),
            pre_average_therapy_Follow_Up = mean(pre_follow_up_limit),
            
            post_min_t_therapy_Follow_Up = min(post_follow_up_limit),
            post_max_t_therapy_Follow_Up = max(post_follow_up_limit),
            post_average_therapy_Follow_Up = mean(post_follow_up_limit))


res8c <- df %>%
group_by(age_bands) %>%
  summarise(        
            pre_min_t_therapy_Follow_Up = max(pre_follow_up_limit),
            pre_max_t_therapy_Follow_Up = min(pre_follow_up_limit),
            pre_average_therapy_Follow_Up = mean(pre_follow_up_limit),
            
            post_min_t_therapy_Follow_Up = min(post_follow_up_limit),
            post_max_t_therapy_Follow_Up = max(post_follow_up_limit),
            post_average_therapy_Follow_Up = mean(post_follow_up_limit)) %>% 
  arrange(age_bands)


resA <- df %>% 
group_by(exposure_groups,treatment_outcome) %>%
summarise(mean= mean( ic_count_treatment_appointments ,na.rm=TRUE),
          median = median(ic_count_treatment_appointments ,na.rm=TRUE),
          min = min(ic_count_treatment_appointments ,na.rm=TRUE), 
         max = max(ic_count_treatment_appointments ,na.rm=TRUE)) 

resB <- df %>% 
group_by(exposure_groups) %>%
summarise(mean= mean( ic_count_treatment_appointments ,na.rm=TRUE),
          median = median(ic_count_treatment_appointments ,na.rm=TRUE),
          min = min(ic_count_treatment_appointments ,na.rm=TRUE), 
         max = max(ic_count_treatment_appointments ,na.rm=TRUE))



##### some stats on the date of the first therapy

total_all  <- df %>% filter(t_first_therapy == 0) %>% sdf_nrow()
total_comp <- df %>% filter(t_first_therapy == 0 & 
                            exposure_groups == 'completed_treatment') %>% sdf_nrow()
total_drop <- df %>% filter(t_first_therapy == 0 &
                            exposure_groups == 'dropped_out_of_treatment') %>% sdf_nrow()

### QUARTERLY
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




### MONTHLY

df1 <- df %>% mutate(pay = pay/3)
rm(q)

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



### EMPLOYMENT

employment_on_therapy1 <- df %>%
  filter(t_first_therapy == 0) %>%
  group_by(in_work) %>% 
  summarise(exposure_groups ="All",
            n =n(),
            total = total_all) %>% 
  mutate(Percentage = (n/total)*100) 
  

employment_on_therapy2 <- df %>% 
  filter(t_first_therapy == 0 & 
         exposure_groups == 'completed_treatment') %>%
  group_by(exposure_groups,in_work) %>%
  summarise(n =n(),
            total = total_comp) %>% 
  mutate(Percentage = (n/total)*100) 

employment_on_therapy3 <- df %>% 
  filter(t_first_therapy == 0 & 
         exposure_groups == 'dropped_out_of_treatment') %>%
  group_by(exposure_groups,in_work) %>%
  summarise(n =n(),
            total = total_drop) %>% 
  mutate(Percentage = (n/total)*100)

employment_res1 <- rbind(employment_on_therapy1,employment_on_therapy2,employment_on_therapy3)



pay_and_employment_by_age <- df %>%
  group_by(age_bands, census_sex) %>%
  summarise(average_pay = mean(pay),
           average_pay_deflated = mean(pay_deflated),
           prop_in_work = sum(in_work)/n(),
           n_people = n(),
           test = mean(pay_deflated, na.rm=FALSE)) 




#--- save all 8 results for linked_data --------#
dataset_names <- list('exposure_groups' = res1, 
                      'age_distribution' = res2,
                      'age_bands_counts' = res3,
                      'age_min_max_mean' = res4a,
                      'pay_min_max_mean' = res4b,
                      'pay_min_max_sex' = res4c,
                      'age_min_max_sex' = res4d,
                      'age_min_max_exp_groups' = res4dd,
                      'pay_min_max_sex_treatment' = res4e,
                      'age_min_max_sex_treatment' = res4f,                      
                      'proportion_census' = res5,
                      'proportion_exposure_census'= res6,
                      'proportion_census_exposure' = res7,
                      'no_of_treat_by_exp' = resA, 
                      'no_of_treat_by_exp_treatoutc' = resB,
                      'follow_up_stats' = res8a,
                      'follow_up_stats_exp_groups' = res8b,
                      'follow_up_stats_by_age' = res8c,
                      #'counts_by_t_therapy_byex_groups' = res9,
                      'comparisons_wide_table' = wide_table,
                      'proportion_all_formatted' = proportions_formatted,
                     'qtr_pay_on_1ther_all'=res_pay1_quarterly,
                      'qtr_pay_1ther_employed'=res_pay2_quarterly,
                      'm_pay_1ther_all' = res_pay1_monthly,
                      'm_pay_1ther_employed' = res_pay2_monthly,
                      'employment_on_1ther' = employment_res1)

write.xlsx(dataset_names, 
           paste0(outputs_folder,"3_Descriptive_statistics_quarterly_population_", Sys.Date(), ".xlsx"))

rm(res1,res2,res3,res4,res5,res6,res7, res8a,res8b,res8c, #res9,
   wide_table, proportions_formatted, dataset_names)
gc()


#------------------------------------------------------------------------------------------
### 3. Summary graphs 
#------------------------------------------------------------------------------------------

### GRAPH 1 - Monthly pay average of all people per year

temp_df_1 <- df %>% 
             mutate(pay = as.numeric(pay)) %>%
             group_by(reference_date) %>% 
             summarise(average_monthly_pay = mean(pay))

temp_df_1 <- collect(temp_df_1)
# Convert 'date' to a character string with leading zeros for the month
temp_df_1$date_label <- sub("(\\b\\d{4}-)(\\d{1}\\b)", "\\10\\2", temp_df_1$reference_date)
  
  # plot graph
g1 <- temp_df_1 %>% 
 ggplot(aes(x = date_label, 
            y = average_monthly_pay)) +
 geom_line(aes(group=1),
           color = "cyan3",
           size= 0.5)  + 
  #ylim(1000,3000) +
  labs(title="Quarterly average pay for the whole population",
       x ="Date", 
       y = "Average quarterly pay per person") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



### GRAPH 2 -  Monthly pay average of all broken down by exposure groups
temp_df_2 <- df %>% 
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
  labs(title="Quarterly average pay broken down by exposure groups",
       x ="Date", 
       y = "Average quarterly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')


### GRAPH 3 -  Average pay by time to referral broken down by exposure groups
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
  labs(title="Quarterly average pay broken down by exposure groups",
       x ="Time to referral in quarters", 
       y = "Average quarterly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')

### GRAPH 4 -  Distribution of age

# Distribution of ages 
age_distribution_all <- df %>%
  filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") %>%
  group_by(age_at_referral) %>%
  summarise(Count = n()) %>%
  arrange(age_at_referral) %>%
  collect()

# Age stats - min, max, mean
age_stats_all <- df %>%
filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") %>%
  summarise(
    min_age = min(age_at_referral),
    max_age = max(age_at_referral),
    mean_age = mean(age_at_referral),
    sd_age = sd(age_at_referral)
  ) %>% collect()


# Distribution of age chart    
g5 <- ggplot(age_distribution_all, aes(x= age_at_referral, y= Count)) + 
  geom_bar(stat="identity", fill = "lightblue") +
  labs(title="Distribution of age at person level",
       x ="Age", 
       y = "Frequency") +
  scale_x_continuous(breaks=seq(10,80, by = 5)) +
  geom_vline(xintercept = age_stats_all$mean_age, col='black', linetype = 'dashed', size=1)+
  annotate(x=age_stats_all$mean_age,y=+Inf,label="Mean age",vjust=2,geom="label") +
  theme_bw()


# number of referrals 
referrals_df <- df %>%
  filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") %>%
  group_by(referral_date) %>%
  summarise(Count = n()) %>%
  arrange(referral_date) %>%
  collect()


g6 <- ggplot(referrals_df, aes(x= referral_date, y= Count)) + 
  geom_bar(stat="identity", fill = "lightblue") +
  labs(title="Distribution of referrals received per year quarter",
       x ="Year quarter", 
       y = "Frequency") +
  theme_bw()



# save all plots in PDF file
pdf(paste0(outputs_folder,"3_Summary_graphs_quarterly_population_", Sys.Date(), ".pdf"), 
                       width = 10, height = 4)
print(g1)
print(g2)
print(g3)
print(g5)
print(g6)



dev.off()


dataset_names <- list('1_average_pay_all' = temp_df_1, 
                      '2_average_pay_by_exposure_groups_qrt' = temp_df_2,
                      '3_average_pay_by_exposure_groups_t_referral' = temp_df_3,
                      '4_quarterly_age_distribution' = age_distribution_all,
                      '5_quarterly_age_stats'=age_stats_all,
                      '6_distribution_of_referrlas' = referrals_df)

write.xlsx(dataset_names, 
           paste0(outputs_folder,
                  "3_Summary_graphs_numbers_quarterly_population_", Sys.Date(),".xlsx"))


#--------------------------------------------------------------------------
# Closing Spark sessions
#--------------------------------------------------------------------------
spark_disconnect(sc)
