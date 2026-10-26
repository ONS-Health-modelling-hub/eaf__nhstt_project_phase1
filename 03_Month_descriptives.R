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
#xl_config$spark.shuffle.service.enabled <- "true"

sc <- spark_connect(
  master = "yarn-client",
  app_name = "xl-session0",
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

# Add quantiles 
quantiles <- sdf_quantile(df, "pay", probabilities = c(0.01,0.99))
q99 <- quantiles[[2]]

df <- df %>%
  mutate(pay = ifelse(pay > q99, q99, pay))

#------------------------------------------------------------------------------------------
### 2. Descriptives and summary stats 
#------------------------------------------------------------------------------------------
                      
# Exposure groups counts & proportions 
res1 <- proportion_function(df, exposure_groups)
gc()

# distribution of age, Age bands counts & proportions
res2 <- proportion_function(df, age_at_referral) %>% arrange(age_at_referral)
res3 <- proportion_function(df, age_bands) %>% arrange(age_bands)
gc()

# Age min, max and average
res4a <- min_max_mean(df, age_at_referral)
res4b <- min_max_mean(df, pay)
res4c <- min_max_mean_grouped(df, age_at_referral, sex)
res4d<- min_max_mean_grouped(df, age_at_referral, exposure_groups)

# CATEGORICAL VARIABLES - COUNTS & PROPORTIONS
res5 <- proportion_for_var_list (df, argument_list)
gc()

# CATEGORICAL VARIABLES BY EXPOSURE GROUPS - COUNTS & PROPORTIONS 
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


#--- save all 8 results for linked_data --------#
dataset_names <- list('exposure_groups' = res1, 
                      'age_distribution' = res2,
                      'age_bands_counts' = res3,
                      'age_min_max_mean' = res4a,
                      'pay_min_max_mean' = res4b,
                      'age_min_max_mean_sex' = res4c,
                      'age_min_max_mean_exp_groups' = res4d,
                      'proportion_census' = res5,
                      'proportion_exposure_census'= res6,
                      'proportion_census_exposure' = res7,
                      'comparisons_wide_table' = wide_table,
                      'proportion_all_formatted' = proportions_formatted)

write.xlsx(dataset_names, 
           paste0(outputs_folder,"1_Descriptive_statistics_whole_population_", Sys.Date(), ".xlsx"))

rm(res1,res2,res3,res4,res5,res6,res7, wide_table, proportions_formatted, dataset_names)

gc()


#------------------------------------------------------------------------------------------
### 3. Summary graphs 
#------------------------------------------------------------------------------------------


### GRAPH 1 - Monthly pay average of all people per year

g1 <- graph_1_function(df, "Graph1")
gc()

### GRAPH 2 -  Monthly pay average of all broken down by exposure groups

g2 <- graph_2_function(df=df, vars=exposure_groups, year="Graph2",  name="exposure_groups_")
gc()


### GRAPH 3 -  Average pay by time to referral broken down by exposure groups

g3 <- graph_3_function(df=df, vars=exposure_groups, year="Graph5",  name="exposure_groups_")
gc()


### GRAPH 6 -  Distribution of age

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
g4 <- ggplot(age_distribution_all, aes(x = age_at_referral, y=Count)) + 
  geom_bar(stat="identity", fill = "lightblue") +
  labs(title="Distribution of age at person level",
       x ="Age", 
       y = "Frequency") +
  scale_x_continuous(breaks=seq(10,80, by = 5)) +
  geom_vline(xintercept = age_stats_all$mean_age, col='black', linetype = 'dashed', size=1)+
  annotate(x= age_stats_all$mean_age, y=+Inf, label="Mean age",vjust=2, geom="label") +
  theme_bw()

ggsave(file= paste0(outputs_folder,"age_person_level_whole_pop.png"), width = 10, height = 4, units = "in")

# save all plots in PDF file
pdf(paste0(outputs_folder,"1_Summary_graphs_whole_population_", Sys.Date(), ".pdf"), 
                       width = 10, height = 4)
print(g1)
print(g2)
print(g3)
print(g4)


dev.off()

rm(df,g1,g2,g3,g4,q99,quantiles)
gc()




#####################################################################################
#####################################################################################
#####################################################################################


#------------------------------------------------------------------------------------------
### SAMPLE DESCRIPTIVES
#------------------------------------------------------------------------------------------


#####################################################################################
#####################################################################################
#####################################################################################

df <- sparklyr::sdf_sql(sc=sc,
                       paste0("SELECT * 
                                FROM project_name.",final_linked_dataset,""))

sample <- sparklyr::sdf_sql(sc=sc,
                      paste0("SELECT DISTINCT  *  FROM project_name.", sample_completed_df,""))

sample_1 <- df %>%
  select(month_sample_vars) %>%
  inner_join(sample, by = "census_person_id")

rm(sample)
gc()

sample <- sparklyr::sdf_sql(sc=sc,
                      paste0("SELECT DISTINCT  *  FROM project_name.", sample_dropped_out_df,""))


sample_2 <- df %>% 
  select(month_sample_vars) %>%
  inner_join(sample, by = "census_person_id" )

rm(sample)
gc()


joined_sample_df <- sdf_bind_rows(sample_1,sample_2)


rm(df, sample_1,sample_2)


quantiles <- sdf_quantile(joined_sample_df, "pay", probabilities = c(0.01,0.99))
q99 <- quantiles[[2]]

joined_sample_df <- joined_sample_df %>%
  mutate(pay = ifelse(pay > q99, q99, pay))

#--------------------------------------------------------------------------
# Main descriptives for the sample - 20%
#--------------------------------------------------------------------------

# Exposure groups counts & proportions 

res1 <- proportion_function(joined_sample_df, exposure_groups)
gc()

age_df <- joined_sample_df %>% 
  filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") 

# distribution of age at referral
res2 <- proportion_function(age_df, age_monthly) %>% arrange(age_monthly)
gc()


# Pay at referral- min, max and average broken down by exposure groups

res3 <- age_df %>% group_by(exposure_groups) %>%
    summarize(min_pay = min(pay),
              max_pay = max(pay),
              mean_pay = mean(pay),
              sd_pay = sd(pay))
gc()


res3b <- age_df %>% group_by(exposure_groups, sex) %>%
    summarize(min_pay = min(pay),
              max_pay = max(pay),
              mean_pay = mean(pay),
              sd_pay = sd(pay))

gc()

# Age min, max and average
res4 <- age_df %>%
    summarize(min_age = min(age_monthly),
              max_age = max(age_monthly),
              mean_age = mean(age_monthly),
              sd_age = sd(age_monthly))

gc()

# Age min, max and average
res4b <- age_df %>% group_by(exposure_groups) %>%
    summarize(min_age = min(age_monthly),
              max_age = max(age_monthly),
              mean_age = mean(age_monthly),
              sd_age = sd(age_monthly))

gc()
age_df <-age_df %>% select(census_person_id, age_bands)
joined_sample_df <- left_join(joined_sample_df,age_df,by = "census_person_id")

# CATEGORICAL VARIABLES - COUNTS & PROPORTIONS

res5 <- proportion_for_var_list (joined_sample_df, sample_argument_list2)
gc()


# CATEGORICAL VARIABLES BY EXPOSURE GROUPS - COUNTS & PROPORTIONS 

# Results showing proportions e.g. of males and females in Completed exposure group
# data grouped by exposure groups then grouped by sex
res6 <- proportion_for_exposure_by_var_list(joined_sample_df, sample_argument_list2)
gc()

# Results showing proportions e.g. of exposure groups who are Males 
# data grouped by sex then grouped by exposure groups
res7 <- proportion_for_var_by_exposure(joined_sample_df, sample_argument_list2)
gc()

# Results in wide format 
wide_table <- res6 %>% 
          pivot_wider(names_from = exposure_groups,
                      values_from = c(Count, total, percentage)) %>%
          select(-contains("total_")) %>% 
          arrange(Variable_names) %>%
          mutate(Completed_treatment = paste(Count_completed_treatment, " (", round(percentage_completed_treatment, 1), "%)", sep = ""),
                 Dropped_out_of_treatment = paste(Count_dropped_out_of_treatment, " (", round(percentage_dropped_out_of_treatment, 1), "%)", sep = "")) %>%
          rename(Characteristic = Variable_names,
                 Category = Category_name) %>%
          select(Characteristic, Category, Completed_treatment, Dropped_out_of_treatment) %>%
          rename("Completed treatment" = Completed_treatment,
                 "Dropped out of treatment" = Dropped_out_of_treatment)
gc()


# Fromatted results with proportions for whole df
proportions_formatted <- res5 %>%
                      arrange(Variable_names) %>%
                      mutate(Count_percentage = paste(Count, " (", round(percentage, 1), "%)", sep = "")) %>%
                      rename(Characteristic = Variable_names,
                             Category = Category_name,
                             'Count(%)' = Count_percentage)%>%
                      select(Characteristic, Category, 'Count(%)')
gc()


#--- save all 8 results for linked_data --------#
dataset_names <- list('exposure_groups' = res1, 
                      'age_distribution' = res2,
                      'pay_at_referral_exp_gr' = res3,
                      'pay_at_referral_exp_gr_sex' = res3b,
                      'min_max_mean_stats' = res4,
                      'min_max_mean_stats_by_exp_gr' = res4b,
                      'proportion_census' = res5,
                      'proportion_exposure_census'= res6,
                      'proportion_census_exposure' = res7,
                      'comparisons_wide_table' = wide_table,
                      'proportion_all_formatted' = proportions_formatted)

write.xlsx(dataset_names, 
           paste0(outputs_folder,"2_Descriptive_statistics_sample_population_", Sys.Date(), ".xlsx"))

rm(res1,res2,res3,res4,res5,res6,res7, wide_table, proportions_formatted, dataset_names)
gc()


#------------------------------------------------------------------------------------------
### 3. Summary graphs 
#------------------------------------------------------------------------------------------


### GRAPH 1 - Monthly pay average of all people per year
temp_df_1 <- joined_sample_df %>% 
    mutate(pay = as.numeric(pay)) %>%
    group_by(month) %>% 
    summarise(average_monthly_pay = mean(pay))
    
  # plot graph
g1 <- temp_df_1 %>% 
    ggplot(aes(x = month, 
               y = average_monthly_pay)) +
    geom_line(aes(group=1),
              color = "cyan3",
              size= 0.5)  + 
    labs(title="Monthly average pay for the whole sample",
         x ="Date", 
         y = "Average monthly") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  



### GRAPH 2 -  Monthly pay average of all broken down by exposure groups
temp_df_2 <- joined_sample_df %>% 
    mutate(pay = as.numeric(pay)) %>%
    group_by(month, exposure_groups) %>% 
    summarise(average_monthly_pay = mean(pay))
  
  # plot graph
g2 <- temp_df_2 %>% 
  ggplot(aes(x = month, 
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
  



### GRAPH 3 -  Average pay by time to referral broken down by exposure groups
temp_df_3 <- joined_sample_df %>% 
    mutate(pay = as.numeric(pay)) %>%
    mutate(t_referral = as.numeric(t_referral)) %>%
    group_by(t_referral, exposure_groups) %>% 
    summarise(average_monthly_pay = mean(pay)) 
  
g3 <- temp_df_3 %>% 
  ggplot(aes(x = t_referral, 
           y = average_monthly_pay, 
           colour = exposure_groups,
           group = exposure_groups)) +
  geom_line(alpha=0.6)  + 
  labs(title="Monthly pay per time to referral by exposure groups",
       x ="Time to referral in months", 
       y = "Average monthly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:') +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "black")
  


### GRAPH 4 -  Average pay by time to referral broken down by exposure groups and sex

temp_df_4 <- joined_sample_df %>%
   mutate(t_referral = as.numeric(t_referral)) %>%
  group_by(sex, t_referral, exposure_groups) %>%
  summarise(average_monthly_pay = mean(pay)) 


g4 <- ggplot(temp_df_4, aes(t_referral, average_monthly_pay, color = exposure_groups)) +
  geom_line() +
  labs(title="Monthly pay per time to referral by exposure groups and sex",
       x ="Time to referral in months", 
       y = "Average monthly pay") +
labs(color='Legend:') +
  geom_smooth(method = 'loess') +
  facet_wrap(~sex)



### GRAPH 4 -  Distribution of age

# Distribution of ages 
age_distribution_all <- joined_sample_df %>%
  filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") %>%
  group_by(age_monthly) %>%
  summarise(Count = n()) %>%
  arrange(age_monthly) %>%
  collect()

# Age stats - min, max, mean
age_stats_all <- joined_sample_df %>%
filter(t_referral == 0) %>% 
  sdf_drop_duplicates(cols = "census_person_id") %>%
  summarise(
    min_age = min(age_monthly),
    max_age = max(age_monthly),
    mean_age = mean(age_monthly),
    sd_age = sd(age_monthly)
  ) %>% collect()


# Distribution of age chart    
g5 <- ggplot(age_distribution_all, aes(x=age_monthly, y=Count)) + 
  geom_bar(stat="identity", fill = "lightblue") +
  labs(title="Distribution of age at person level",
       x ="Age", 
       y = "Frequency") +
  scale_x_continuous(breaks=seq(10,80, by = 5)) +
  geom_vline(xintercept = age_stats_all$mean_age, col='black', linetype = 'dashed', size=1)+
  annotate(x=age_stats_all$mean_age,y=+Inf,label="Mean age",vjust=2,geom="label") +
  theme_bw()

ggsave(file= paste0(outputs_folder,"age_person_level_sampled_pop.png"), width = 10, height = 4, units = "in")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

pdf(paste0(outputs_folder,"2_Summary_graphs_sample_population_", Sys.Date(), ".pdf"), 
                       width = 10, height = 4)
print(g1)
print(g2)
print(g3)
print(g4)
print(g5)

dev.off()


dataset_names <- list('1_average_pay_all_sample' = temp_df_1, 
                      '2_average_pay_by_exposure_groups_month' = temp_df_2,
                      '3_average_pay_by_exposure_groups_t_referral' = temp_df_3,
                      '4_average_pay_t_referral_sex_exposure_groups' = temp_df_4,
                      '5a_age_stats_min_max_sd' = age_stats_all,
                      '5b_age_distribution_all'=age_distribution_all)

write.xlsx(dataset_names, 
           paste0(outputs_folder,
                  "2_Summary_graphs_numbers_sample_population_", Sys.Date(),".xlsx"))


#--------------------------------------------------------------------------
# Closing Spark sessions
#--------------------------------------------------------------------------
spark_disconnect(sc)
