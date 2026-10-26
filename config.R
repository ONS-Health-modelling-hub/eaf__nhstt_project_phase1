# Config file 
#Date vector start and end dates (for monthly NHS TT)
date_vec <- create_monthly_dates_vec("2014-04", "2023-01")

date = "02092024"

#List of variables for counts for loop/function on the whole population (calculate_var_count function)
argument_list <- c('sex', 
                   'ethnic_group_tb', 
                   'is_carer_category',
                   'is_carer_binary',
                   'ns_sec',
                   'disability_category_11',
                   'disability_binary',
                   'age_bands',
                   'highest_qualification',
                   'legal_partnership_status',
                   'religion_tb',
                   'country_of_birth_agg',
                   'hh_family_composition',
                   'hh_dependent_children',
                   'treatment_outcomes',
                   'sourcereferral',
                   'emp_support_flag', 
                   'last_employment_stat',
                   'referral_employment_stat',
                   'Region',
                   'deprived',
                   'therapy_intensity',
                   'diagnosis',
                   'first_medication',
                   'first_sickpay',
			 'number_appointments')

# List of variables to be selected from monthly samples (used in script: )
month_sample_vars <- c('census_person_id', #sample_argument_list
                       'exposure_groups',
                       't_referral', 
                       't_first_therapy', 
                       #'t_last_therapy',
                       'sex',
                       'age_bands',
                       'ethnic_group_tb',
                       'highest_qualification',
                       'country_of_birth_agg', 
                       #'is_carer_binary',
                       'disability_category_11',
                       'referral_employment_stat',
                       'hh_dependent_children',
                       'ns_sec',
                       'sourcereferral', 
                       'deprived',
                       'Region',
                       'therapy_intensity',
                       'diagnosis',
                       'first_medication',
                       #'first_sickpay',
                       'pay',
                       'age_monthly',
                       'month')


# List of variables to be selected from quarterly samples (used in script: )
#vars
qtr_sample_vars <- c('census_person_id', 
                     'exposure_groups',
                     't_referral', 
                     't_first_therapy', 
                     't_last_therapy',
                     'sex',
                     'age_bands',
                     'ethnic_group_tb',
                     'highest_qualification',
                     'country_of_birth_agg', 
                     'is_carer_binary',
                     'disability_category_11',
                     'referral_employment_stat',
                     'hh_dependent_children',
                     'ns_sec',
                     'sourcereferral', 
                     'deprived',
                     'Region',
                     'therapy_intensity',
                     'diagnosis',
                     'first_medication',
                     'first_sickpay',
                     'pay',
                     'quarterly_age',
                     'reference_date',
                     'age_at_referral')


# List of variables for descriptives on samples & randomForest formula
sample_argument_list2 <- c('sex', 
                           'age_bands', 
                           'ethnic_group_tb',
                           'highest_qualification',
                           'country_of_birth_agg',  
                           'is_carer_binary',
                           'disability_category_11',
                           'referral_employment_stat',
                           'hh_dependent_children',
                           'ns_sec',
                           'sourcereferral', 
                           'Region',
                           'deprived',
                           'therapy_intensity',
                           'exposure_groups',
                           'diagnosis',
                           'first_medication',
                           'first_sickpay')



# Start and end date of each reporting period
start_19_20 <- "2019-04-01"
start_18_19 <- "2018-04-01"
start_17_18 <- "2017-04-01"
start_16_17 <- "2016-04-01"
start_15_16 <- "2015-04-01"

end_19_20 <- "2020-03-31"
end_18_19 <- "2019-03-31"
end_17_18 <- "2018-03-31"
end_16_17 <- "2017-03-31"
end_15_16 <- "2016-03-31"


# Set start and end reporting periods to take on NHS TT data
end_rp <- end_19_20
start_rp <- start_16_17

# Select sample size for the sampled IDs from NHS TT

sample_size = 0.2
qtr_sample = 1


# Final dataset name 
final_linked_dataset = "final_linked_dataset_v5" 
quarterly_df = "quarterly_final_dataset_table" 

# List of table (partitions) to load from HDFS for regression analysis
table_names <- c('list of table names')
