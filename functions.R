#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
# Load NHS Talking Therapies data function 
#-------------------------------------------------------------------------------------------------
#' This function loads NHS TT data; 
#'  it also allows you to choose which year you'd like to load
#' 
#' @description This is a function load NHS TT data by chosen year
#' @param data_table = dataset name
#' @return Spark DataFrame 

load_nhs_tt_data_function  <- function(data_table) {
        
    sparklyr::sdf_sql(sc = sc, 
                paste0("SELECT DISTINCT 
                          census2011_person_id, 
                          census2021_person_id, 
                          iapt_person_id,
                          ic_pathway_id,
                          refrecdate,
                          ic_date_first_assessment,
                          ic_date_first_therapeutic_session,
                          ic_date_second_therapeutic_session,
                          ic_date_last_therapeutic_session,
                          ic_date_last_appointment,
                          ic_count_treatment_appointments,
                          ic_completed_treatment_flag,
                          sourcereferral,
                          ic_recovery_flag, 
                          ic_reliable_improv_flag, 
                          ic_reliable_deter_flag, 
                          ic_no_change_flag,
                          ic_caseness_flag,
                          ic_not_caseness_flag,
                          endcode,
                          ic_probdesc_primary,
                          ic_probdesc_secondary,
                          ic_first_sickpay_indicator,
                          ic_last_sickpay_indicator,
                          ic_first_psychotropic_indicator,
                          ic_last_psychotropic_indicator

  FROM project_name.",data_table, "
    WHERE refrecdate IS NOT NULL"))
}

#-------------------------------------------------------------------------------------------------
# Format NHS Talking Therapies date variables into date format
#-------------------------------------------------------------------------------------------------
#' Format NHS Talking Therapies date variables into date format (YYYY-MM-DD) 
#' as this varies between years. 
#' 
#' @param data = dataset name 
#' @return Spark DataFrame with corrected date variables

date_format_nhs_vars_function <- function(df){

  formated_df <- df %>% 
  mutate(refrecdate = to_date(refrecdate, "yyyy-MM-dd")) %>%
  mutate(ic_date_first_assessment = to_date(ic_date_first_assessment, "yyyy-MM-dd")) %>%
  mutate(ic_date_first_therapeutic_session = to_date(ic_date_first_therapeutic_session,"yyyy-MM-dd")) %>%
  mutate(ic_date_second_therapeutic_session = to_date(ic_date_second_therapeutic_session, "yyyy-MM-dd")) %>%
  mutate(ic_date_last_therapeutic_session = to_date(ic_date_last_therapeutic_session, "yyyy-MM-dd")) %>%
  mutate(ic_date_last_appointment = to_date(ic_date_last_appointment, "yyyy-MM-dd")) 

  return(formated_df)
  
}

#-------------------------------------------------------------------------------------------------
# Updates dates with latest info
#-------------------------------------------------------------------------------------------------
#' @description This function updates dates with latest information. 
#' @param data_table = dataset name
#' @return Spark DataFrame 

latest_dates_update_function <- function (df) { 
  
  updated_df <- df %>% 
    group_by(ic_pathway_id) %>%
    mutate(ic_date_first_assessment = max(ic_date_first_assessment),
           ic_date_first_therapeutic_session = max(ic_date_first_therapeutic_session),
           ic_date_second_therapeutic_session = max(ic_date_second_therapeutic_session), 
           ic_date_last_therapeutic_session = max(ic_date_last_therapeutic_session),
           ic_date_last_appointment = max(ic_date_last_appointment),
           ic_count_treatment_appointments = max(ic_count_treatment_appointments),
           ic_completed_treatment_flag = max(ic_completed_treatment_flag),
           ic_recovery_flag = max(ic_recovery_flag),
           ic_reliable_improv_flag = max(ic_reliable_improv_flag),
           ic_reliable_deter_flag = max(ic_reliable_deter_flag),
           ic_no_change_flag = max(ic_no_change_flag),
           ic_caseness_flag = max(ic_caseness_flag),
           ic_not_caseness_flag = max(ic_not_caseness_flag),
           endcode = max(endcode)) %>%

    ungroup() 

 return(updated_df)
}


#-------------------------------------------------------------------------------------------------
# Main NHS Talking Therapies editing function - we do not use
#-------------------------------------------------------------------------------------------------
#' @description This is the main function that edits and creates final NHS TT data. 
#'
#' This function has other nested functions:
#'  - date_format_nhs_vars_function 
#'  - latest_dates_update_function
#'  - caseness_and_outcomes_function (currently not being used)
#'  - exposure_groups_function
#'
#' This function does: 
#' 1. Formats all date variables into a date format (yyyy-mm-dd).
#' 2. Filters referrals that were received within the reporting period.
#' 3. Keeps the earliest referral per person (if there are 1+ referrals in the reporting year)
#' 4. Updates columns with most up to date information if therapy sessions moved onto next year.
#' 5. Removes any remaining duplicates. 
#' 6. Creates exposure groups (treatment stages categories)
#' 7. New columns for correct assessment_date, first_therapy_date, last_therapy_date.
#'
#' @param end_rp = end of the reporting period (can be edited in config.R)
#' @param start_rp = start of the reporting period (can be edited in config.R)
#' @return Spark DataFrame with final person level NHS TT


main_nhs_tt_function <-function(df, end_rp, start_rp){
  

  final_nhs_tt <- df %>%
  

    # correct the date format of all date variables
    date_format_nhs_vars_function(.) %>%    
  
    filter(refrecdate <= end_rp & refrecdate >= start_rp) %>% 

  
    latest_dates_update_function(.) %>%   # updates cols- groups by pathways id
  
    sdf_drop_duplicates() %>%

    # remove people who had an assessment after first therapy session 
    mutate(temp_flag = 
         ifelse(!is.na(ic_date_first_assessment) & 
                 ic_date_first_assessment > ic_date_first_therapeutic_session, 1,0)) %>%
    filter(is.na(temp_flag) | temp_flag == 0) %>% 
    select(-temp_flag) %>%

    # create rank 
    group_by(iapt_person_id) %>%
    mutate(rank = 
           case_when(ic_completed_treatment_flag == 'Y' & 
                     refrecdate == min(refrecdate) ~ 1,
                     
                     ic_completed_treatment_flag == 'Y' & 
                     refrecdate != min(refrecdate) ~ 2,
                
                     is.na(ic_completed_treatment_flag) & 
                     ic_count_treatment_appointments == max(ic_count_treatment_appointments)  ~ 3,
              
                     is.na(ic_completed_treatment_flag) & 
                     ic_count_treatment_appointments != max(ic_count_treatment_appointments) &
                     refrecdate == min(refrecdate) ~ 4,
                
                     is.na(ic_completed_treatment_flag) & 
                     is.na(ic_count_treatment_appointments) & 
                     refrecdate == min(refrecdate) ~ 5,
                     
                     is.na(ic_completed_treatment_flag) & 
                     ic_completed_treatment_flag == mean(ic_completed_treatment_flag) &
                     refrecdate == min(refrecdate) ~ 6,
                     
                     TRUE ~ 99)) %>% ungroup() %>%

    #Order by rank - keep the referral with the highest rank
    arrange(iapt_person_id, rank, refrecdate) %>%
    sdf_drop_duplicates(cols='iapt_person_id') %>%
  
#
    #create treatment stages categories (exposure groups) - function
    exposure_groups_function(.) %>%

   # New cols with correct dates
    mutate(assessment_date = 
          ifelse(is.na(ic_date_first_assessment) & 
                  !is.na(ic_date_first_therapeutic_session),
            ic_date_first_therapeutic_session,
            ic_date_first_assessment)) %>% 

    mutate(last_therapy_date = 
        ifelse(exposure_groups == 'completed_treatment', #|
                #exposure_groups == 'started_treatment', 
                ic_date_last_appointment, NA)) %>% 

    mutate(first_therapy_date = 
           ifelse(!is.na(ic_date_first_assessment) &
                   !is.na(ic_date_first_therapeutic_session) &
                   ic_date_first_therapeutic_session != ic_date_first_assessment,
                    ic_date_first_therapeutic_session, 
                    ic_date_second_therapeutic_session)) %>% 

      # Create our initial caseness flags NOTE: can't do at the moment, not all years have phq/gad in ONS
      # we'll use flags made by NHS
    caseness_and_outcomes_function(.) %>%
  
    rename(., last_appointment = ic_date_last_appointment) %>%

  
  mutate(caseness_flag = 
        case_when(ic_caseness_flag == 'Y' | ic_caseness_flag == 'y' ~ 1,
                 ic_not_caseness_flag == 'Y' | ic_not_caseness_flag == 'y' ~ 0,
                 is.na(ic_caseness_flag) & is.na(ic_not_caseness_flag) ~ 'Missing')) %>%

    # remove old columns
    select(#-ic_pathway_id,
           -ic_date_first_assessment,
           -ic_date_first_therapeutic_session,
           -ic_date_second_therapeutic_session,
           -ic_date_last_therapeutic_session)


return(final_nhs_tt)
}



#-------------------------------------------------------------------------------------------------
# caseness_and_outcomes_function
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#' @description This is function creates new variables and flags from iapt variables.
#'              These are: anxiety caseness and depression variables, overall caseness flag,
#'              and creates an overall variable that combines all treatment outcome flags
#' 
#'
#'
#' @param data = dataset name 
#' 
#' @return Spark DataFrame with new variables and flags derived from iapt variables

caseness_and_outcomes_function <- function(data){
  
data_new <- data %>%
  
#  #create initial anxiety caseness variable
#  mutate(anxiety_initial_caseness = 
#        case_when(ic_first_gad >= 8 ~ "at_caseness",
#                  ic_first_gad < 8 ~ "not_at_caseness",
#                  TRUE ~ "missing")) %>%
#  
#  #create initial depression caseness variable
#  mutate(depression_initial_caseness =
#        case_when(ic_first_phq9 >= 10 ~ "at_caseness",
#                  ic_first_phq9 < 10 ~ "not_at_caseness",
#                  TRUE ~ "missing")) %>%
#  
#  #initial overall caseness flag (patients with missing scares will assigned 0, so same as people "not_at_caseness")
#  mutate(initial_caseness = 
#        if_else(depression_initial_caseness == "at_caseness" | 
#                anxiety_initial_caseness == "at_caseness", 1,0)) %>%
  
  #create 1 variable that combines all treatment outcome flags (improvement, recovery, no chnage & deterioration)
  mutate(treatment_outcomes =
       case_when(ic_recovery_flag =='Y' ~ "recovery",
                 ic_reliable_improv_flag =='Y' ~ "improvement",
                 ic_reliable_deter_flag =='Y' ~ "deterioration", 
                 ic_no_change_flag =='Y' ~ "no change",
                 is.na(ic_recovery_flag) &
                 is.na(ic_reliable_improv_flag) &
                 is.na(ic_reliable_deter_flag) & 
                 is.na(ic_no_change_flag) ~ "not completed treatment/missing scores")) %>% 
  
  #create binary flag for ic_completed_treatment_flag -replace NAs with 0s amd chnage Y into 1 
  mutate(completed_treatment_flag = 
         case_when(is.na(ic_completed_treatment_flag) ~0,
                   ic_completed_treatment_flag == 'Y' ~ 1)) %>% 
  select(-c('ic_recovery_flag','ic_reliable_improv_flag', 'ic_reliable_deter_flag',
            'ic_no_change_flag', 'ic_completed_treatment_flag'))

return(data_new)
}



#-------------------------------------------------------------------------------------------------
# Update column dates (update_column)
#-------------------------------------------------------------------------------------------------

#' @description Update empty columns that might have been empty in the 18/19 financial year but new date
#' appeared in the following financial year. 
#'
#' @param data = Spark DataFrame of IAPT 
#' @param column = orignal column e.g. ic_date_first_assessment, ic_date_first_therapeutic_session etc
#' @param extra_column = corresponding column from following financial year e.g. ic_date_first_assessment_extra
#'
#' @return Data frame with updated columns (if they were empty before)


update_column <- function(data, column, extra_column) {
  data %>%
    mutate({{ column }} := if_else(is.na({{ column }}) & 
                                   !is.na({{ extra_column }}), 
                                   {{ extra_column }}, {{ column }}))
}



#-------------------------------------------------------------------------------------------------
# Update dates (update_column_last_dates)
#-------------------------------------------------------------------------------------------------

#' @description Update columns that include dates and counts and replace with the latest information 
#' (if a referral moved onto the next financial year).

#' @param data = Spark DataFrame of IAPT 
#' @param data = Spark DataFrame of IAPT 
#' @param column = orignal column e.g. ic_count_treatment_appointments
#' @param extra_column = corresponding column from following financial year e.g.ic_count_treatment_appointments_extra
#'
#' @return Data frame with updated columns


update_column_last_dates <- function(data, column, extra_column) {
  data %>%
    mutate ({{ column }} := case_when({{ extra_column }} > {{ column }} | 
                                       is.na({{ column }}) & 
                                        !is.na({{ extra_column }}) ~ {{ extra_column }},
                                       
                                       !is.na({{ column }}) & 
                                        is.na({{ extra_column }}) |
                                        {{ column }} == {{ extra_column }} |
                                        {{ extra_column }} < {{ column }} ~ {{ column }}))
}







#-------------------------------------------------------------------------------------------------
# Add column for number of months passed (add_months_diff_col)
#-------------------------------------------------------------------------------------------------

#' Add column for number of months passed.
#' 
#' Adds column to data frame using a reference date column and column of interest. If the date
#' in the column of interest is before the reference date or is empty, it will be populated
#' with zero. 
#'
#' @param data Data frame.                                  
#' @param colname Unquoted name of column containing date of occurence of interest.
#' @param ref_col Unquoted name of column contraining the reference date for time passed.
#' Default is ref_date.
#' @param floor Boolean (TRUE or FALSE) of whether values should be floored. Default is TRUE
#' @param neg_to_zero Boolean (TRUE or FALSE) of whether negative values should be changed
#' to 0. Default is TRUE.
#' @return Data frame with added column `t_colname`
#' 
add_months_diff_col <- function(data,
                                colname,
                                ref_col = ref_date,
                                floor = TRUE,
                                neg_to_zero = FALSE) {
  
  data %>%
  dplyr::mutate(diff = months_between({{ ref_col }}, {{ colname }}),
                diff = ifelse(floor, floor(diff), diff),
                "t_{{ colname }}" :=  ifelse(diff > 0, diff, ifelse(neg_to_zero, 0, diff))) %>%
  select(-diff)
}

#-------------------------------------------------------------------------------------------------
# Create vector of monthly dates (create_monthly_dates_vec)
#-------------------------------------------------------------------------------------------------
#' Create vector of monthly dates.
#' 
#' Create a vector containing the last day of every month within provided dates
#'
#' @param start_yr_month Character of date for start month in form "YYYY-MM"
#' @param end_yr_month Character of date for last month in form "YYYY-MM"
#' @return Vector of dates
#'
create_monthly_dates_vec <- function(start_yr_month, end_yr_month) {
  min_date <- as.Date(paste0(start_yr_month, "-01"))
  max_date <- as.Date(paste0(end_yr_month, "-01"))
  
  n_months <- lubridate::interval(min_date, max_date) %/% months(1)
  
  c(min_date + months(1:n_months)) - days(1)
}

#-------------------------------------------------------------------------------------------------
#Create long dataframe with monthly rows (create_long_monthly_df)
#-------------------------------------------------------------------------------------------------

#' Create long dataframe with monthly rows
#' 
#' @param data Data frame.
#' @param date_vec Vector of dates to be used when pivoting longer
#' @return Data frame with extra column 'ref_date', where the number of rows 
#' will be the length of date_vec times more than input data frame.
#'
create_long_monthly_df <- function(data, date_vec) {
  
  for (i in 1:length(date_vec)) {
    date <- as.character(date_vec[i])
    data <- mutate(data, "t_{i}" := date)
}
  data %>%
  pivot_longer(cols = starts_with("t_"), values_to ="ref_date") %>%
  select(-name)
}


#-------------------------------------------------------------------------------------------------
# Main monthly function
#-------------------------------------------------------------------------------------------------
#' @description This function makes monthly data out of the person level data for given year/data. 
#'
#' This function has other nested functions:
#'  - create_long_monthly_df
#'  - add_months_diff_col
#'
#' @param df = financial year person level data going in 
#' @return Spark DataFrame with monthly NHS TT data (with Census & deaths)

main_monthly_function <- function(df) {
  
  monthly_person_level_df <- df %>%
  mutate(referral_date = last_day(refrecdate)) %>% 
    rename(referral_received_date = refrecdate) %>%
    create_long_monthly_df(date_vec) %>%
    add_months_diff_col(referral_date) %>%

    mutate(firsttherapy_date = last_day(first_therapy_date)) %>% 
    mutate(lasttherapy_date = last_day(last_therapy_date)) %>% 
    mutate(lastappointment_date = last_day(last_appointment)) %>% 
  
    add_months_diff_col(firsttherapy_date) %>%
    add_months_diff_col(lasttherapy_date) %>%
    add_months_diff_col(lastappointment_date) %>%

    rename_with(~ gsub("_date", "", .x), starts_with("t_")) %>% # rm "_date" from t_ colnames
    arrange(iapt_person_id, ref_date) %>%
    mutate(date_month = month(ref_date)) %>% 
    mutate(date_year = year(ref_date)) %>%
    mutate(month_label = paste0(date_year, "-", date_month)) %>% 
    select(-c('date_month','date_year')) %>% 

    # if person died keep rows before date of death
    filter(is.na(dod) | dod >= ref_date) %>%

    # create monthly age 
     mutate(ref_year = as.numeric(substring(ref_date,1,4)),
            dob_year = as.numeric(substring(prov_dob,1,4))) %>%
     mutate(age_monthly = 
            ifelse(substring(ref_date,6,10) >= substring(prov_dob,6,10), 
                   ref_year - dob_year, (ref_year - dob_year) -1)) %>%
    select(-c('ref_year', 'dob_year','lastappointment_date',
              'referral_date','firsttherapy_date','lasttherapy_date')) %>% 

    # remove people who under 16 and over 69 (mostly working age people)
    filter(age_monthly >= 16 &  age_monthly <= 69) %>%
    rename(referral_date = referral_received_date) %>%
    rename(t_first_therapy = t_firsttherapy) %>%
    rename(t_last_therapy = t_lasttherapy) %>%
    rename(t_last_appointment = t_lastappointment) 


  return(monthly_person_level_df)
}



#-------------------------------------------------------------------------------------------------
# New census id variable and flag - we do not use
#-------------------------------------------------------------------------------------------------
#' @description This function creates new census id variable where if 2011 Census ID is 
#' @description available we use that but if it's missing we used 2021 Census ID.
#' 
#' @param df = dataframe
#' @return Spark DataFrame 


census_11_flag_fun <- function(df){ 
  
  df_new <- df %>% 
    mutate(census_person_id = 
         ifelse(!is.na(census2011_person_id), 
                census2011_person_id, census2021_person_id)) %>% 
    mutate(census11_flag = 
         ifelse(!is.na(census2011_person_id), 1, 0))
  
  return(df_new)

}





################################################################################################
#                                   CENSUS FUNCTIONS 
################################################################################################

#-------------------------------------------------------------------------------------------------
#Creates 'provisional' date of birth based on dob_quarter (create_dob)
#-------------------------------------------------------------------------------------------------

#' Create provisional date of birth based on dob_quarter
#' 
#' @param data Data frame.
#' @return Data frame with extra column 'prov_dob'
#'

create_dob <- function(data) {
  
data <- data %>%
  mutate(prov_dob = 
         case_when(grepl('Q1', dob_quarter) | 
                   grepl('01', dob_quarter) ~ paste0(substring(dob_quarter,1,5), "02-15"),
                   
                   grepl('Q2', dob_quarter)| 
                   grepl('02', dob_quarter) ~ paste0(substring(dob_quarter,1,5), "05-15"),
                   
                   grepl('Q3', dob_quarter)| 
                   grepl('03', dob_quarter) ~ paste0(substring(dob_quarter,1,5), "08-15"),
                   
                   grepl('Q4', dob_quarter)| 
                   grepl('04', dob_quarter) ~ paste0(substring(dob_quarter,1,5), "11-15"),
                   
                   TRUE ~ 'error')) %>%
  mutate(prov_dob = to_date(prov_dob, "yyyy-MM-dd"))
               

  return(data)
  
    
}

#-------------------------------------------------------------------------------------------------
#Renames and aggregates census21 variables (rename_census21_variables)
#-------------------------------------------------------------------------------------------------
#' @description This function renames and aggregates census 2021 variables
#' @description we use this, but if census 2021 missing then we use census variables from census 2011
#' 
#' @param df = dataframe
#' @return Spark DataFrame 




rename_census21_variables <- function(data){
  
new_data <- data %>%
mutate(sex = case_when(sex == 1 ~ 'Females',
                       sex == 2 ~ 'Males'),
  
  ethnic_group_tb = 
       case_when(ethnic_group_tb >= 01 & ethnic_group_tb <= 05 ~ "White",
                 ethnic_group_tb >= 06 & ethnic_group_tb <= 09 ~ "Mixed",
                 ethnic_group_tb >= 10 & ethnic_group_tb <= 14 ~ "Asian",
                 ethnic_group_tb >= 15 & ethnic_group_tb <= 17 ~ "Black",
                 ethnic_group_tb >= 18 & ethnic_group_tb <= 19 ~ "Other",
                 TRUE ~ "Missing or not stated"),
       
#  disability_category = case_when(disability == 1 ~ "Yes - reduced a lot",
#                                  disability == 2 ~ "Yes - reduced a little",
#                                  disability == 3 ~ "Yes - not reduced at all",
#                                  disability == 4 ~ "No",
#                                  TRUE ~ "Missing or not stated"),
       
  disability_binary = 
       case_when(disability == 1 | disability == 2 ~ 1,
                 disability == 3 | disability == 4 ~ 0,
                 TRUE ~ "Missing or not stated"),
       
  is_carer_category = 
       case_when(is_carer == 1 ~ "Not a carer",
                 is_carer >= 2 & is_carer <= 3 ~ "Yes, 1 to 19 hours a week",
                 is_carer >= 4 & is_carer <= 5 ~ "Yes, 20 to 49 hours a week",
                 is_carer == 6 ~ "Yes, 50 or more hours a week",
                 TRUE ~ "Missing or not stated"),
       
  is_carer_binary = case_when(is_carer == 1 ~ 0,
                              is_carer >= 2 & is_carer <= 6 ~ 1,
                              TRUE ~ "Missing or not stated"),
       
  ns_sec = 
       case_when(ns_sec == 1 & ns_sec <= 3 ~ "Higher managerial, administrative and professional occupations",
                 ns_sec >= 4 & ns_sec <= 6 ~ "Lower managerial, administrative and professional occupations",
                 ns_sec == 7 ~ "Intermediate occupations",
                 ns_sec >= 8 & ns_sec <= 9 ~ "Small employers and own account workers",
                 ns_sec >= 10 & ns_sec <= 11 ~ "Lower supervisory and technical occupations",
                 ns_sec == 12 ~ "Semi-routine occupations",
                 ns_sec == 13 ~ "Routine occupations",
                 ns_sec == 14 ~ "Never worked",
                 ns_sec == 15 ~ "Long-term unemployed",
                 ns_sec == 16 ~ "Full-Time students",
                 TRUE ~ "Missing or not stated"),


  religion_tb = case_when(religion_tb == 1 ~ "No religion",
                          religion_tb == 2 ~ "Christian",
                          religion_tb == 3 ~ "Buddhist",
                          religion_tb == 4 ~ "Hindu",
                          religion_tb == 5 ~ "Jewish",
                          religion_tb == 6 ~ "Muslim",
                          religion_tb == 7 ~ "Sikh",
                          religion_tb == 8 ~ "Other religion",
                          TRUE ~ "Missing or not stated"),

    
  highest_qualification = 
       case_when(highest_qualification == 0 ~ "No qualifications",
                 highest_qualification == 1 ~ "Level 1",
                 highest_qualification == 2 ~ "Level 2",
                 highest_qualification == 3 ~ "Apprenticeship",
                 highest_qualification == 4 ~ "Level 3",
                 highest_qualification == 5 ~ "Level 4+",
                 highest_qualification == 6 ~ "Other",
                 TRUE ~ "Missing or not stated"),

  legal_partnership_status =
       case_when(legal_partnership_status == 1 ~ "Never married and never registered a civil partnership",
                 legal_partnership_status >= 2 & legal_partnership_status <= 5 ~ "Married or in a registered civil partnership",
                 legal_partnership_status >= 6 & legal_partnership_status <= 7 ~ "Separated, but still legally married or still legally in a civil partnership",
                 legal_partnership_status >= 8 & legal_partnership_status <= 9 ~ "Divorced or civil partnership dissolved",
                 legal_partnership_status >= 10 & legal_partnership_status <= 11 ~ "Widowed or surviving civil partnership partner",
                 TRUE ~ "Missing or not stated"),

  hh_dependent_children = 
       case_when(hh_dependent_children <= 01 ~ "No dependent children in household",
                 hh_dependent_children > 01 & hh_dependent_children <= 07 ~ "One dependent child",
                 hh_dependent_children >= 08 & hh_dependent_children <= 13 ~ "Two dependent children",
                 hh_dependent_children >= 14 & hh_dependent_children <= 19 ~ "Three or more dependent children",
                 TRUE ~ "Missing or not stated"),
       
       
  hh_family_composition = 
       case_when(hh_family_composition >= 01 & hh_family_composition <= 02 ~ "One person household",
                 hh_family_composition >= 04 & hh_family_composition <= 39 ~ "Single family household: Couple family household",
                 hh_family_composition >= 40 & hh_family_composition <= 45 ~ "Single family household: Lone parent household",
                 hh_family_composition >= 46 & hh_family_composition <= 51 ~ "Other household type",
                 hh_family_composition == 03  ~ "Other household type",
                 TRUE ~ "Missing or not stated"),
       
  country_of_birth_agg = case_when(country_of_birth_agg == "UK" ~ "UK",
                                   country_of_birth_agg == "Non-UK" ~ "Non-UK",
                                   TRUE ~ "Missing or not stated")) %>%
  select(-is_carer,-disability)
       
  
  return(new_data)
  }


#-------------------------------------------------------------------------------------------------
#Renames and aggregates census11 variables (rename_census11_variables)
#-------------------------------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
#' @description This function renames and aggregates census 2011 variables
#' @description if census 2021 missing then we use census variables from census 2011
#' 
#' @param df = dataframe
#' @return Spark DataFrame 


rename_census11_variables <- function(data){
  
new_data <- data %>%
mutate(
  sex = case_when(sex == 1 ~ 'Males',
                  sex == 2 ~ 'Females'),
  
  Region = case_when(region_code == 'E12000001' ~ 'North East',
                     region_code == 'E12000002' ~ 'North West',
                     region_code == 'E12000003' ~ 'Yorkshire and The Humber',
                     region_code == 'E12000004' ~ 'East Midlands',
                     region_code == 'E12000005' ~ 'West Midlands',
                     region_code == 'E12000006' ~ 'East of England',
                     region_code == 'E12000007' ~ 'London',
                     region_code == 'E12000008' ~ 'South East',
                     region_code == 'E12000009' ~ 'South West',
                     region_code == 'W92000004' ~ 'Wales',
                     region_code == 'S92000003' ~ 'Scotland',
                     region_code == 'N92000002' ~ 'Northern Ireland',
                     TRUE ~ "Missing or not stated"),
  
  deprived = case_when(deprived == "1" ~ "Not deprived",
                       deprived == "2" ~ "Deprived, 1 dimension",
                       deprived == "3" ~ "Deprived, 2 dimensions",
                       deprived == "4" ~ "Deprived, 3 dimensions",
                       deprived == "5" ~ "Deprived, 4 dimensions",
                       TRUE ~ "Missing or not stated"),
                     
  ethnic_group_tb = case_when(ethpuk11 >= 01 & ethpuk11 <= 04 ~ "White", 
                              ethpuk11 >= 05 & ethpuk11 <= 08 ~ "Mixed",
                              ethpuk11 >= 09 & ethpuk11 <= 13 ~ "Asian",
                              ethpuk11 >= 14 & ethpuk11 <= 16 ~ "Black",
                              ethpuk11 >= 17 & ethpuk11 <= 18 ~ "Other",
                              TRUE ~ "Missing or not stated"),
       
#  disability_category = case_when(disability == 1 ~ "Yes - reduced a lot",
#                                  disability == 2 ~ "Yes - reduced a little",
#                                  disability == 3 ~ "Yes - not reduced at all",
#                                  disability == 'X' ~ "No",
#                                  TRUE ~ "No"),
       
  disability_binary = case_when(disability == 1 | disability == 2 ~ 1,
                                disability == 3 ~ 0,
                                TRUE ~ "Missing or not stated"),
       
  is_carer_category = case_when(carer == 1 ~ "Not a carer",
                                carer == 2 ~ "Yes, 1 to 19 hours a week",
                                carer == 3 ~ "Yes, 20 to 49 hours a week" ,
                                carer == 4 ~ "Yes, 50 or more hours a week",
                                TRUE ~ "Missing or not stated"),
       
  is_carer_binary = case_when(carer == 1 ~ 0, 
                              carer >= 2 & carer <= 4 ~ 1,
                              TRUE ~ "Missing or not stated"),
       
  ns_sec = 
  case_when(nssec >= 1 & nssec < 4 ~ "Higher managerial, administrative and professional occupations",
            nssec >= 4 & nssec < 7 ~ "Lower managerial, administrative and professional occupations",
            nssec >= 7 & nssec < 8 ~ "Intermediate occupations",
            nssec >= 8 & nssec < 10 ~ "Small employers and own account workers",
            nssec >= 10 & nssec < 12 ~ "Lower supervisory and technical occupations",
            nssec >= 12 & nssec < 13 ~ "Semi-routine occupations",
            nssec >= 13 & nssec < 14 ~ "Routine occupations",
            nssec == 14.1 ~ "Never worked",
            nssec == 14.2 ~ "Long-term unemployed",
            nssec == 15 ~ "Full-Time students",
            nssec == 16 & nssec == 17  ~ "Not classifiable",
            TRUE ~ "Missing or not stated"),
       
  religion_tb = case_when(relpuk11 == 1 ~ "No religion",
                          relpuk11 == 2 ~ "Christian",
                          relpuk11 == 3 ~ "Buddhist",
                          relpuk11 == 4 ~ "Hindu",
                          relpuk11 == 5 ~ "Jewish",
                          relpuk11 == 6 ~ "Muslim",
                          relpuk11 == 7 ~ "Sikh",
                          relpuk11 == 8 ~ "Other religion",
                          TRUE ~ "Missing or not stated"),

    
  highest_qualification = 
    case_when(hlqpuk11 == 10 ~ "No qualifications",
              hlqpuk11 == 11 ~  "Level 1",
              hlqpuk11 == 12 ~  "Level 2",
              hlqpuk11 == 13 ~  "Apprenticeship",
              hlqpuk11 == 14 ~  "Level 3",
              hlqpuk11 == 15 ~  "Level 4 and above",
              hlqpuk11 == 16 ~  "Other",
              TRUE ~ "Missing or not stated"),


  
  
    legal_partnership_status =
       case_when(marstat == 1 ~ "Never married and never registered a civil partnership",
                 marstat == 2 | marstat == 6 ~ "Married or in a registered civil partnership",
                 marstat == 3 | marstat == 7 ~ "Separated, but still legally married or still legally in a civil partnership",
                 marstat == 4 | marstat == 8 ~ "Divorced or civil partnership dissolved",
                 marstat == 5 | marstat == 9 ~ "Widowed or surviving civil partnership partner",
                 TRUE ~ "Missing or not stated"),
  
  hh_dependent_children = 
    case_when(dpchuk11 == 01 ~ "No children",
              dpchuk11 > 01 & dpchuk11 <= 07 ~  "One child",
              dpchuk11 >= 08  & dpchuk11 <= 13 ~ "Two children",
              dpchuk11 >= 14  & dpchuk11 <= 19 ~ "Three or more children",
              TRUE ~ "Missing or not stated"),
    
         
  hh_family_composition = 
       case_when(hhchuk11 >= 01 & hhchuk11 <= 02 ~ "One person household",
                 hhchuk11 >= 04 & hhchuk11 <= 15 ~ "Single family household: Couple family household",
                 hhchuk11 >= 16 & hhchuk11 <= 21 ~ "Single family household: Lone parent household",
                 hhchuk11 >= 22 & hhchuk11 <= 26 ~ "Other household type",
                 hhchuk11 == 03  ~ "Other household type",
                 TRUE ~ "Missing or not stated"),

  
  country_of_birth_agg = case_when(aggcobpuk113 == 1 ~ "UK",
                                   aggcobpuk113 == 2 ~ "Non-UK",
                                   TRUE ~ "Missing or not stated")) %>%
  
  select(-hhchuk11,-marstat,-relpuk11,-nssec,-carer,
         -disability,-ethpuk11,-hlqpuk11,-region_code,-aggcobpuk113,
        -dpchuk11)#-dpcefamuk11
  
       
  return(new_data)
  
  }
 

#-------------------------------------------------------------------------------------------------
# Employment categories 
#-------------------------------------------------------------------------------------------------
#' @description This function renames employment categories from NHS TT. 
#' 
#' @param df = dataframe
#' @param var1 = variable of interest

#' @return Spark DataFrame with fully renamed employment categories.


employment_cat_function <- function(df, var1) {

  renamed_df <- df %>%
    mutate({{var1}} := 
           case_when({{var1}} == '01' |{{var1}} == '1'~ "Employed",
                     
                     {{var1}} == '02' |{{var1}} == '2'~ "Unemployed, seeking work",
                     
                     {{var1}} == '03' |{{var1}} == '3'~ "Student",
                     
                     {{var1}} == '04' |{{var1}} == '4'~ "Long-term sick/disabled",
                     
                     {{var1}} %in% c('5','6','7','05','06','07') ~ "Unemployed, not seeking work",
                     
                     {{var1}} == '08' | {{var1}} == '8' ~ "Retired",
                     TRUE ~ "Missing or not stated"))

  return(renamed_df)
}


#-------------------------------------------------------------------------------------------------
# Source of referral categories
#-------------------------------------------------------------------------------------------------
#' @description This function categorises and cleans referral categories 
#' 
#' @param df = dataframe

#' @return Spark DataFrame with fully renamed source of referral categories.

referral_source_fun<- function(df){
  
  df1 <- df %>% 
  mutate(sourcereferral = 
       case_when(
         sourcereferral %in% c('A1', 'A2','A3','a1',
                               'a2','a3',
                               'a','A') ~'Primary Health Care',
         
         sourcereferral %in% c('B1','B2','B','b1',
                               'b2','b') ~ "Self-referral",
                 
         sourcereferral %in% c('C1','C2','C','c1',
                               'c2','c') ~ 'Other', #'Local Authority Services'

         sourcereferral %in% c('D1','D','d1','d') ~ "Other",#Employer
         
         sourcereferral %in% c('E1','e1','E2', 'e2','E3', 'e3',
                               'E4', 'e4','E5', 'e5',
                               'e','E') ~ 'Other', #'Justice System'

         sourcereferral %in% c ('G1','G2','G3','G4','g1','g2',
                                'g3','g4','g','G') ~ 'Other', #'Independent sector'
                                
         sourcereferral %in% c('H1','H2','h1','h2',
                               'h','H') ~ 'Other',#'Acute Secondary Care'

         sourcereferral %in% c('L1','L2','K1','K2','K3',
                               'K4','K5','J1','J2','J3',
                               'J4','I1','I2','L','K',
                               'J','I','l1','l2','k1','k2',
                               'k3','k4','k5','j1','j2','j3',
                               'j4','i1','i2','l','k',
                               'j','i') ~ 'Other',#'Transfers & internal referrals'

         sourcereferral %in% c('M1','M2','M3','M4','M5',
                               'M6','M','N1','N2','N','m1',
                               'm2','m3','m4','m5','m6',
                               'm','n1','n2','n','F1','F2','F3','F3',
                               'f1','f2','f3','f') ~ 'Other', 
         TRUE ~"Missing or not stated"))

return(df1)
}

#-------------------------------------------------------------------------------------------------
# Treatment types and intensity variables
#-------------------------------------------------------------------------------------------------
#' @description This function recodes,categorises and cleans therapy types. It
#'  also created low/high intensity categories
#' @param data = dataframe
#' @param variable = original variable of uncoded therapy type
#' @param variable_2 = variable name for the intensity variable

#' @return Spark DataFrame with fully renamed df

rename_treatment_opt <- function(data, variable, variable_2){

  df <- data %>% 
  
    mutate(
      {{ variable_2 }} := 
       case_when(
         {{ variable }} == '20' ~	'Low intensity',
         {{ variable }} == '21' ~	'Low intensity',
         {{ variable }} == '22' ~	'Low intensity',
         {{ variable }} == '23' ~	'Low intensity',
         {{ variable }} == '24' ~	'Low intensity',
         {{ variable }} == '25' ~	'Low intensity',
         {{ variable }} == '26' ~	'Low intensity',
         {{ variable }} == '27' ~	'Low intensity',
         {{ variable }} == '28' ~	'Low intensity',
         {{ variable }} == '29' ~	'Low intensity',
         {{ variable }} == '40' ~	'High intensity',
         {{ variable }} == '41' ~	'High intensity',
         {{ variable }} == '42' ~	'High intensity',
         {{ variable }} == '43' ~	'High intensity',
         {{ variable }} == '44' ~	'High intensity',
         {{ variable }} == '45'~	'High intensity',
         {{ variable }} == '46' ~	'High intensity',
         {{ variable }} == '47' ~	'High intensity',
         {{ variable }} == '48' ~	'High intensity',
         {{ variable }} == '49' ~	'High intensity',
         {{ variable }} == '50' ~	'High intensity',
         {{ variable }} == '51' ~	'High intensity',
          TRUE ~ 'Missing')) %>%
          
      mutate({{ variable }} := 
       case_when(
         {{ variable }} == '20' ~	'Guided Self Help', 
         {{ variable }} == '21' ~	'Non-guided Self Help', 
         {{ variable }} == '22' ~	'Guided Self Help',
         {{ variable }} == '23' ~	'Non-guided Self Help',
         {{ variable }} == '24' ~	'Behavioural Activation (Low Intensity)',
         {{ variable }} == '25' ~	'Structured Physical Activity',
         {{ variable }} == '26' ~	'Ante or post natal counselling',
         {{ variable }} == '27' ~	'Psychoeducational peer support',
         {{ variable }} == '28' ~	'Other Low Intensity',
         {{ variable }} == '29' ~	'Employment Support (Low Intensity)',
         {{ variable }} == '40' ~	'Applied relaxation',
         {{ variable }} == '41' ~	'Behavioural Activation (High Intensity)',
         {{ variable }} == '42' ~	'Couples Therapy for Depression',
         {{ variable }} == '43' ~	'Collaborative care',
         {{ variable }} == '44' ~	'Counselling for Depression',
         {{ variable }} == '45' ~	'Brief psychodynamic psychotherapy',
         {{ variable }} == '46' ~	'Eye Movement Desensitisation Reprocessing',
         {{ variable }} == '47' ~	'Mindfulness',
         {{ variable }} == '48' ~	'Other High Intensity',
         {{ variable }} == '49' ~	'Employment Support (High Intensity)',
         {{ variable }} == '50' ~	'Cognitive Behaviour Therapy (CBT)' ,
         {{ variable }} == '51' ~	'Interpersonal Psycho therapy (IPT)',
          TRUE ~ 'Missing'))
  return(df)
}

################################################################################################
#                                   DESCRIPTIVE FUNCTIONS 
################################################################################################

#-------------------------------------------------------------------------------------------------
# Simple counting & proportion function 
#-------------------------------------------------------------------------------------------------
#' @description This function groups by specific variable and counts unique ids and calculates proportions 
#' 
#' @param df = dataframe
#' @param var1 = variable of interest

#' @return Spark DataFrame with counts and proportions by chosen group


proportion_function <- function(df, var1) {

df_new <- df %>% 
  group_by({{var1}}) %>%
  summarise(count = n_distinct(census_person_id)) %>%
            mutate(total = sum(count)) %>% 
            mutate(percentage = (count/total)*100)


}




#--- Age ---#
#-------------------------------------------------------------------------------------------------
# Calculates min, max and mean function 
#-------------------------------------------------------------------------------------------------
#' @description This function calculates min, max and mean for a specific variable 
#' 
#' @param df = dataframe
#' @param var1 = variable of interest

#' @return Spark DataFrame with min, max and mean value for specific variable


# Min, max and average

min_max_mean <- function(df, var1) {

df_new <- df %>%
  select(census_person_id, {{var1}}) %>%
  sdf_drop_duplicates(cols = "census_person_id") %>%
  summarize(
    min = min({{var1}}),
    max = max({{var1}}),
    mean = mean({{var1}}),
    sd = sd({{var1}})           
  )
}


# Min, max and average - Same as above but groups by chosen 1 variable

min_max_mean_grouped <- function(df, var1,var2) {

df_new <- df %>%
  select(census_person_id, {{var1}},{{var2}}) %>%
  sdf_drop_duplicates(cols = "census_person_id") %>%
  group_by({{var2}}) %>%
  summarize(
    min = min({{ var1 }}),
    max = max({{ var1 }}),
    mean = mean({{ var1 }}),
    sd = sd({{ var1 }})           
  )
}

# Min, max and average - Same as above but groups by chosen 2 variables

min_max_mean_2_grouped <- function(df, var1,var2,var3) {

df_new <- df %>%
  select(census_person_id, {{var1}},{{var2}}, {{var3}}) %>%
  sdf_drop_duplicates(cols = "census_person_id") %>%
  group_by({{var2}}, {{var3}}) %>%
  summarize(
    min = min({{var1}}),
    max = max({{var1}}),
    mean = mean({{var1}}),
    sd = sd({{var1}})           
  )
}

#-------------------------------------------------------------------------------------------------
# Simple counts and proportion for a list of arguments
#-------------------------------------------------------------------------------------------------
#' @description This function loops through list of categorical variables.
#               It groups by specific categorical variable and counts unique ids, it also calculates proportions.
#' 
#' @param df = dataframe
#' @param argument_list = a list with variables of interest

#' @return Spark DataFrame with counts and proportions grouped by specific variable in the argument list 



# Function - Calculates counts and proportions for given variable in the argument list and given data set
proportion_for_var_list <- function(df, argument_list) {
    df_new <- df %>% sdf_drop_duplicates(cols = "census_person_id")
  
  
    # Initialize an empty list to store the results
  result_list <- list()
  
  
  
  for (var in argument_list) {
 
  result <- df_new %>%
    group_by(Category_name = !!sym(var)) %>%
    summarize(Count = n()) %>%
            mutate(total = sum(Count)) %>% 
            mutate(percentage = (Count/total)*100) %>%
    mutate(Variable_names = var)  %>%
  select(Variable_names, everything())
    
  result_list[[var]] <- result
    
  }
  
    # Combine the results into a single data frame
  result_df <- do.call(sdf_bind_rows, result_list)
    
  return(result_df)
}


#-------------------------------------------------------------------------------------------------
# Calculates counts and proportions for exposure groups by categorical variable 
#-------------------------------------------------------------------------------------------------
#' @description This function loops through list of categorical variables.
#               It groups by exposure groups and specific categorical variable and counts unique ids, 
#               It also calculates proportions.
#' 
#' @param df = dataframe
#' @param argument_list = a list with variables of interest

#' @return Spark DataFrame with counts and proportions grouped by specific variable in the argument list 



proportion_for_exposure_by_var_list <- function(df, argument_list) {
    df_new <- df %>% sdf_drop_duplicates(cols = "census_person_id")
  
  
    # Initialize an empty list to store the results
  result_list <- list()
  
  
  
  for (var in argument_list) {
 
  result <- df_new %>%
    group_by(exposure_groups, Category_name = !!sym(var)) %>%
    #group_by(Category_name = !!sym(var)) %>%
    summarize(Count = n()) %>%
            mutate(total = sum(Count)) %>% 
            mutate(percentage = (Count/total)*100) %>%
    mutate(Variable_names = var)  %>%
  select(Variable_names, everything())
    
  result_list[[var]] <- result
    
  }
  
    # Combine the results into a single data frame
  result_df <- do.call(sdf_bind_rows, result_list)
    
  return(result_df)
}



#-------------------------------------------------------------------------------------------------
# Calculates counts and proportions for categorical variable by exposure group
#-------------------------------------------------------------------------------------------------
#' @description This function loops through list of categorical variables.
#               It groups by categorcial variable then by exposure groups and counts unique ids, 
#               It also calculates proportions.
#' 
#' @param df = dataframe
#' @param argument_list = a list with variables of interest

#' @return Spark DataFrame with counts and proportions grouped by specific variable in the argument list and exposure group


proportion_for_var_by_exposure <- function(df, argument_list) {
    df_new <- df %>% sdf_drop_duplicates(cols = "census_person_id")
  
  
    # Initialize an empty list to store the results
  result_list <- list()
  
  
  
  for (var in argument_list) {
 
  result <- df_new %>%
    group_by(Category_name = !!sym(var),exposure_groups) %>%
    #group_by(Category_name = !!sym(var)) %>%
    summarize(Count = n()) %>%
            mutate(total = sum(Count)) %>% 
            mutate(percentage = (Count/total)*100) %>%
    mutate(Variable_names = var)  %>%
  select(Variable_names, everything())
    
  result_list[[var]] <- result
    
  }
  
    # Combine the results into a single data frame
  result_df <- do.call(sdf_bind_rows, result_list)
    
  return(result_df)
}

#-------------------------------------------------------------------------------------------------
# Simple counting function 
#-------------------------------------------------------------------------------------------------
#' @description This function groups by specific variable and counts unique ids
#' 
#' @param df = dataframe
#' @param var1 = variable of interest

#' @return Spark DataFrame with counts by chosen group


count_function <- function(df, var1){
  
  df_new <- df %>%
    group_by({{ var1 }}) %>%
    summarise(count = n_distinct(census_person_id)) %>%
    arrange({{ var1 }})
}



#-------------------------------------------------------------------------------------------------
# Graph 1 - Monthly average pay for all 
#-------------------------------------------------------------------------------------------------

#' Graph function to visualise monthly average pay for all patients in NHS Talking Therapies. 
#' Select year and df you want to apply it to. 
#'  
#' @param data = monthly dataset 
#' @param year = year of data you are using needed for file names 
#' @return saves ggplot and underlying data 


graph_1_function <- function(df, name){
  
  temp_df <- df %>% 
    mutate(pay = as.numeric(pay)) %>%
    group_by(month_label) %>% 
    summarise(average_pay = mean(pay))
  
  write.csv(temp_df, 
              paste0(outputs_folder,name, "_monthly_average_pay_for_the_whole_population",Sys.Date(),".csv"))
  
  temp_df <- collect(temp_df)
  
  # Convert 'date' to a character string with leading zeros for the month
    temp_df$date_label <- sub("(\\b\\d{4}-)(\\d{1}\\b)", "\\10\\2", temp_df$month_label)
  
  # plot graph
    g <- temp_df %>% 
    ggplot(aes(x = date_label, 
               y = average_pay)) +
    geom_line(aes(group=1),
              color = "cyan3",
              size= 0.5)  + 
    #ylim(1000,3000) +
    labs(title="Monthly average pay for the whole population",
         x ="Date", 
         y = "Average monthly pay per person") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  
  return(g)
#  #save graph
#    ggsave(paste0(outputs_folder,year, "_monthly_pay_all_",Sys.Date(),".pdf"), 
#                       width = 10, height = 4, units = "in")

}


#-------------------------------------------------------------------------------------------------
# Graph 2 - Monthly average pay broken down by exposure groups 
#-------------------------------------------------------------------------------------------------

#' Graph function to visualise monthly average pay for all patients in NHS Talking Therapies 
#'  broken down by exposure groups. 
#' Select year and df you want to apply it to. 
#'  
#' @param data = monthly dataset 
#' @param year = year of data you are using needed for file names 
#' @return saves ggplot and underlying data 


graph_2_function <- function(df, vars, year,  name){
  
  temp_df <- df %>% 
    mutate(pay = as.numeric(pay)) %>%
    group_by(month_label, {{ vars }}) %>% 
    summarise(average_pay = mean(pay))
  
  
  write.csv(temp_df, 
              paste0(outputs_folder,year,"_monthly_average_pay_by_", name , Sys.Date(),".csv"))
  
  temp_df <- collect(temp_df)

  ## Convert 'date' to a character string with leading zeros for the month
  temp_df$date_label <- sub("(\\b\\d{4}-)(\\d{1}\\b)", "\\10\\2", temp_df$month_label) 
  
  # plot graph
  g <- temp_df %>% 
  ggplot(aes(x = date_label, 
             y = average_pay, 
             colour = {{ vars }},
             group = {{ vars }})) +
  geom_line(alpha=0.6)  + 
  labs(title="Monthly average pay broken down by exposure groups",
       x ="Date", 
       y = "Average monthly pay") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:')
   
return(g)
#    ggsave(paste0(outputs_folder,year, "_monthly_average_pay_by_", name ,Sys.Date(),".pdf"), 
#                       width = 10, height = 4, units = "in")
    
    
}


#-------------------------------------------------------------------------------------------------
# Graph 3 - average pay by time to referral broken down by exposure groups 
#-------------------------------------------------------------------------------------------------

#' Graph function to visualise average pay for all patients in NHS Talking Therapies 
#'  by time to referral broken down by exposure groups. 
#' Select year and df you want to apply it to. 
#'  
#' @param data = monthly dataset 
#' @param year = year of data you are using needed for file names 
#' @return saves ggplot and underlying data 


graph_3_function <- function(df,vars,year,name){
   
  temp_df <- df %>% 
    mutate(pay = as.numeric(pay)) %>%
    mutate(t_referral = as.numeric(t_referral)) %>%
    #filter(t_referral < 35) %>% # added this as graph has outliers towards the end 
    group_by(t_referral, {{ vars }}) %>% 
    summarise(average_pay = mean(pay)) 
  
    write.csv(temp_df, paste0(outputs_folder,year,
                              "_monthly_pay_by_t_to_referral_by_",name,Sys.Date(),".csv"))  
  g <- temp_df %>% 
  ggplot(aes(x = t_referral, 
           y = average_pay, 
           colour = {{ vars }},
           group = {{ vars }})) +
  geom_line(alpha=0.6)  + 
  labs(title="Monthly pay per time to referral by groups",
       x ="Time to referral in months", 
       y = "Average monthly pay per person") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position="bottom") + 
  labs(color='Legend:') +
  geom_vline(xintercept = 0, linetype="dashed", 
             color = "black")
 return(g) 
  
#    ggsave(paste0(outputs_folder,"monthly_pay_by_t_to_referral_by_",name,Sys.Date(),".pdf"), 
#                       width = 10, height = 4, units = "in")
#    


 
} 


#-------------------------------------------------------------------------------------------------
# Quality assurance checks
#-------------------------------------------------------------------------------------------------
# This section contains functions that are used in a script "3_quality_checks.R"
# There are 2 sections: > first contains specific functions to run on sample data; 
#                       > second has specific functions for spark data 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 1. QUALITY CHECKS ON A SAMPLE  #                                                                 #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#


#----- Function to get row count and display result ------#
getRowCount <- function(df, var_name) {
  count <- df %>% summarise(Row_count = n())

  result <- count %>% mutate(Var_name = {{var_name}})
}




#-------- Function grouping by 2 specified variables and filtering those where the count is more than 1 ---------#
# E.g. you can find if there is only one row per individual and month

count_var2_more_than_1 <- function(df, var1, var2, description_text){
  
  new_df <- df %>%
    group_by({{var1}}, {{var2}}) %>%
    summarize(count = n()) %>% 
    mutate(duplicate_flag = ifelse(count > 1,1,0)) %>% ungroup() %>%
    summarise(count_of_duplicates=sum(duplicate_flag)) %>% 
    mutate(qa_outcome = ifelse(count_of_duplicates == 0, 'PASS','FAIL'))%>% 
    mutate(qa_description = description_text)

return(new_df)
  }



#----------- One referral date per person ------#

unique_var_per_person <- function(df, var1, var2, description_text) {
  
new_df <- df %>% 
  select({{var1}}, {{var2}}) %>%
 # distinct() %>%

  group_by({{var1}}) %>%
  summarize(count = n_distinct({{var2}})) %>% 
  mutate(duplicate_flag = ifelse(count > 1,1,0)) %>% ungroup() %>%
  summarise(count_of_duplicates = sum(duplicate_flag)) %>% 
  mutate(qa_outcome = ifelse(count_of_duplicates == 0, 'PASS','FAIL'))%>% 
  mutate(qa_description = description_text)
  
  return(new_df)
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
# 2. QA CHECKS ON THE WHOLE LINKED DATA (SPARK FORMAT) #                                                                  #
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#

#----- Function to get row count and display result ------#
getRowCount_spark <- function(df, var_name) {
  count <- sdf_nrow(df)

  result <- data.frame(Var_name = {{var_name}}, Count = count )
}


#----------- Function to get count of distinct values ------------#

getDistinctCount_spark <- function(df) {

  argument_list <- colnames(df)
  
    # Initialize an empty list to store the results
  result_list <- list()

 for (var in argument_list) {

   result <- df %>% sdf_drop_duplicates({{var}}) %>% 
          summarize(Count_distinct = n()) %>%
   mutate(Var_name = var)


   result_list[[var]] <- result
   
 }
   
   # Combine the results into a single data frame
  result_df <- do.call(sdf_bind_rows, result_list)
    
  return(result_df)
}


#-------- Function grouping by 2 specified variables and filtering those where the count is more than 1 ---------#
# E.g. you can find if there is only one row per individual and month

count_var2_more_than_1_spark <- function(df, var1, var2, description_text) {

new_df <- df %>%
  group_by({{var1}}, {{var2}}) %>%
  summarize(count = n()) %>%
  mutate(duplicate_flag = ifelse(count > 1,1,0)) %>% ungroup() %>%
  summarise(count_of_duplicates = sum(duplicate_flag)) %>% 
  mutate(qa_outcome = ifelse(count_of_duplicates == 0, 'PASS','FAIL'))%>% 
  mutate(qa_description = description_text)
  
  
  print(new_df)
}



#----------- One referral date per person ------#

unique_var_per_person_spark <- function(df, var1, var2, description_text) {
  
new_df <- df %>% 
  select({{var1}}, {{var2}}) %>%
  sdf_drop_duplicates() %>%

  group_by({{var1}}, {{var2}}) %>%
  summarize(count = n()) %>% 
  mutate(duplicate_flag = ifelse(count > 1,1,0)) %>% ungroup() %>%
  summarise(count_of_duplicates = sum(duplicate_flag)) %>% 
  mutate(qa_outcome = ifelse(count_of_duplicates == 0, 'PASS','FAIL'))%>% 
  mutate(qa_description = description_text)
  
  return(new_df)
}






#------- Checks NAs for all variables in a dataset -------# 



check_na <- function(df, col_names) {

  col_names <- colnames(df)
     
    # Initialize an empty list to store the results
  result_list <- list()
  
    
  for (var in col_names) {
 
  result <- df %>%
   filter(is.na(!!sym(var))) %>%
   summarize(Count = n()) %>%
   mutate(Variable_name = var)
    
    result_list[[var]] <- result
    
  }
  
  # Combine the results into a single data frame
  result_df <- do.call(sdf_bind_rows, result_list)
    
  return(result_df)
}


# --------- Check min, max, mean for all variables in data frame ---------------#

min_max_mean_spark <- function(data, col_names) {
  
  data <- data %>% select(census_person_id,
                          first_therapy_date,
                          last_therapy_date,
                          assessment_date,
                          month, 
                          ref_date, 
                          referral_date)
  
   col_names <- colnames(data)
  
  # Initialize an empty list to store results
  results_list <- list()
  
  # Iterate over column names
  for (var in col_names) {
    if (var %in% c("first_therapy_date","last_therapy_date",
                   "assessment_date","month", "ref_date", "referral_date")) {
      result <- data %>%
      select(census_person_id, {{var}}) %>%
        summarize(
          min_value = min(!!sym(var)),
          max_value = max(!!sym(var))
        ) %>%
      mutate(Variable_name = var) #%>%
        
    } else {
      result <- data %>%
      select(census_person_id, {{var}}) %>%
        summarize(
          mean_value = mean(!!sym(var)),
          min_value = min(!!sym(var)),
          max_value = max(!!sym(var))
        ) %>%
        mutate(Variable_name = var) #%>%
    }
    
    # Add the result to the list
    results_list[[var]] <- result
  }
  
  # Bind the results together
  final_result <- do.call(sdf_bind_rows, results_list)
  
  # Return the final result
  return(final_result)
}



#-------------------------------------------------------------------------------------------------
# memory efficient collect
#-------------------------------------------------------------------------------------------------

memory_efficient_collect <- function(data) {
  
  # Get number of partitions the spark data frame is on
  num_partitions <- sdf_num_partitions(data)

  # Get user environment name
  username <- Sys.getenv('HADOOP_USER_NAME')
  path <- paste0('/user/', username, '/temp')
  
  start_time <- Sys.time()

  # Save the spark data frame partitions to hdfs in .rds format
  spark_write_rds(
    data,
    dest_uri = paste0(path,"/temp-part-{partitionId}.rds"))
  
  end_time <- Sys.time()
  
  cat(paste("",
            "Saving to HDFS time:",
            paste0(round(difftime(end_time, start_time, units = "mins"), 2), " mins"),
            "",
            sep = "\n"))
  
  start_time <- Sys.time()

  # Create a temporary local data folder
  dir.create("Data/temp")

  # Run `hadoop fs -copyToLocal` to download RDS files from HDFS to local file system
  for (partition in seq(num_partitions) - 1) {
  
    hdfs_path <- paste0(path,"/temp-part-%d.rds")
    local_path <- "Data/temp/temp-part-%d.rds"
  
    system2(
      "hadoop",
      c("fs", "-copyToLocal",
        sprintf(hdfs_path, partition),
        sprintf(local_path, partition)))
  
  }
  
  end_time <- Sys.time()
  
  cat(paste("",
            "Moving to local from HDFS time:",
            paste0(round(difftime(end_time, start_time, units = "mins"), 2), " mins"),
            "",
            sep = "\n"))
  
  start_time <- Sys.time()

  # Post-process RDS outputs
  partitions <- lapply((seq(num_partitions) - 1), function(partition) {
    
      collect_from_rds(sprintf("Data/temp/temp-part-%d.rds", partition))
      #readRDS(sprintf("Data/temp/temp-part-%d.rds", partition))
    
    })

  # Optionally, call `rbind()` to combine data from all partitions into a single R dataframe
  local_df <- do.call(rbind, partitions)
  
  end_time <- Sys.time()
  
  cat(paste("",
            "Read in from local time:",
            paste0(round(difftime(end_time, start_time, units = "mins"), 2), " mins"),
            "",
            sep = "\n"))

  # Clean up
  
  start_time <- Sys.time()
  
  # Delete the temporary hdfs and local .rds partition files
  for (partition in seq(num_partitions) - 1) {
  
#    # HDFS
#    cmd <- paste0("hdfs dfs -rm -r -skipTrash /user/", username, "/temp/temp-part-", partition, ".rds")
#    system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
    
    # Local
    file.remove(sprintf("Data/temp/temp-part-%d.rds", partition))
  
  } 
  
  # HDFS
  cmd <- paste0("hdfs dfs -rm -r -skipTrash /user/", username, "/temp")
  system(cmd, ignore.stdout = TRUE, ignore.stderr = TRUE)
  
  end_time <- Sys.time()
  
  cat(paste("",
            "Clean up time:",
            paste0(round(difftime(end_time, start_time, units = "mins"), 2), " mins"),
            "",
            sep = "\n"))
  
  return(local_df)
  
}

#-------------------------------------------------------------------------------------------------
# Regression function - fixed effect model
#-------------------------------------------------------------------------------------------------
#' @description Function to run fixed effect model and produce graphs & data. 
#' Format: felm(y ~ x1 + x2 | fe1 + fe2 | (x3 | x4 ~ z1 + z2) | cl1, data = df)
#' @description x1 + x2 = time varying variables 
#' @description  (x3 | x4 ~ z1 + z2) instruments= we don't have any in this analysis so 0
#' @param data =  dataframe 
#' @param outcome =  outcome variable (y)
#' @param exposure_var =  exposure variable (x1 + x2|) 
#' @param control_vars =  control variables for the model (x1 + x2|)
#' @param fixed_effect_vars =  variable/s to be included as fixed effects(|fe1 + fe2|)
#' @param cluster =  variable for Standard Errors (if none then 0)
#' @param name =  name for the graph/csv file
#' @param y_aes =  Y axis title for the graph
#' @param x_aes =  X axis title for the graph
#'
#' @return csv file with result (underlying data for the graph)
#' @return graph with plotted results


fixed_effect_function <- function(data, 
                                  outcome,
                                  exposure_var,
                                  control_vars,
                                  fixed_effect_vars,
                                  cluster,
                                  weight = "none",
                                  name,
                                  y_aes,
                                  x_aes,
                                  min,
                                  max,
                                  y_break,
                                  title,
                                  x_lower = -4,
                                  x_upper = 6,
                                  plot_type= "line",
                                  vline,
                                  h=4){
  
  if(weight == "none"){
     w = rep(1, nrow(data))
  
  }else{
     w = data[[weight]]
  }
  
  if(outcome == "pay"){
      colour = "#00aaff" 
    } else{
      colour = "#8b0850"
    }
  
  model<-lfe::felm(as.formula(
                paste0(outcome,"~",exposure_var,control_vars,"|",fixed_effect_vars,"|0|",cluster)),
                       data = data,
                  weights = w)
    
  model_df <- broom::tidy(model, conf.int=T) %>%
    mutate(t= as.numeric(gsub(exposure_var, "", term)))
  
  
  if(outcome == "in_work"){
    
    model_df$estimate = model_df$estimate*100
    model_df$conf.low = model_df$conf.low*100
    model_df$conf.high = model_df$conf.high*100
  } else{
    model_df$estimate = model_df$estimate
    model_df$conf.low = model_df$conf.low
    model_df$conf.high = model_df$conf.high
  }

  model_df %>% print()

   # Plot the model
    if (plot_type == "line"){
      model_plot <- ggplot(data = filter(model_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate) +
      geom_line()+
      geom_ribbon(alpha =0.1)+
      geom_hline(yintercept=0, linetype="dotted")+
      geom_vline(xintercept=vline)+
      labs(x= x_aes, y = y_aes,
           title = paste0(title)) +
      theme_bw() +
      theme(plot.title = element_text(size = 15))
    }
    
    if (plot_type == "dot"){
      model_plot <- ggplot(data = filter(model_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate, 
          ymin = ymin, 
          ymax = ymax)+
      geom_point()+
      geom_errorbar(width =0.1)+
      geom_hline(yintercept=0, linetype="dotted")+
      geom_vline(xintercept=vline)+
      labs(x= x_aes, y = y_aes,
           title = paste0(title))+
      scale_x_continuous(breaks = seq(x_lower, x_upper, by = 2))+
      theme_bw()+
      theme(plot.title = element_text(size = 15))
    }
  
    if (plot_type == "bar"){
      model_plot <- ggplot(data = filter(model_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate,, 
          ymin = conf.low, 
          ymax = conf.high,
          fill = colour)+
      geom_bar(stat = 'identity')+
      geom_errorbar(width = 0.1)+
      geom_hline(yintercept = 0, linetype = "dotted")+
      geom_vline(xintercept = vline)+
      labs(x= x_aes, y = y_aes,
           title = paste0(title))+
      scale_x_continuous(breaks = seq(x_lower, x_upper, by = 1))+
      scale_y_continuous(limits = c(min ,max), breaks = seq(min ,max, y_break))+

      scale_fill_manual(values = c(colour))+
      theme_bw()+ theme(legend.position="none") +
      theme(plot.title = element_text(size = 15))
  }
  model_plot %>% print()
  
  ggsave(paste0(outputs_folder,name,"_",  weight, "_", Sys.Date(),".png"), 
          width = h*4/3, height = h, units = "in")
    
  write.csv(model_df, paste0(outputs_folder,
                            name,"_", weight, "_", Sys.Date(),".csv"))
}



#-------------------------------------------------------------------------------------------------
# Breakdowns by sociodemogrphics; Regression function with - fixed effect model
#-------------------------------------------------------------------------------------------------
#' @description Function to run fixed effect model and produce graphs & data brokendown by variables
#' and its levels. 


#' @param data =  dataframe 
#' @param category = variable/characteristic from the data e.g. sex, ethnicity
#' @param outcome =  outcome variable e.g. pay
#' @param exposure_var = exposure variable (x1 + x2|) 
#' @param control_vars =  control variables for the model (x1 + x2|)
#' @param fixed_effect_vars =  variable/s to be included as fixed effects(|fe1 + fe2|)
#' @param cluster =  variable for Standard Errors (if none then 0)
#' @param weight = "none",
#' @param y_aes =  Y axis title for the graph
#' @param x_aes =  X axis title for the graph
#' @param x_lower = -4,
#' @param x_upper = 6,
#' @param title = Graph title
#' @param vline
#' @param plot_type= "line",
#' @param name =  name for the graph/csv file
#'
#' @return excel file with all models for each level within the variable (saved in different tabs)
#' @return PFD file with all plots for each level within the variable


regression_with_breakdowns_function <- function(data, 
                          category, 
                          outcome,
                          exposure_var,
                          control_vars,
                          fixed_effect_vars,
                          cluster,
                          weight = "none",
                          y_aes,
                          x_aes,
                          x_lower = -4,
                          x_upper = 6,
                          title,
                          vline,
                          plot_type= "line",
                          name,
                          ipw_vars) { 
  
  #Identify levels within variable
  levels <- unique(data[[category]]) 
  
  #open an empty pdf file to save plots
  pdf(paste0(outputs_folder,"regression_graphs_",category,"_", outcome,"_",weight,"_", Sys.Date(), ".pdf"), 
                       width = 10, height = 4)

  # Open an empty Excel file to save models
  wb <- createWorkbook() 
  
  # Open an empty list to store summary data frames 
  models <- list() 
  summary_list <- list() 

  #Loop
  
  for (level in levels) {
    
    # subset data for the current level
    subset_df <- data[data[[category]] == level, ]
    
      # here create IPWs for each stratified data
  
    subset_df_0 <- subset_df %>% filter(t_referral == 0)
  
    model_reg <- glm(as.formula(paste0("treatment", "~", ipw_vars)),
                   family = binomial,
                   data = subset_df_0)
                           

    subset_df_0$p <- predict(model_reg, subset_df_0, type="response")
    subset_df_0$ipw <- ifelse(subset_df_0$treatment == 1,
                            1 /subset_df_0$p,
                            1/(1-subset_df_0$p))
  
    subset_df <- subset_df %>%
      left_join(select(subset_df_0,  
                     census_person_id, ipw), by="census_person_id")#%>%
    #mutate(ipw  = ifelse(!is.na(ipw),  ipw*sampling_weight, ipw))

    rm(model_reg,subset_df_0)
    
    
    if(weight == "none"){
      w = rep(1, nrow(subset_df))
    } else{
      w = subset_df[[weight]]
    }
    
  if(outcome == "pay"){
      colour = "#00aaff" 
    } else{
      colour = "#8b0850"
    }
    # run the regression model
    model <- lfe::felm(as.formula(
      paste0(outcome,"~",exposure_var,control_vars,"|",fixed_effect_vars,"|0|",cluster)),
                       data = subset_df,
                       weights = w) 
    
    summary(model) %>% print()
    
    models[[paste(category, level, sep = "_")]] <- model

    model_df <- broom::tidy(model, conf.int=T) %>%
      mutate(t = as.numeric(gsub(exposure_var, "", term)),
             category = as.character(level))
    
    if(outcome == "in_work"){
    
      model_df$estimate = model_df$estimate*100
      model_df$conf.low = model_df$conf.low*100
      model_df$conf.high = model_df$conf.high*100
    } else{
      model_df$estimate = model_df$estimate
      model_df$conf.low = model_df$conf.low
      model_df$conf.high = model_df$conf.high
    }
    
    summary_list[[level]] <- model_df 
    
    # Combine all summary data frames into one 
    combined_summary_df <- bind_rows(summary_list) 
  
    
    #Add new tab to excel
    addWorksheet(wb, sheetName = as.character(level)) 
  
    # Write the model table to the worksheet 
    writeData(wb, sheet = as.character(level), x = model_df)
    
   if (plot_type == "line"){
      model_plot <- ggplot(data = filter(model_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate, 
          ymin = conf.low, 
          ymax = conf.high)+
      geom_line()+
      geom_ribbon(alpha =0.1)+
      geom_hline(yintercept=0, linetype="dotted")+
      geom_vline(xintercept=vline)+
      labs(x= x_aes, y = y_aes,
           title = paste0(title," ", level)) +
      theme_bw() +
      theme(plot.title = element_text(size = 15))
    }
if (plot_type == "bar"){
      model_plot <- ggplot(data = filter(model_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate, 
          ymin = conf.low, 
          ymax = conf.high,
          fill = colour)+
      geom_bar(stat = 'identity')+
      geom_errorbar(width = 0.1)+
      geom_hline(yintercept = 0, linetype = "dotted")+
      geom_vline(xintercept = vline)+
      labs(x= x_aes, y = y_aes,
           title = paste0(title," ", level))+
      scale_x_continuous(breaks = seq(x_lower, x_upper, by = 1))+
      scale_fill_manual(values = c(colour))+
      theme_bw()+ theme(legend.position="none") +
      theme(plot.title = element_text(size = 15))
  }
    
    #Write plots to pdf
    print(model_plot)
  }
  dev.off()

  if(outcome == "pay"){
      colour = "#00aaff" 
    } else{
      colour = "#8b0850"
    }
  
    # Plot the model
    if (plot_type == "line"){
      model_plot <- ggplot(data = filter(combined_summary_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate, 
          ymin = conf.low, 
          ymax = conf.high)+
      geom_line()+
      geom_ribbon(alpha =0.1)+
      geom_hline(yintercept=0, linetype="dotted")+
      geom_vline(xintercept=vline)+
      facet_wrap(~ category) + 
      labs(x= x_aes, y = y_aes,
           title = paste0(title))+
      theme_bw() +
      theme(plot.title = element_text(size = 15))
    }
    
    if (plot_type == "dot"){
      model_plot <- ggplot(data = filter(combined_summary_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate, 
          ymin = conf.low, 
          ymax = conf.high)+
      geom_point()+
      geom_errorbar(width =0.1)+
      geom_hline(yintercept=0, linetype="dotted")+
      geom_vline(xintercept=vline)+
      facet_wrap(~ category) + 
      labs(x= x_aes, y = y_aes,
           title = paste0(title))+
      scale_x_continuous(breaks = seq(x_lower, x_upper, by = 2))+
      theme_bw()+
      theme(plot.title = element_text(size = 15))

    }
  
    if (plot_type == "bar"){
      model_plot <- ggplot(data = filter(combined_summary_df, !is.na(t),t>= x_lower, t <= x_upper)) +
      aes(x = t,
          y = estimate, 
          ymin = conf.low, 
          ymax = conf.high,
          fill = colour)+
      geom_bar(stat = 'identity')+
      geom_errorbar(width = 0.1)+
      geom_hline(yintercept = 0, linetype = "dotted")+
      geom_vline(xintercept = vline)+
      facet_wrap(~ category) + 
      labs(x= x_aes, y = y_aes,
           title = paste0(title))+
      scale_x_continuous(breaks = seq(x_lower, x_upper, by = 1))+
      scale_fill_manual(values = c(colour))+
      theme_bw() + theme(legend.position="none") +
      theme(plot.title = element_text(size = 15))   
    }
  
  
  ggsave(paste0(outputs_folder,
                "combined_regression_graphs_", 
                outcome, "_", category,"_",weight,"_", Sys.Date(),".png"), model_plot)  
  
  #Save Excel file with results
  saveWorkbook(wb, 
               paste0(outputs_folder,"regression_graphs_",category,"_", outcome,"_",weight,"_",Sys.Date(),".xlsx"),
               overwrite = TRUE)
}


#-------------------------------------------------------------------------------------------------
# Function to load data from HDFS 
#-------------------------------------------------------------------------------------------------
#

# add date to function 
load_rds_function <- function(hdfs_path, table_name, outputs_folder) {
  
  # Load data into local
  #system(paste0("hadoop fs -get ",hdfs_path, table_name,".RDS", " ",outputs_folder, table_name,".RDS"))
  system(paste0("hdfs dfs -get ",hdfs_path, table_name,".RDS", " ",outputs_folder, table_name,".RDS"))
  
  # Load data from local to session
  rds_df <- readRDS(paste0(outputs_folder, table_name,".RDS"))
   
  
  # Delete the data from local to free memory
  file.remove(paste0(outputs_folder,table_name,".RDS"))
  
  
  return(rds_df)
  
}

#-------------------------------------------------------------------------------------------------
# Function to save RDS data in HDFS - 1 filter 
#-------------------------------------------------------------------------------------------------
#
save_rds_1 <- function(data, 
                       variable_list, 
                       treatment_group, 
                       sex_group, 
                       age_band, 
                       outputs_folder,
                       hdfs_path,
                       date){
  
  table <- df_qtr %>% 
    select(variable_list) %>%
    filter(exposure_groups == treatment_group & sex == sex_group) %>% 
    filter(age_bands == age_band) %>% 
    collect()
  
  age_band <- gsub("-","_",age_band)

  table_name = paste0(age_band,"_", treatment_group, "_", sex_group,"_", date)

  # Save temporary RDS in local
  saveRDS(table, paste0(outputs_folder, table_name,".RDS"))

  # Save table in HDFS
#  system(paste0("hadoop fs -put ",
#                outputs_folder,table_name,".RDS", " ",hdfs_path,table_name,".RDS"))
  system(paste0("hdfs dfs -put ",
                outputs_folder,table_name,".RDS", " ",hdfs_path,table_name,".RDS"))
  # Remove from local 
  file.remove(paste0(outputs_folder,table_name,".RDS"))
  rm(table)
  gc()
  
}



#-------------------------------------------------------------------------------------------------
# Function to save RDS data in HDFS - 2 filter 
#-------------------------------------------------------------------------------------------------

save_rds_2 <- function(data, 
                       variable_list, 
                       treatment_group, 
                       sex_group, 
                       age_band,
                       source1,
                       #sample,
                       outputs_folder,
                       hdfs_path,
                       date){
  
  table <- df_qtr %>% 
    select(variable_list) %>%
    filter(exposure_groups == treatment_group & sex == sex_group) %>% 
    filter(age_bands == age_band) %>% 
    filter(sourcereferral == source1) %>%
    #inner_join(sample, by = "census_person_id") %>%
    collect()
  
  age_band <- gsub("-","_",age_band)
  source1 <- substr(source1, start=1, stop=3)

  table_name = paste0(age_band,"_", treatment_group, "_", sex_group,"_",source1,"_", date)

  # Save temporary RDS in local
  saveRDS(table, paste0(outputs_folder, table_name,".RDS"))

  # Save table in HDFS
  system(paste0("hdfs dfs -put ",
                outputs_folder,table_name,".RDS", " ",hdfs_path,table_name,".RDS"))

  # Remove from local 
  file.remove(paste0(outputs_folder,table_name,".RDS"))
  rm(table)
  gc()
  
}


#-------------------------------------------------------------------------------------------------
# Function to save RDS data in HDFS - 3 filter 
#-------------------------------------------------------------------------------------------------

save_rds_2 <- function(data, 
                       variable_list, 
                       treatment_group, 
                       sex_group, 
                       age_band,
                       source1,
                       #sample,
                       outputs_folder,
                       hdfs_path,
                       date){
  
  table <- df_qtr %>% 
    select(variable_list) %>%
    filter(exposure_groups == treatment_group & sex == sex_group) %>% 
    filter(age_bands == age_band) %>% 
    filter(sourcereferral == source1) %>%
    collect()
  
  age_band <- gsub("-","_",age_band)
  source1 <- substr(source1, start=1, stop=3)

  table_name = paste0(age_band,"_", treatment_group, "_", sex_group,"_",source1,"_", date)

  # Save temporary RDS in local
  saveRDS(table, paste0(outputs_folder, table_name,".RDS"))

  # Save table in HDFS
  system(paste0("hdfs dfs -put ",
                outputs_folder,table_name,".RDS", " ",hdfs_path,table_name,".RDS"))

  # Remove from local 
  file.remove(paste0(outputs_folder,table_name,".RDS"))
  rm(table)
  gc()
  
}
