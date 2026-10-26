#------------------------------------------------------------------------------------------
### Setting up session
#------------------------------------------------------------------------------------------

library(sparklyr)
library(dplyr) 
library(tidyverse)
library(DBI)
library(lubridate)
library(openxlsx) 

xl_config <- spark_config()
xl_config$spark.executor.memory <- "20g"
xl_config$spark.yarn.executor.memoryOverhead <- "2g"
xl_config$spark.executor.cores <- 5
xl_config$spark.dynamicAllocation.enabled <- "true"
xl_config$spark.dynamicAllocation.maxExecutors <- 12
xl_config$spark.sql.shuffle.partitions <- 240
xl_config$spark.sql.session.timeZone = "GMT"
xl_config$spark.sql.legacy.timeParserPolicy = "LEGACY"
xl_config$spark.sql.parquet.datetimeRebaseModeInRead = "CORRECTED"

xl_config$spark.sql.parquet.int96RebaseModeInRead
xl_config$spark.sql.parquet.int96RebaseModeInRead = "CORRECTED"

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
# Load data; NHS Talking Therapies Data, Census, deaths, look up table & hmrc
#------------------------------------------------------------------------------------------

# Load NHS TT
nhs_tt_20_21 <- load_nhs_tt_data_function(data_table = "iapt_referral_20_21") 
nhs_tt_19_20 <- load_nhs_tt_data_function(data_table = "iapt_referral_19_20")
nhs_tt_18_19 <- load_nhs_tt_data_function(data_table = "iapt_referral_18_19")
nhs_tt_17_18 <- load_nhs_tt_data_function(data_table = "iapt_referral_17_18_std")
nhs_tt_16_17 <- load_nhs_tt_data_function(data_table = "iapt_referral_16_17_std")
nhs_tt_15_16 <- load_nhs_tt_data_function(data_table = "iapt_referral_15_16_std")


# Employment information from NHS TT
employment_df  <- sdf_sql(sc,
    "SELECT DISTINCT ic_pathway_id, empsupportind, empstatus, appointment
        FROM project_space..iapt_appointment_20_21
          UNION ALL
    SELECT DISTINCT ic_pathway_id, empsupportind, empstatus, appointment
        FROM project_space..iapt_appointment_19_20
          UNION ALL
    SELECT DISTINCT ic_pathway_id, empsupportind, empstatus, appointment
        FROM project_space..iapt_appointment_18_19
          UNION ALL
    SELECT DISTINCT ic_pathway_id, empsupportind, empstatus, appointment
        FROM project_space..iapt_appointment_17_18
          UNION ALL
    SELECT DISTINCT ic_pathway_id, empsupportind, empstatus, appointment
        FROM project_space..iapt_appointment_16_17
          UNION ALL
    SELECT DISTINCT ic_pathway_id, empsupportind, empstatus, appointment
        FROM project_space..iapt_appointment_15_16
    WHERE ic_pathway_id IS NOT NULL")

# Employment information from NHS TT
treatment_df  <- sdf_sql(sc,
    "SELECT DISTINCT ic_pathway_id, thertype1, appointment
        FROM project_space.iapt_appointment_20_21 
        WHERE ic_pathway_id IS NOT NULL AND 
        attendance IN (5, 6, 05, 06)
          UNION ALL
    SELECT DISTINCT ic_pathway_id, thertype1, appointment
        FROM project_space..iapt_appointment_19_20 
        WHERE ic_pathway_id IS NOT NULL AND 
        attendance IN (5, 6, 05, 06)
          UNION ALL
    SELECT DISTINCT ic_pathway_id, thertype1, appointment
        FROM project_space.iapt_appointment_18_19 
        WHERE ic_pathway_id IS NOT NULL AND 
        attendance IN (5, 6, 05, 06)
          UNION ALL
    SELECT DISTINCT ic_pathway_id, thertype1, appointment
        FROM project_space.iapt_appointment_17_18_std 
        WHERE ic_pathway_id IS NOT NULL AND 
        attendance IN (5, 6, 05, 06)
          UNION ALL
    SELECT DISTINCT ic_pathway_id, thertype1, appointment
        FROM project_space..iapt_appointment_16_17_std 
        WHERE ic_pathway_id IS NOT NULL AND 
        attendance IN (5, 6, 05, 06)
          UNION ALL
    SELECT DISTINCT ic_pathway_id, thertype1, appointment
        FROM project_space.iapt_appointment_15_16_std
        WHERE ic_pathway_id IS NOT NULL AND 
        attendance IN (5, 6, 05, 06)") %>% 
  mutate(appointment = to_date(appointment, "yyyy-MM-dd")) 


# Load Census 2011 data 
census_11 <- sdf_sql(sc,
                     "SELECT il4_person_id, sex, ethpuk11,
                             carer, disability, nssec, hlqpuk11, marstat, relpuk11,
                             aggcobpuk113, hhchuk11,dpchuk11, dob_quarter,
                             region_code,lsoa_code,deprived
                      FROM project_space.deidentified_census_2011_dataset
			    WHERE uresindpuk11 ==1 AND cen_pr_flag ==1")
                            
# Load deaths data

deaths <- sdf_sql(sc,
                  "SELECT doddy, dodmt, dodyr, census2011_person_id
                    FROM project_space.deaths_2023
                    UNION ALL
			 SELECT doddy, dodmt, dodyr, census2011_person_id
                    FROM project_space.deaths_2022
                    UNION ALL
                  SELECT doddy, dodmt, dodyr,  census2011_person_id
                     FROM project_space.deaths_2021
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id  
                    FROM project_space.deaths_2020
                    UNION ALL
                  SELECT doddy, dodmt, dodyr,  census2011_person_id  
                    FROM project_space.deaths_2019
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id  
                    FROM project_space.deaths_2018
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id 
                    FROM project_space.deaths_2017
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id 
                    FROM project_space.deaths_2016
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id 
                    FROM project_space.deaths_2015
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id  
                    FROM project_space.deaths_2014
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id  
                    FROM project_space.deaths_2013
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id  
                    FROM project_space.deaths_2012
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id  
                    FROM project_space.deaths_2011
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id 
                    FROM project_space.deaths_2010
                    UNION ALL
                  SELECT doddy, dodmt, dodyr, census2011_person_id 
                    FROM project_space.deaths_2009")


# Load NINO to Census 2011 look up table
nino_11 <- sdf_sql(sc, "SELECT frameworks_nino, census_id
                          FROM project_space.census_nino_look_up
                          WHERE nino_contains_multiple_census_id == 0 AND 
                      frameworks_nino IS NOT NULL AND census_id IS NOT NULL")

# Load monthly HMRC data
hmrc_monthly <- sdf_sql(sc,"SELECT valid_nino, month, pay 
                            FROM project_space.monthly_hmrc")

# load look up for the 2023 deflator
deflator <- read.csv(paste0("project/Lookups/cpih_deflator_2023.csv"))%>% 
  select(-month_org)

copy_to(sc,deflator)

### POPULATION FLOW CHART
sample_flow <- tibble(stage = "Total number of people in Census 2011",
                      count = sdf_nrow(census_11))


#------------------------------------------------------------------------------------------
# Process NHS Talking Therapies data - bind all datasets
#------------------------------------------------------------------------------------------

final_nhs_all1 <- rbind(nhs_tt_15_16, 
                       nhs_tt_16_17, 
                       nhs_tt_17_18, 
                       nhs_tt_18_19, 
                       nhs_tt_19_20,
                       nhs_tt_20_21) 


### POPULATION FLOW CHART

temp <- final_nhs_all %>% sdf_distinct(iapt_person_id)
sample_flow <- sample_flow %>% dplyr::bind_rows(
    tibble(stage = "Number of unique patients between 2015/16 and 2019/20 years in NHS TT datasets",
           description= "Number of people before filtering",
           count = sdf_nrow(temp)))
rm(temp)
gc()

#------------------------------------------------------------------------------------------
# Process NHS Talking Therapies data - filter population and clean NHS TT df
#------------------------------------------------------------------------------------------

final_nhs_all <- final_nhs_all %>% 

  filter(ic_caseness_flag == "Y") %>%
  filter(endcode %in% c("14","42", "43")) %>%

  date_format_nhs_vars_function(.) %>%

  filter(refrecdate <= end_rp & refrecdate >= start_rp) %>% 

  latest_dates_update_function(.) %>%

  mutate(caseness_flag = case_when(ic_caseness_flag == "Y" ~ 1,
                                   ic_not_caseness_flag == "Y" ~ 0,
                                   TRUE ~ 'Missing'),
         completed_treatment_flag = ifelse(ic_completed_treatment_flag == "Y",1,0)) %>%
  mutate(exposure_groups = 
         case_when(ic_completed_treatment_flag == "Y" & endcode == "42" ~ "completed_treatment",
                   ic_completed_treatment_flag == "Y" & endcode == "14" | 
                   ic_completed_treatment_flag == "Y" & endcode == "43" ~ "dropped_out_of_treatment",
                   TRUE ~ "XXXX"),
        diagnosis = 
         case_when(ic_probdesc_primary == 'Depression' ~ 'Depression',
          ic_probdesc_primary == 'Anxiety and stress related disorders (Total)' ~ ic_probdesc_secondary,
          TRUE ~ 'Missing or not stated')) %>%

  select(-c("endcode","ic_caseness_flag","ic_not_caseness_flag","ic_probdesc_secondary")) %>%
  sdf_drop_duplicates() %>% 

  # remove people who had an assessment after first therapy session 
  mutate(temp_flag = ifelse(!is.na(ic_date_first_assessment) & 
                            ic_date_first_assessment > ic_date_first_therapeutic_session, 1,0)) %>%
  filter(is.na(temp_flag) | temp_flag == 0) %>% 
  select(-temp_flag) %>%

  group_by(iapt_person_id) %>%
  filter(refrecdate == min(refrecdate)) %>%
  ungroup() %>%

  # Note: some patients might still have more than 1 referral if they have been referred twice (or more) on the same date
  arrange(iapt_person_id, 
          desc(as.numeric(ic_count_treatment_appointments)),
          ic_pathway_id,
          diagnosis,
          ic_first_sickpay_indicator,
          ic_last_sickpay_indicator,
          ic_first_psychotropic_indicator,
          ic_last_psychotropic_indicator) %>%
  sdf_drop_duplicates(cols = 'iapt_person_id') %>%

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

      # we'll use flags made by NHS
    caseness_and_outcomes_function(.) %>%
  
    rename(., last_appointment = ic_date_last_appointment) %>%

  mutate(
    first_medication = 
    case_when(ic_first_psychotropic_indicator == '1' |
              ic_first_psychotropic_indicator == '01' ~ 'Prescribed, not taking',
              ic_first_psychotropic_indicator == '2' |
              ic_first_psychotropic_indicator == '02' ~ 'Prescribed, taking',
              ic_first_psychotropic_indicator == '3' |
              ic_first_psychotropic_indicator == '03' ~ 'Not prescribed',
              TRUE ~ 'Missing or not stated'),

    last_medication = 
    case_when(ic_last_psychotropic_indicator == '1' |
              ic_last_psychotropic_indicator == '01' ~ 'Prescribed, not taking',
              ic_last_psychotropic_indicator == '2' |
              ic_last_psychotropic_indicator == '02' ~ 'Prescribed, taking',
              ic_last_psychotropic_indicator == '3' |
              ic_last_psychotropic_indicator == '03' ~ 'Not prescribed',
              TRUE ~ 'Missing or not stated'),

    first_sickpay = 
    case_when(ic_first_sickpay_indicator == 'Y' |
              ic_first_sickpay_indicator == 'y' ~ 'Yes', 
              ic_first_sickpay_indicator == 'N' |
              ic_first_sickpay_indicator == 'n' ~ 'No', 
              TRUE ~ 'Missing or not stated'),
    
    last_sickpay = 
    case_when(ic_last_sickpay_indicator == 'Y' |
              ic_last_sickpay_indicator == 'y' ~ 'Yes', 
              ic_last_sickpay_indicator == 'N' |
              ic_last_sickpay_indicator == 'n' ~ 'No', 
              TRUE ~ 'Missing or not stated')) %>%

    # remove old columns
    select(-c('ic_date_first_assessment',
              'ic_date_first_therapeutic_session',
              'ic_date_second_therapeutic_session',
              'ic_date_last_therapeutic_session',
              'ic_first_psychotropic_indicator',
              'ic_last_psychotropic_indicator',
              'ic_first_sickpay_indicator',
              'ic_last_sickpay_indicator')) %>%  
  census_11_flag_fun(.) 

### POPULATION FLOW CHART

temp <- final_nhs_all %>% sdf_distinct(iapt_person_id)
sample_flow <- sample_flow %>% dplyr::bind_rows(
    tibble(stage = "Number of patinets in NHS TT at caseness and with codes 14, 42 and 43.",
           description= "Desired population - endcodes, within RP, at caseness, corrected dates(asses<first ther)",
           count = sdf_nrow(temp)))
rm(temp)
gc()


#------------------------------------------------------------------------------------------
### 1.5. Edit employment (employment at referral & last recorded employment per pathway)
#------------------------------------------------------------------------------------------

# this gives us last non missing employment status and the corresponding date
last_employment <- employment_df %>%
  select(ic_pathway_id, appointment, empstatus) %>%
  mutate(appointment = to_date(appointment, "yyyy-MM-dd")) %>%
  filter(!is.na(empstatus)) %>%
  group_by(ic_pathway_id) %>%
  mutate(last_emp_date = max(appointment)) %>% 
  ungroup() %>%
  filter(last_emp_date == appointment) %>%
  rename(last_employment_stat = empstatus) %>%
  select(-appointment) %>%
  # some people have 2 statuses taken on the same day so drop at random
  sdf_drop_duplicates('ic_pathway_id') 


employment_df <- employment_df %>%
  mutate(appointment = to_date(appointment, "yyyy-MM-dd")) %>%

  # Initial employment status column
  group_by(ic_pathway_id) %>% 
  mutate(first_app = min(appointment, na.rm=TRUE),
         rank = rank(appointment),
         first_emp_stat = ifelse(appointment==first_app, empstatus,NA),
         first_emp_stat = max(first_emp_stat),
         second_emp_stat = ifelse(rank==2, empstatus,NA),
         second_emp_stat = max(second_emp_stat),
         referral_employment_stat = 
         ifelse(!is.na(first_emp_stat),
                  first_emp_stat, second_emp_stat),
    
  
# Employment support flag
    emp_support_flag = 
      case_when(empsupportind == "Y" | empsupportind == "Yes"|
                empsupportind == "y" | empsupportind == "yes" ~ 1,
                TRUE ~ 0),
# ensure we pick up a flag from any point in treatment 
        emp_support_flag = max(emp_support_flag)) %>%

  ungroup() %>% 

  select(-c("first_emp_stat", "second_emp_stat","first_app", 
           "empsupportind", "empstatus", "appointment", "rank")) %>% 
  # some people have 2 statuses taken on the same day so drop at random
  sdf_drop_duplicates('ic_pathway_id') %>%
  left_join(., last_employment, by = 'ic_pathway_id')
 
# remove last_emp df
rm(last_employment)
gc()


final_nhs_all <-left_join(final_nhs_all ,employment_df, by = 'ic_pathway_id') %>%

  #rename source of referral categories
  referral_source_fun(.) %>%

  # rename employment categories
  employment_cat_function(., referral_employment_stat) %>% 
  employment_cat_function(., last_employment_stat) 


#------------------------------------------------------------------------------------------
###   2. Process Census, nino look up & deaths. 3.Recode Census variables                    
#------------------------------------------------------------------------------------------

# Rename census id variables
census_11 <- rename(census_11,
                    census2011_person_id = il4_person_id) 

nino_11 <- rename(nino_11, 
                  census2011_person_id = census_id,
                  valid_nino = frameworks_nino)



# Make date of death into one variable 
deaths <- deaths %>%
  mutate(dod = paste0(dodyr, dodmt, doddy)) %>%
  mutate(dod = to_date(dod, "yyyy.MM.dd")) %>% 
  #Drop variables
  select(-c(dodyr, dodmt, doddy))
  

#Create 'provisional' date of birth (prov_dob) based on dob_quarter & link deaths & nino
census_11_with_nino <- create_dob(census_11) %>%
  left_join(., deaths, by = 'census2011_person_id') %>%
  inner_join(., nino_11, by ='census2011_person_id')


# Process disability 
disability11 <- census_11_with_nino %>% select(census2011_person_id, disability) %>%
  mutate(disability_category_11 =
          case_when(disability == 1 ~ "Activities limited a lot",
                    disability == 2 ~ "Activities limited a little",
                    disability == 3 ~ "Not disabled or activities not limited",
                    TRUE ~ "Missing or not stated")) %>% select(-disability)

census_11_with_nino <- rename(census_11_with_nino, 
                              census_person_id = census2011_person_id) %>% 
  rename_census11_variables(.) 

#main_census <- rbind(census_21_with_nino, census_11_with_nino) 

### POPULATION FLOW CHART 
temp <- census_11_with_nino %>% sdf_distinct(census_person_id)
sample_flow <- sample_flow %>%
  dplyr::bind_rows(tibble(stage = "Number of people in Census 2011 that can be linked to NINO",
                          description= "UK usual residenst and PR linked flag included",
                         count = sdf_nrow(temp)))
rm(temp) 
gc()

#------------------------------------------------------------------------------------------
###           4a. Link NHS TT & Census
#------------------------------------------------------------------------------------------


linked_final_nhs_all  <- inner_join(final_nhs_all , 
                                    census_11_with_nino, 
                                    by = 'census_person_id') 

### POPULATION FLOW CHART 
temp <- linked_final_nhs_all   %>% sdf_distinct(census_person_id)
sample_flow <- sample_flow %>%
    dplyr::bind_rows(tibble(stage = "Number of patients in NHS TT that can be linked to 2011 Census and NINO",
                            count = sdf_nrow(temp)))
rm(temp, final_nhs_all, census_11_with_nino)
gc()

#------------------------------------------------------------------------------------------
###           4b. Add age_at_referral & age bands
#------------------------------------------------------------------------------------------

linked_final_nhs_all  <- linked_final_nhs_all %>% 

  # Calculate age at referral
  mutate(
    ref_year = as.numeric(substring(refrecdate,1,4)),
    dob_year = as.numeric(substring(prov_dob,1,4)),
    age_at_referral = 
      ifelse(substring(refrecdate,6,10) >= substring(prov_dob,6,10), 
             ref_year - dob_year, (ref_year - dob_year)-1),
    age_bands = 
         case_when(age_at_referral < 25 ~ "18-24",
                   age_at_referral >= 25 & age_at_referral < 35 ~ "25-34",
                   age_at_referral >= 35 & age_at_referral < 45 ~ "35-44",
                   age_at_referral >= 45 & age_at_referral < 55 ~ "45-54",
                   age_at_referral >= 55 ~ "55-69"),    

    emp_support_flag = ifelse(is.na(emp_support_flag), 0, emp_support_flag)) %>%

  # Remove individuals who are less than 16 at the time of referral
    filter(age_at_referral >= 18 & age_at_referral <= 69) %>%

  # Join disability
  left_join(., disability11, by = 'census2011_person_id') %>%

  select(-c("census2011_person_id","dob_year","ref_year")) %>% 
  sdf_drop_duplicates()    

rm(disability11)
gc()

### POPULATION FLOW CHART 
temp <- linked_final_nhs_all   %>% sdf_distinct(census_person_id)
sample_flow <- sample_flow %>%
    dplyr::bind_rows(tibble(stage = "Restricted population to 18-69 years old at referral only",
                            count = sdf_nrow(temp)))

rm(temp)
gc()


#------------------------------------------------------------------------------------------
###                        5. Create monthly NHS TT & Census data 
#------------------------------------------------------------------------------------------

final_monthly_with_hmrc <- main_monthly_function(linked_final_nhs_all)

### POPULATION FLOW CHART 
temp <- final_monthly_with_hmrc %>% sdf_distinct(census_person_id)
sample_flow <- sample_flow %>%
    dplyr::bind_rows(tibble(stage = "Number of patients in NHS TT with referral between 2016/17 to 2018/19",
                            description="Monthly NHS TT data - should be the same as above",
                            count = sdf_nrow(temp)))
rm(temp, linked_final_nhs_all)
gc()

#------------------------------------------------------------------------------------------
###                       6. Edit HMRC & Link to other data
#------------------------------------------------------------------------------------------

# add month_label variable to hmrc for linkage 
hmrc_monthly <- hmrc_monthly %>%  
  mutate(date_month = month(month)) %>% 
  mutate(date_year = year(month)) %>%
  mutate(month_label = paste0(date_year, "-", date_month)) %>% 
  select(-c('date_month','date_year'))


# Link HMRC to other datasets

final_monthly_with_hmrc <- left_join(final_monthly_with_hmrc, hmrc_monthly,
                                    by = c("valid_nino" = "valid_nino", 
                                           "month_label" = "month_label")) %>% 
  mutate(month = date_add(last_day(add_months(ref_date, -1)),1),
         ref_date = to_date(ref_date, "yyyy-MM-dd")) %>%  
  group_by(month_label,iapt_person_id) %>%
  mutate(pay= sum(pay)) %>% 
  select(-valid_nino) %>%
  sdf_drop_duplicates()  

# at this stage there should be no duplicates

#clean pay variable-negative payments into 0s but negative as in work
final_monthly_with_hmrc <- final_monthly_with_hmrc %>%
  mutate(original_pay = ifelse(is.na(pay),0, pay),
          #less than 1 as some have value of 0.0003
         pay = ifelse(original_pay < 1, 0, original_pay))


# remove people who do not have t_referral 0 
# caused by people when they reach min/max age e,g, 18 or 69 
# or when their date of death is on the same month of referral
# Keep only people who have minimum of 2 years prior follow up 

final_monthly_with_hmrc <- final_monthly_with_hmrc %>% 
  mutate(t_0 = ifelse(t_referral == -24, 1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = max(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)

final_monthly_with_hmrc <- final_monthly_with_hmrc %>%
  mutate(t_0 = ifelse(t_referral == 0,1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = sum(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)

final_monthly_with_hmrc <- final_monthly_with_hmrc %>%
  mutate(t_0 = ifelse(t_first_therapy == 0,1, 0))%>%
  group_by(census_person_id) %>%
  mutate(t_0 = sum(t_0))%>%
  filter(t_0 == 1)%>%
  ungroup() %>% 
  select(-t_0)

### POPULATION FLOW CHART 
temp <- final_monthly_with_hmrc %>% sdf_distinct(census_person_id)
sample_flow <- sample_flow %>%
    dplyr::bind_rows(tibble(stage = "Linked data with HMRC - removed those with no t_ref or t_first_ther == 0",
                            description="Monthly NHS TT data - after linking",
                            count = sdf_nrow(temp)))

rm(temp)
gc()

#------------------------------------------------------------------------------------------
###               6. Add deflator keep only those with min -24 months
#------------------------------------------------------------------------------------------
# CPIH 2023 deflator - convert pay information to current payments

final_monthly_with_hmrc <- left_join(final_monthly_with_hmrc, deflator,
                                     by = c("month" = "month")) %>%
  mutate(pay_no_deflator = pay, 
         pay = pay_no_deflator/cpih_deflator_2023)
rm(deflator)
gc()


### POPULATION FLOW CHART 
temp <- final_monthly_with_hmrc %>% sdf_distinct(census_person_id)
sample_flow <- sample_flow %>%
    dplyr::bind_rows(tibble(stage = "Final data for treated & non-treated group",
                            description="Kept only people with minimum 24 months of prior follow up",
                            count = sdf_nrow(temp)))
rm(temp)
gc()


# save sample flow 
write.csv(sample_flow, paste0(outputs_folder, "new_method_population_flow_chart_",Sys.Date(), ".csv"))

#--------------------------------------------------------------------------
# Save final data
#--------------------------------------------------------------------------
  
sql <- paste0("DROP TABLE IF EXISTS project_space.",final_linked_dataset,"")
DBI::dbExecute(sc, sql)

# Save temp data for checks 
sdf_register(final_monthly_with_hmrc , 'final_monthly_with_hmrc')
sql <- paste0("CREATE TABLE project_space.",final_linked_dataset, " AS SELECT * FROM final_monthly_with_hmrc")
DBI::dbExecute(sc, sql)


#--------------------------------------------------------------------------
# Closing Spark sessions
#--------------------------------------------------------------------------
spark_disconnect(sc)
