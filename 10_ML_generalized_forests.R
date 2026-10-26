# Section 1: Package loading ===================================================

#rm(list = ls())
#Sys.setenv(TZ='GMT')

library(dplyr)
library(grf)
library(car) 
library(kableExtra)
library(fBasics)
library(fastDummies)
library(readr)
library(lubridate)
library(openxlsx)

# Section 1.1: Files/folders ===================================================

# Functions
source("project_name/folder/functions.R")

## Config file
source("project_name/folder/config.R")

##OUTPUTS FOLDER
outputs_folder <- "project_name/Results/"

#HDFS folder with RDS files
hdfs_path = 'path'


# Section 2: Load data ========================================================

# Initialize an empty list to store dataframes 
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

treat_out <- readRDS(paste0(outputs_folder, "treatment_outcomes.RDS"))

df <- left_join(df,treat_out, by="census_person_id") 
rm(df_list, treat_out)

set.seed(123456)

# Section 3: Data prep ========================================================
 # Add quantiles
q = quantile(df$pay, prob=c(0.01, 0.99))

# Focus on the time of the start of therapy / drop variables that are not needed
glimpse(df)


# generate a year and month of first therapy date
#df$first_therapy_date <- as.Date(as.character(df$first_therapy_date), format="%Y-%m-%d")
#df$first_therapy_year <- format(df$first_therapy_date, "%Y")   # Extract the year
#df$first_therapy_month <- format(df$first_therapy_date, "%m")  # Extract the month

df$first_therapy_quarter <- substr(df$reference_date,6,6)
df$first_therapy_year <- substr(df$reference_date,1,4)

df <- df %>% mutate(treatment = 
                    ifelse(exposure_groups == 'completed_treatment', 1, 0),
                    employed = ifelse(pay > 0, 1, 0),
                    pay = case_when(pay < 0 ~ 0, 
                                    pay > q[[2]]~ q[[2]],
                    TRUE ~ pay))

# summary(df$t_first_therapy)
# levels(df$sex)

drops <- c("exposure_groups",
           "date_of_birth", 
           "t_referral", 
           "t_last_therapy", 
           "referral_date", 
           "quarterly_age",
           "reference_date",
           "age_at_referral",
           "treatment_outcomes",
		"is_carer_binary",
		"first_sickpay")

df <- df[, !(names(df) %in% drops )]
sum(is.na(df))

# Section 3.2: Wrangling to leave one observation per person ==========================
# Define outcomes

# Function to calculate averages before and after treatment and merge into the original data
# -1 year before 
# 5 years after 

calculate_avg <- function(var, data) {
  before_avg <- data %>%
    dplyr::filter(t_first_therapy >= -3 & t_first_therapy <= 0) %>%
    dplyr::group_by(census_person_id) %>%
    dplyr::summarise(average_before = mean({{ var }}, na.rm = TRUE))
  
  after_avg <- data %>%
    dplyr::filter(t_first_therapy >= 13 & t_first_therapy <= 16) %>%
    dplyr::group_by(census_person_id) %>%
    dplyr::summarise(average_after = mean({{ var }}, na.rm = TRUE))
  
  # Merge the before and after averages back into the original working data
  data <- data %>%
    dplyr::left_join(before_avg, by = "census_person_id") %>%
    dplyr::left_join(after_avg, by = "census_person_id", suffix = c("_before", "_after")) %>%
    dplyr::mutate(!!paste0(rlang::as_name(ensym(var)), "_average") := dplyr::case_when(
      t_first_therapy == 0 ~ average_before,
      t_first_therapy == 16 ~ average_after
    )) %>%
    dplyr::select(-average_before, -average_after)
  
  return(data)
}

# Apply the function for both 'pay' and 'employed' variables
df <- calculate_avg(pay, df)
df <- calculate_avg(employed, df)

# Keep rows where pay_average is not missing
df <- df %>%
  dplyr::filter(!is.na(employed_average)) #or pay_average

# Remove rows with duplicate combinations of id and t_first_therapy
df <- df %>%
  dplyr::distinct(census_person_id, t_first_therapy, .keep_all = TRUE)

# Keep only people whom we observe at both periods

## Identify the ids that have both t_first_therapy == 0 and t_first_therapy == 16
ids_with_both <- df %>%
  dplyr::filter(t_first_therapy %in% c(0, 16)) %>%
  dplyr::group_by(census_person_id) %>%
  dplyr::filter(all(c(0, 16) %in% t_first_therapy)) %>%
  dplyr::pull(census_person_id) %>%
  unique()

## Keep only rows where id is in the identified ids_with_both
df <- df %>%
  dplyr::filter(census_person_id %in% ids_with_both)

# Generate change from the first period to the second

## Sort the dataset by id and t_first_therapy
df <- df %>%
  dplyr::arrange(census_person_id, t_first_therapy)

## Calculate the change in pay_average and employed_average between the two periods (0 and 49)
df <- df %>%
  dplyr::group_by(census_person_id) %>%
  dplyr::mutate(
    pay_change = if_else(t_first_therapy == 16, 
                         pay_average - pay_average[t_first_therapy == 0], NA_real_),
    
    #employed_change = if_else(t_first_therapy == 16, 
    #                          employed_average - employed_average[t_first_therapy == 0], NA_real_)
  ) %>%
  dplyr::ungroup()

## Assign the change to both periods (0 and 16) by filling down the change
df <- df %>%
  dplyr::group_by(census_person_id) %>%
  dplyr::mutate(
    pay_change = dplyr::coalesce(pay_change, lead(pay_change, order_by = t_first_therapy)),
    #employed_change = dplyr::coalesce(employed_change, lead(employed_change, order_by = t_first_therapy))
  ) %>%
  dplyr::ungroup()

## Keep only the first observation for each id
df <- df %>%
  dplyr::group_by(census_person_id) %>%
  dplyr::filter(row_number() == 1) %>%
  dplyr::ungroup()


drops <- c("census_person_id", 
           "t_first_therapy", 
           "employed_average",
           "month", 
           "pay",
           "pay_average", "employed")

df <- df[, !(names(df) %in% drops )]

sum(is.na(df)) # count na - should be 0

# Section 2.3: Preparing data for estimation =========================================

outputs <- c("employed_change","pay_change") 
outputs_df <- df[, (names(df) %in% outputs )]

df$W <- as.numeric(df$treatment)

# Extract treatment variable & covariates
W <- df[["treatment"]]

drops <- c("treatment","pay_change", "employed_change", "W")
df <- df[, !(names(df) %in% drops )]

table(W)

colnames(df)

# table(df$hh_dependent_children)
# levels(mode_disability)

# Group 1: Convert a group to ordered factors
# It's beneficial for the algorithm to have less variables, so I recode all that I can as ordered factors with 
# missing levels imputed with the mode and a dummy for missing
# I do it for "disability_category_11", "hh_dependent_children", "deprived",

calculate_mode <- function(x) {
  uniq_x <- unique(x)                          # Get unique values
  uniq_x[which.max(tabulate(match(x, uniq_x)))] # Return the most frequent value (mode)
}

## Disability 

summary(df$disability_category_11)
# Convert 'disability_category_11' to an ordered factor
df$disability_category_11 <- factor(df$disability_category_11,
                                         levels = c("Not disabled or activities not limited", 
                                                    "Activities limited a little", 
                                                    "Activities limited a lot"),
                                         ordered = TRUE)

# Check the result
summary(df$disability_category_11)

## Children
summary(df$hh_dependent_children)

# Create a dummy for missing values ("Missing or not stated")
df$hh_dependent_children_missing <- as.numeric(df$hh_dependent_children == "Missing or not stated")

# Convert only "Missing or not stated" to NA
df$hh_dependent_children[df$hh_dependent_children == "Missing or not stated"] <- NA

# Calculate the mode, excluding missing values
mode_hh_children <- calculate_mode(df$hh_dependent_children)

# Impute missing values with the calculated mode
df$hh_dependent_children[is.na(df$hh_dependent_children)] <- mode_hh_children

# Convert 'hh_dependent_children' to an ordered factor
df$hh_dependent_children <- factor(df$hh_dependent_children,
                                        levels = c("No children", 
                                                   "One child", 
                                                   "Two children", 
                                                   "Three or more children"),
                                        ordered = TRUE)

# Check the result
summary(df$hh_dependent_children)
table(df$hh_dependent_children_missing)



summary(df$age_bands)
# Convert 'age_bands' to an ordered factor
df$age_bands <- factor(df$age_bands,
                       levels = c("25-34", 
                                  "35-44", 
                                  "45-54",
                                  "55-60"),
                       ordered = TRUE)

# Check the result
summary(df$age_bands)
table(df$age_bands)


## Deprivation
summary(df$deprived)
# Create a dummy for missing values ("Missing or not stated")
df$deprived_missing <- as.numeric(df$deprived == "Missing or not stated")

# Convert only "Missing or not stated" to NA
df$deprived[df$deprived == "Missing or not stated"] <- NA

# Calculate the mode, excluding missing values
mode_deprived <- calculate_mode(df$deprived)

# Impute missing values with the calculated mode
df$deprived[is.na(df$deprived)] <- mode_deprived

# Convert 'deprived' to an ordered factor
df$deprived <- factor(df$deprived,
                           levels = c("Not deprived", 
                                      "Deprived, 1 dimension", 
                                      "Deprived, 2 dimensions", 
                                      "Deprived, 3 dimensions", 
                                      "Deprived, 4 dimensions"),
                           ordered = TRUE)

# Check the result
summary(df$deprived)
table(df$deprived_missing)

# Convert ordered factors to numeric values
df$disability_category_11 <- as.numeric(df$disability_category_11)
df$hh_dependent_children <- as.numeric(df$hh_dependent_children)
df$deprived <- as.numeric(df$deprived)
df$age_bands <- as.numeric(df$age_bands)

# Step 2: Check the conversion (optional)
table(df$disability_category_11)
table(df$hh_dependent_children)
table(df$deprived)
table(df$age_bands)

##################################################
##################################################
##################################################

# Group 2: Convert a group of factors to integers
factors_to_integer <- c("first_therapy_year", "first_therapy_quarter")
df[factors_to_integer] <- lapply(df[factors_to_integer], 
                                 function(x) as.integer(x))

# Group 3: Convert a group of factors to dummy variables
#   for non-binary variables, it would be better to have them as one ordered variable
#   but here I cannot come up with an obvious ordering, unless regroup
#   we can think about it if we cannot make the forest work / use lasso to select variables

factors_to_dummy <- c("sex",
                      "therapy_intensity", 
                      "ethnic_group_tb", 
                      "country_of_birth_agg", 
                      "Region", 
                      "diagnosis",
                      "sourcereferral",
                      "referral_employment_stat",
                      "highest_qualification", 
                      "ns_sec", 
                      "first_medication")
  
df <- fastDummies::dummy_cols(df, 
                              select_columns = factors_to_dummy, 
                              remove_first_dummy = TRUE)
  
df <- df[, !(names(df) %in% factors_to_dummy )]

temp <-df %>% nrow()

write.csv(temp, paste0(outputs_folder , "sample_size_of_ML_code.csv"))
rm(temp)
                                 
# Scale covariates to reduce variance 
summ_stats <- fBasics::basicStats(df)
summ_stats <- as.data.frame(t(summ_stats))
covariate.to.scale.names <- rownames(summ_stats)[which(summ_stats$Variance>3)] # select variables that have zero sd

covariates.to.scale <- df[, (names(df) %in% covariate.to.scale.names)]
covariates <- df[, !(names(df) %in% covariate.to.scale.names)]
covariates.scaled <- scale(covariates.to.scale) 

# scale, with default settings, will calculate the mean and standard deviation of the entire vector,

# clist <- c("pay_change", "employed_change")
i <- c("pay_change")#  "employed_change")#,)
# for (i in clist) {
#   print(i)

Y <- outputs_df[[i]]

processed.unscaled <- data.frame(Y,W,covariates,covariates.to.scale) #
processed.scaled <-data.frame(Y,W,covariates,covariates.scaled) #

covariate.names <- colnames(processed.scaled)[-c(1,2)]

sumx <- paste(covariate.names, collapse = " + ")  # "X1 + X2 + X3 + ..." for substitution later
linear <- paste("Y",sumx, sep=" ~ ") #  Y ~  X1 + X2 + X3 + ...
linear <- as.formula(linear)

working.train <- processed.scaled
y.train <- as.matrix(working.train$Y, ncol=1)


#############################################################
#############################################################
#############################################################
#############################################################

# Section 3: Estimation and Presenting Results =========================================

# Run an OLS
# Create the regression formula dynamically
ols_formula <- as.formula(
  paste("Y", "~", "W", "+", paste(covariate.names, collapse = " + "))
)
model <- lm(ols_formula, data = working.train)
summary(model)
model <- broom::tidy(model, conf.int=T)
model %>% print()

## Run a Causal Forest using the 'grf' package

cf_grf <- grf::causal_forest(
  X = as.matrix(working.train[,covariate.names]),  # Covariates matrix (features)
  Y = as.vector(working.train$Y),                 # Outcome variable vector
  W = as.vector(working.train$W),                 # Treatment variable vector
  # Optional parameters you can uncomment to customize the model tuning:
  # tune.parameters = c("honesty.prune.leaves"),   # Tune specific parameters
  # tune.parameters = c("min.node.size", "honesty.prune.leaves"),
  # honesty = FALSE,                               # Disable 'honesty' in estimation
  # honesty.fraction = 0.8,                        # Fraction of data for honesty
  tune.parameters = 'all',                         # Tune all parameters - can be computationally expensive
  # mtry = 300,                                    # Number of variables to consider for splitting
  # sample.fraction = 0.1,                         # Fraction of data to use
  # min.node.size = 500,                           # Minimum size of nodes: a node must contain at least min.node.size treated samples, and also at least that many control samples
  # alpha = 0.05,                                  # Minimum child node size proportion: each child node is not allowed to be less than size(parent) * alpha. Its value must lie between (0, 0.25), and defaults to 0.05. 
  num.trees = 2000                                 # Number of trees to grow: a rule of thumb = N of individuals, I usually check if the results differ if I increase the number until then. If too computationally expensive - check if differ between 500 and 2000 trees
)

# Sanity check (An omnibus test for heterogeneity (BLP)): ====
cf_grf_r2 <- grf::test_calibration(cf_grf)
print(cf_grf_r2)

tc <- test_calibration(cf_grf)
print(tc)
caption <- "Best linear fit using forest predictions (on held-out data)
                      as well as the mean forest prediction as regressors, along
                      with heteroskedasticity-robust (HC3) SEs."
table6 <- as.data.frame(tc[,]) 
#table6 %>%
#  kable(format="html", 
#        digits=6, caption=caption, 
#        escape = FALSE, 
#        row.names = FALSE) %>%
#  kable_styling(bootstrap_options=c("condensed", "responsive"), full_width = FALSE)

# both estimates should ideally be close to 1
# the first one being close to 1 = the forest captures the average effect well
# the second being close to 1 = the forest captures heterogeneity well
# End of Sanity check ===


## Calculate and store variable importance from the fitted causal forest model

var_imp <- c(variable_importance(cf_grf))

# Count the number of covariates with non-zero importance scores
sum(var_imp > 0)  

# Name the variable importance values according to the covariate names
names(var_imp) <- covariate.names

# Sort the variable importance values in descending order
sorted_var_imp <- sort(var_imp, decreasing = TRUE)

# Convert the sorted variable importance values to a data frame
#table_vimp <- as.data.frame(sorted_var_imp, row.names = names(sorted_var_imp))
table_vimp <- broom::tidy(sorted_var_imp)                          
                        
write.csv(table_vimp, paste(outputs_folder,i, "_vimp.csv"), row.names = TRUE)

          ## Display the sorted variable importance table using 'kable' for HTML formatting
          #sorted_var_imp %>%
          #  kable("html", digits = 4, row.names = TRUE) %>%  # Format with 4 decimal places
          #  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), 
          #                full_width = FALSE)  # Add styling for better display

sorted_var_imp <- broom::tidy(sorted_var_imp) #%>% print()                               

# Write the variable importance data frame to a CSV file

write.csv(sorted_var_imp, 
          paste0(outputs_folder,i,"_variable_importance_testing_", Sys.Date(),".csv"),
          row.names = TRUE)

## Predict point estimates and standard errors (out-of-bag)

# Generate out-of-bag predictions with variance estimates
oob_pred <- predict(cf_grf, estimate.variance = TRUE)

# Sanity check (check CI of of treatment effects): ====
# Create 95% confidence intervals for the treatment effects
oob_tauhat_cf <- oob_pred$predictions
oob_tauhat_cf_se <- sqrt(oob_pred$variance.estimates)

lower_ci <- oob_tauhat_cf - 1.96 * oob_tauhat_cf_se
upper_ci <- oob_tauhat_cf + 1.96 * oob_tauhat_cf_se

# View the confidence intervals for the first 10 observations
result <- data.frame(Predicted_Treatment_Effect = oob_tauhat_cf[1:10],
                     Lower_CI = lower_ci[1:10],
                     Upper_CI = upper_ci[1:10])

print(result)
# End of Sanity check === 

# Estimate the Conditional Average Treatment Effect (CATE) on the full sample
cate <- average_treatment_effect(cf_grf, target.sample = "all")

# Estimate the Conditional Average Treatment Effect on the Treated (CATT)
catt <- average_treatment_effect(cf_grf, target.sample = "treated")

# Print the CATE and CATT results
print(cate)
print(catt)

# Store the predicted treatment effects (tau-hat) from out-of-bag predictions
oob_tauhat_cf <- oob_pred$predictions

# Store the standard errors of the tau-hat estimates (square root of variance estimates)
oob_tauhat_cf_se <- sqrt(oob_pred$variance.estimates)

# Plot a histogram of the predicted treatment effects (tau-hat)
hist(oob_tauhat_cf,
    xlab=i,
    ylab = 'Frequency',
    main = "Histogram of predicted treatment effects")
abline(v = mean(oob_tauhat_cf), col='red', lwd = 1)


png(paste(outputs_folder,i, "_histogram.png"))
hist(oob_tauhat_cf,
    xlab=i,
    ylab = 'Frequency',
    main = "Histogram of predicted treatment effects")
abline(v = mean(oob_tauhat_cf), col='red', lwd = 1)
dev.off()                                   
write.csv(oob_tauhat_cf, paste(outputs_folder,i,"_histogram_data.csv"))
                                   

# Compute the median of the predicted treatment effects (tau-hat)
median(oob_tauhat_cf)

# Compute quantiles of the predicted treatment effects (tau-hat)
quantile(oob_tauhat_cf)

# Write all to CSV files
write.csv(oob_pred, paste(outputs_folder,i, "_out_of_bag_prediction.csv"), row.names = FALSE)
write.csv(t(cate), paste(outputs_folder,i, "_cate_full.csv"), row.names = FALSE)
write.csv(t(catt), paste(outputs_folder,i, "_cate_treated.csv"), row.names = FALSE)
write.csv(table6, paste(outputs_folder,i, "_test_calibration_heteroskedasticity_robust.csv"), row.names = FALSE)

                                   
                                   
## Report how covariates vary across quartiles of effects distribution
# Describes the process of dividing the sample into quantiles based on the out-of-bag treatment effects, 
# and calculating statistics such as means, variances, and coefficients of variation for each covariate
# to study relative differences between quantiles.

# Number of quantiles to divide the sample into
num_tiles <- 4

# Create 'ntile' variable that groups the out-of-bag treatment effects (oob_tauhat_cf) into quantiles
working.train$ntile <- factor(ntile(oob_tauhat_cf, n = num_tiles))

# Convert all columns in 'working.train' to numeric for easier manipulation
df_est <- as.data.frame(apply(working.train, 2, as.numeric))

# Extract the covariate names, excluding 'ntile' and 'W' (treatment indicator)
covariate.names.us <- names(df_est[, !(names(df_est) %in% c("ntile", "W"))])

# Calculate the mean of each covariate within each quantile (ntile)
covariate_means_per_ntile <- aggregate(. ~ ntile, df_est, mean, na.rm = TRUE, na.action = NULL)[, covariate.names.us]

# Calculate the overall mean of each covariate across the entire dataset
covariate_means <- apply(df_est, 2, mean, na.rm = TRUE, na.action = NULL)[covariate.names.us]

# Calculate the proportion of data in each quantile (ntile)
ntile_weights <- table(df_est$ntile) / dim(df_est)[1]

# Calculate deviations from the overall mean for each covariate within each quantile
deviations <- t(apply(covariate_means_per_ntile, 1, function(x) x - covariate_means))

# Calculate the weighted variance for each covariate across quantiles
covariate_means_weighted_var <- apply(deviations, 2, function(x) sum(ntile_weights * x^2))

# Calculate the overall variance for each covariate across the entire dataset
covariate_var <- apply(df_est, 2, var, na.rm = TRUE)[covariate.names.us]

# Calculate the coefficient of variation for each covariate
cov_variation <- covariate_means_weighted_var / covariate_var

# Create a sorted list of covariates based on the coefficient of variation
sorted_cov_variation <- sort(cov_variation, decreasing = TRUE)
cov_sorted <- names(sorted_cov_variation)

# Create a data frame for the sorted coefficients of variation
table2 <- as.data.frame(sorted_cov_variation)
colnames(table2) <- NULL  # Remove column names for neat presentation
#
## Display the sorted coefficient of variation table with HTML formatting using 'kable'
#kable_styling(kable(table2, "html", digits = 4, row.names = TRUE,
#                    caption = "Covariate variation across quantiles"),
#              bootstrap_options = c("striped", "hover", "condensed", "responsive"),
#              full_width = FALSE)

## Ordered means by the decrease in variance // an alternative is to order by variable importance
# Explains how linear models are fitted to each covariate to estimate their means within quantiles. 

# For each covariate, fit a linear model to estimate means per quantile (ntile)
cov_means.sorted <- lapply(cov_sorted, function(covariate) {
  lm(paste0(covariate, ' ~ 0 + ntile'), data = working.train)
})

# Extract the estimated mean and standard error of each covariate per quantile
cov_table <- lapply(cov_means.sorted, function(cov_mean) {
  as.data.frame(t(coef(summary(cov_mean))[, c("Estimate", "Std. Error")]))
})

## F test
# Describes the process of performing F-tests to compare means across quantiles. 

# Create the hypothesis test string to check if the first quantile (ntile1) equals the other quantiles
hypothesis_test <- paste0("ntile1 = ", paste0("ntile", seq(2, num_tiles)))

# Perform F-tests for the hypothesis and extract p-values for each covariate
ftest_pvalue_test <- lapply(cov_means.sorted, function(cov_mean) {
  format(round(signif(linearHypothesis(cov_mean, hypothesis_test)[2, "Pr(>F)"], digits = 4), 3), nsmall = 4)
})

# Little trick to display the standard errors
rm(table1)
table1 <- lapply(seq_along(cov_sorted), function(j) {
  m <- rbind(format(round(signif(cov_table[[j]], digits = 4), 3), nsmall = 3))
  m["Estimate", ] <- as.character(m["Estimate", ])
  m["Std. Error", ] <- paste0("(", m["Std. Error", ], ")")
  m
})
table1 <- do.call(rbind, table1)

# Add F-test p-values and sorted covariate variation to the table
ftest <- rep("", nrow(table1))
ftest[seq(1, length(ftest), 2)] <- ftest_pvalue_test
ftest <- unlist(ftest)

# Format the sorted covariate variation for the table
sorted_cov_variation_r <- format(round(sorted_cov_variation, 4), nsmall = 4)
variation <- rep("", nrow(table1))
variation[seq(1, length(variation), 2)] <- sorted_cov_variation_r

# Covariate names for the table
covnames <- rep("", nrow(table1))
covnames[seq(1, length(covnames), 2)] <- cov_sorted

# Combine the covariate names, variations, and table contents into a final table
table1 <- cbind(covariates = covnames, variation, table1, ftest)

# Title of the table
caption <- "Average covariate values in each quantile"

# Display the final table using 'kable' with HTML formatting
#table1 %>%
#  kable(format = "html", digits = 2, caption = caption, escape = FALSE, row.names = FALSE) %>%
#  kable_styling(bootstrap_options = c("condensed", "responsive"), full_width = FALSE)

# Save the final table to a CSV file
write.csv(table1, paste(outputs_folder,i, "covariates_analysis.csv"))

## Save the results

# Save the causal forest model as an RDS file
#saveRDS(cf_grf, file = paste(outputs_folder,i, "IAPT_forest.Rds"))

# Save the entire workspace to an RData file
#save.image(file = paste(outputs_folder,i, "IAPT_forest.RData"))  
