library(dplyr)
library(readr)
library(ggplot2)
library(sjmisc)
library(tidyverse)
library(scales)
library(ggthemes)
library(repr)
library(mediation)
library(combinat)
library(ggrepel)
library(ggtext)
library(showtext)
library(emmeans)
library(yhat)
library(anytime)
library(stargazer)


# ====================  Main manuscript --- Table 1 ==================


rm(list = ls())
final_Salary_data <- read.csv("final_Salary_data_shareable.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')


final_Salary_data <- final_Salary_data %>%
  mutate(s_level = ifelse(standard_level == "DS", "DS", ifelse(standard_level == "SWE", "SWE", "Senior SWE")))



final_Salary_data %>%
  group_by(s_level, gender) %>%
  summarise(number = n())


top_30 <- final_Salary_data %>%
  top_n(n = 3400, wt = total_yearly_compensation) #3409


top_30 %>%
  group_by(s_level, gender) %>%
  summarise(
    group_size = n()
  )









# ===================================== Main Manuscript --- Plot 1 =============================================



rm(list = ls())
  
format.money  <- function(x, ...) {
  ifelse(x>0, paste0("$", formatC(as.numeric(x/1000), format="f", digits=0, big.mark=","), 'K'), x)
}


final_Salary_data <- read.csv("final_Salary_data_shareable.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')


final_Salary_data <- final_Salary_data %>%
  mutate(s_level = ifelse(standard_level == "DS", "DS", ifelse(standard_level == "SWE", "SWE", "Senior SWE")))


categorical_var_US <-c('company','city','gender','Race','Education', "s_level")
final_Salary_data[categorical_var_US] <- lapply(final_Salary_data[categorical_var_US], as.factor)

final_Salary_data$gender <- relevel(final_Salary_data$gender, ref = "Male")
final_Salary_data$s_level <- relevel(final_Salary_data$s_level, ref = "SWE")
final_Salary_data$Race <- relevel(final_Salary_data$Race, ref = "Asian")
final_Salary_data$Education <- relevel(final_Salary_data$Education, ref = "MA or higher")
final_Salary_data$company <- relevel(final_Salary_data$company, ref = "amazon")
final_Salary_data$city <- relevel(final_Salary_data$city, ref = "Seattle")




# ------ all salary records -------

base_salary_all_basis <- final_Salary_data %>%
  filter(gender == "Male") %>%
  summarise(avg_base = exp(mean(log_salary))-1); base_salary_all_basis

compensation_all_basis <- final_Salary_data %>%
  filter(gender == "Male") %>%
  summarise(avg_comp = exp(mean(log_compensation))-1); compensation_all_basis

# --------------------------------------------------------------------------- log_salary --------------------------------------------------------------------------------
#------ Model with gender only ------

lm_1_base_salary_all <- lm(log_salary ~ gender + year, data = final_Salary_data)
lm_1_base_salary_all_summary <- summary(lm_1_base_salary_all); lm_1_base_salary_all_summary

lm_1_base_salary_all_summary_coef <- lm_1_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_1_base_salary_all_summary_coef
lm_1_base_salary_all_summary_std_error <- lm_1_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_1_base_salary_all_summary_std_error


lm_1_base_salary_all_avg_pred <- data.frame(gender_coef = lm_1_base_salary_all_summary_coef,
                                            gender_std_error = lm_1_base_salary_all_summary_std_error) ; lm_1_base_salary_all_avg_pred

pred_salary_all_gender_value_avg <- cbind(lm_1_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_gender_value_avg
pred_salary_all_gender_value_avg$factor <- 1; pred_salary_all_gender_value_avg

plot_salary_all_gender <- pred_salary_all_gender_value_avg ; plot_salary_all_gender




#------ Gender + race ------

lm_2_base_salary_all <- lm(log_salary ~ gender + Race + year, data = final_Salary_data)
lm_2_base_salary_all_summary <- summary(lm_2_base_salary_all); lm_2_base_salary_all_summary

lm_2_base_salary_all_summary_coef <- lm_2_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_2_base_salary_all_summary_coef
lm_2_base_salary_all_summary_std_error <- lm_2_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_2_base_salary_all_summary_std_error


lm_2_base_salary_all_avg_pred <- data.frame(gender_coef = lm_2_base_salary_all_summary_coef,
                                            gender_std_error = lm_2_base_salary_all_summary_std_error) ; lm_2_base_salary_all_avg_pred

pred_salary_all_race_value_avg <- cbind(lm_2_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_race_value_avg
pred_salary_all_race_value_avg$factor <- 2; pred_salary_all_race_value_avg

plot_salary_all_race <- pred_salary_all_race_value_avg ; plot_salary_all_race




#------ Gender + race + Education ------

lm_3_base_salary_all <- lm(log_salary ~ gender + Race + Education + year, data = final_Salary_data)
lm_3_base_salary_all_summary <- summary(lm_3_base_salary_all); lm_3_base_salary_all_summary

lm_3_base_salary_all_summary_coef <- lm_3_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_3_base_salary_all_summary_coef
lm_3_base_salary_all_summary_std_error <- lm_3_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_3_base_salary_all_summary_std_error


lm_3_base_salary_all_avg_pred <- data.frame(gender_coef = lm_3_base_salary_all_summary_coef,
                                            gender_std_error = lm_3_base_salary_all_summary_std_error) ; lm_3_base_salary_all_avg_pred

pred_salary_all_education_value_avg <- cbind(lm_3_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_education_value_avg
pred_salary_all_education_value_avg$factor <- 3; pred_salary_all_education_value_avg

plot_salary_all_education <- pred_salary_all_education_value_avg ; plot_salary_all_education





#------ Gender + Race + Education + Company ------

lm_4_base_salary_all <- lm(log_salary ~ gender + Race + Education + company + year, data = final_Salary_data)
lm_4_base_salary_all_summary <- summary(lm_4_base_salary_all); lm_4_base_salary_all_summary

lm_4_base_salary_all_summary_coef <- lm_4_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_4_base_salary_all_summary_coef
lm_4_base_salary_all_summary_std_error <- lm_4_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_4_base_salary_all_summary_std_error


lm_4_base_salary_all_avg_pred <- data.frame(gender_coef = lm_4_base_salary_all_summary_coef,
                                            gender_std_error = lm_4_base_salary_all_summary_std_error) ; lm_4_base_salary_all_avg_pred

pred_salary_all_company_value_avg <- cbind(lm_4_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_company_value_avg
pred_salary_all_company_value_avg$factor <- 4; pred_salary_all_company_value_avg

plot_salary_all_company <- pred_salary_all_company_value_avg ; plot_salary_all_company



#------ Gender + race + Education + Company + levels ------

lm_5_base_salary_all <- lm(log_salary ~ gender + Race + Education + company + year + s_level, data = final_Salary_data)
lm_5_base_salary_all_summary <- summary(lm_5_base_salary_all); lm_5_base_salary_all_summary

lm_5_base_salary_all_summary_coef <- lm_5_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_5_base_salary_all_summary_coef
lm_5_base_salary_all_summary_std_error <- lm_5_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_5_base_salary_all_summary_std_error


lm_5_base_salary_all_avg_pred <- data.frame(gender_coef = lm_5_base_salary_all_summary_coef,
                                            gender_std_error = lm_5_base_salary_all_summary_std_error) ; lm_5_base_salary_all_avg_pred

pred_salary_all_level_value_avg <- cbind(lm_5_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_level_value_avg
pred_salary_all_level_value_avg$factor <- 5; pred_salary_all_level_value_avg

plot_salary_all_level <- pred_salary_all_level_value_avg ; plot_salary_all_level





#------ Gender + race + Education + Company + Levels + location(city) ------

lm_location_base_salary_all <- lm(log_salary ~ gender + Race + Education + year + company + s_level + city, data = final_Salary_data)
lm_location_base_salary_all_summary <- summary(lm_location_base_salary_all); lm_location_base_salary_all_summary

lm_6_base_salary_all_summary_coef <- lm_location_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_6_base_salary_all_summary_coef
lm_6_base_salary_all_summary_std_error <- lm_location_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_6_base_salary_all_summary_std_error


lm_6_base_salary_all_avg_pred <- data.frame(gender_coef = lm_6_base_salary_all_summary_coef,
                                            gender_std_error = lm_6_base_salary_all_summary_std_error) ; lm_6_base_salary_all_avg_pred

pred_salary_all_city_value_avg <- cbind(lm_6_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_city_value_avg
pred_salary_all_city_value_avg$factor <- 6; pred_salary_all_city_value_avg

plot_salary_all_city <- pred_salary_all_city_value_avg ; plot_salary_all_city







#------ Gender + race + Education + Company + Levels + location(city) + Working Experience ------

lm_experience_base_salary_all <- lm(log_salary ~ gender + Race + Education + year + company + s_level + years_of_experience + city, data = final_Salary_data)
lm_experience_base_salary_all_summary <- summary(lm_experience_base_salary_all); lm_experience_base_salary_all_summary

lm_7_base_salary_all_summary_coef <- lm_experience_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_7_base_salary_all_summary_coef
lm_7_base_salary_all_summary_std_error <- lm_experience_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_7_base_salary_all_summary_std_error


lm_7_base_salary_all_avg_pred <- data.frame(gender_coef = lm_7_base_salary_all_summary_coef,
                                            gender_std_error = lm_7_base_salary_all_summary_std_error) ; lm_7_base_salary_all_avg_pred

pred_salary_all_Experience_value_avg <- cbind(lm_7_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_Experience_value_avg
pred_salary_all_Experience_value_avg$factor <- 7; pred_salary_all_Experience_value_avg

plot_salary_all_experience <- pred_salary_all_Experience_value_avg ; plot_salary_all_experience




#------ Gender + race + Education + Company + Levels + location(city) + Working Experience + Experience proportion ------
lm_experience_proportion_base_salary_all <- lm(log_salary ~ gender + Race + Education + year + company + s_level + years_of_experience + exp + city, data = final_Salary_data)
lm_experience_proportion_base_salary_all_summary <- summary(lm_experience_proportion_base_salary_all); lm_experience_proportion_base_salary_all_summary

lm_8_base_salary_all_summary_coef <- lm_experience_proportion_base_salary_all_summary$coefficients[ , 1]['genderFemale']; lm_8_base_salary_all_summary_coef
lm_8_base_salary_all_summary_std_error <- lm_experience_proportion_base_salary_all_summary$coefficients[ , 2]['genderFemale']; lm_8_base_salary_all_summary_std_error


lm_8_base_salary_all_avg_pred <- data.frame(gender_coef = lm_8_base_salary_all_summary_coef,
                                            gender_std_error = lm_8_base_salary_all_summary_std_error) ; lm_8_base_salary_all_avg_pred

pred_salary_all_proportion_value_avg <- cbind(lm_8_base_salary_all_avg_pred, base_salary_all_basis); pred_salary_all_proportion_value_avg
pred_salary_all_proportion_value_avg$factor <- 8; pred_salary_all_proportion_value_avg

plot_salary_all_experience_proportion <- pred_salary_all_proportion_value_avg ; plot_salary_all_experience_proportion




pred_base_salary_all <- rbind(plot_salary_all_gender, plot_salary_all_race, plot_salary_all_education,
                              plot_salary_all_company, plot_salary_all_level, plot_salary_all_city,
                              plot_salary_all_experience, plot_salary_all_experience_proportion); pred_base_salary_all

pred_base_salary_all$factor <- ordered(pred_base_salary_all$factor, levels=c(1,2,3,4,5,6,7,8)); pred_base_salary_all




pred_base_salary_all$gender_coef_number <-  pred_base_salary_all$avg_base * (-pred_base_salary_all$gender_coef); pred_base_salary_all
pred_base_salary_all$gender_std_error_number <-  pred_base_salary_all$avg_base * pred_base_salary_all$gender_std_error; pred_base_salary_all




# --------------------------------------------------------------------------- log_compensation --------------------------------------------------------------------------------
#------ Model with gender only ------

lm_1_compensation_all <- lm(log_compensation ~ gender + year, data = final_Salary_data)
lm_1_compensation_all_summary <- summary(lm_1_compensation_all); lm_1_compensation_all_summary

lm_1_compensation_all_summary_coef <- lm_1_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_1_compensation_all_summary_coef
lm_1_compensation_all_summary_std_error <- lm_1_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_1_compensation_all_summary_std_error


lm_1_compensation_all_avg_pred <- data.frame(gender_coef = lm_1_compensation_all_summary_coef,
                                             gender_std_error = lm_1_compensation_all_summary_std_error) ; lm_1_compensation_all_avg_pred

pred_salary_gender_value_avg <- cbind(lm_1_compensation_all_avg_pred, compensation_all_basis); pred_salary_gender_value_avg
pred_salary_gender_value_avg$factor <- 1; pred_salary_gender_value_avg

plot_salary_gender <- pred_salary_gender_value_avg ; plot_salary_gender




#------ Gender + race ------

lm_2_compensation_all <- lm(log_compensation ~ gender + Race + year, data = final_Salary_data)
lm_2_compensation_all_summary <- summary(lm_2_compensation_all); lm_2_compensation_all_summary

lm_2_compensation_all_summary_coef <- lm_2_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_2_compensation_all_summary_coef
lm_2_compensation_all_summary_std_error <- lm_2_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_2_compensation_all_summary_std_error


lm_2_compensation_all_avg_pred <- data.frame(gender_coef = lm_2_compensation_all_summary_coef,
                                             gender_std_error = lm_2_compensation_all_summary_std_error) ; lm_2_compensation_all_avg_pred

pred_salary_race_value_avg <- cbind(lm_2_compensation_all_avg_pred, compensation_all_basis); pred_salary_race_value_avg
pred_salary_race_value_avg$factor <- 2; pred_salary_race_value_avg

plot_salary_race <- pred_salary_race_value_avg ; plot_salary_race




#------ Gender + race + Education ------

lm_3_compensation_all <- lm(log_compensation ~ gender + Race + Education + year, data = final_Salary_data)
lm_3_compensation_all_summary <- summary(lm_3_compensation_all); lm_3_compensation_all_summary

lm_3_compensation_all_summary_coef <- lm_3_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_3_compensation_all_summary_coef
lm_3_compensation_all_summary_std_error <- lm_3_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_3_compensation_all_summary_std_error


lm_3_compensation_all_avg_pred <- data.frame(gender_coef = lm_3_compensation_all_summary_coef,
                                             gender_std_error = lm_3_compensation_all_summary_std_error) ; lm_3_compensation_all_avg_pred

pred_salary_education_value_avg <- cbind(lm_3_compensation_all_avg_pred, compensation_all_basis); pred_salary_education_value_avg
pred_salary_education_value_avg$factor <- 3; pred_salary_education_value_avg

plot_salary_education <- pred_salary_education_value_avg ; plot_salary_education





#------ Gender + Race + Education + Company ------

lm_4_compensation_all <- lm(log_compensation ~ gender + Race + Education + company + year, data = final_Salary_data)
lm_4_compensation_all_summary <- summary(lm_4_compensation_all); lm_4_compensation_all_summary

lm_4_compensation_all_summary_coef <- lm_4_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_4_compensation_all_summary_coef
lm_4_compensation_all_summary_std_error <- lm_4_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_4_compensation_all_summary_std_error


lm_4_compensation_all_avg_pred <- data.frame(gender_coef = lm_4_compensation_all_summary_coef,
                                             gender_std_error = lm_4_compensation_all_summary_std_error) ; lm_4_compensation_all_avg_pred

pred_salary_company_value_avg <- cbind(lm_4_compensation_all_avg_pred, compensation_all_basis); pred_salary_company_value_avg
pred_salary_company_value_avg$factor <- 4; pred_salary_company_value_avg

plot_salary_company <- pred_salary_company_value_avg ; plot_salary_company



#------ Gender + race + Education + Company + levels ------

lm_5_compensation_all <- lm(log_compensation ~ gender + Race + Education + company + year + s_level, data = final_Salary_data)
lm_5_compensation_all_summary <- summary(lm_5_compensation_all); lm_5_compensation_all_summary

lm_5_compensation_all_summary_coef <- lm_5_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_5_compensation_all_summary_coef
lm_5_compensation_all_summary_std_error <- lm_5_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_5_compensation_all_summary_std_error


lm_5_compensation_all_avg_pred <- data.frame(gender_coef = lm_5_compensation_all_summary_coef,
                                             gender_std_error = lm_5_compensation_all_summary_std_error) ; lm_5_compensation_all_avg_pred

pred_salary_level_value_avg <- cbind(lm_5_compensation_all_avg_pred, compensation_all_basis); pred_salary_level_value_avg
pred_salary_level_value_avg$factor <- 5; pred_salary_level_value_avg

plot_salary_level <- pred_salary_level_value_avg ; plot_salary_level





#------ Gender + race + Education + Company + Levels + location(city) ------

lm_location_compensation_all <- lm(log_compensation ~ gender + Race + Education + year + company + s_level + city, data = final_Salary_data)
lm_location_compensation_all_summary <- summary(lm_location_compensation_all); lm_location_compensation_all_summary

lm_6_compensation_all_summary_coef <- lm_location_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_6_compensation_all_summary_coef
lm_6_compensation_all_summary_std_error <- lm_location_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_6_compensation_all_summary_std_error


lm_6_compensation_all_avg_pred <- data.frame(gender_coef = lm_6_compensation_all_summary_coef,
                                             gender_std_error = lm_6_compensation_all_summary_std_error) ; lm_6_compensation_all_avg_pred

pred_salary_city_value_avg <- cbind(lm_6_compensation_all_avg_pred, compensation_all_basis); pred_salary_city_value_avg
pred_salary_city_value_avg$factor <- 6; pred_salary_city_value_avg

plot_salary_city <- pred_salary_city_value_avg ; plot_salary_city







#------ Gender + race + Education + Company + Levels + location(city) + Working Experience ------

lm_experience_compensation_all <- lm(log_compensation ~ gender + Race + Education + year + company + s_level + years_of_experience + city, data = final_Salary_data)
lm_experience_compensation_all_summary <- summary(lm_experience_compensation_all); lm_experience_compensation_all_summary

lm_7_compensation_all_summary_coef <- lm_experience_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_7_compensation_all_summary_coef
lm_7_compensation_all_summary_std_error <- lm_experience_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_7_compensation_all_summary_std_error


lm_7_compensation_all_avg_pred <- data.frame(gender_coef = lm_7_compensation_all_summary_coef,
                                             gender_std_error = lm_7_compensation_all_summary_std_error) ; lm_7_compensation_all_avg_pred

pred_salary_Experience_value_avg <- cbind(lm_7_compensation_all_avg_pred, compensation_all_basis); pred_salary_Experience_value_avg
pred_salary_Experience_value_avg$factor <- 7; pred_salary_Experience_value_avg

plot_salary_experience <- pred_salary_Experience_value_avg ; plot_salary_experience




#------ Gender + race + Education + Company + Levels + location(city) + Working Experience + Experience proportion ------
lm_experience_proportion_compensation_all <- lm(log_compensation ~ gender + Race + Education + year + company + s_level + years_of_experience + exp + city, data = final_Salary_data)
lm_experience_proportion_compensation_all_summary <- summary(lm_experience_proportion_compensation_all); lm_experience_proportion_compensation_all_summary

lm_8_compensation_all_summary_coef <- lm_experience_proportion_compensation_all_summary$coefficients[ , 1]['genderFemale']; lm_8_compensation_all_summary_coef
lm_8_compensation_all_summary_std_error <- lm_experience_proportion_compensation_all_summary$coefficients[ , 2]['genderFemale']; lm_8_compensation_all_summary_std_error


lm_8_compensation_all_avg_pred <- data.frame(gender_coef = lm_8_compensation_all_summary_coef,
                                             gender_std_error = lm_8_compensation_all_summary_std_error) ; lm_8_compensation_all_avg_pred

pred_salary_proportion_value_avg <- cbind(lm_8_compensation_all_avg_pred, compensation_all_basis); pred_salary_proportion_value_avg
pred_salary_proportion_value_avg$factor <- 8; pred_salary_proportion_value_avg

plot_salary_experience_proportion <- pred_salary_proportion_value_avg ; plot_salary_experience_proportion




pred_compensation_all <- rbind(plot_salary_gender, plot_salary_race, plot_salary_education,
                               plot_salary_company, plot_salary_level, plot_salary_city,
                               plot_salary_experience, plot_salary_experience_proportion); pred_compensation_all

pred_compensation_all$factor <- ordered(pred_compensation_all$factor, levels=c(1,2,3,4,5,6,7,8)); pred_compensation_all




pred_compensation_all$gender_coef_number <-  pred_compensation_all$avg_comp * (-pred_compensation_all$gender_coef); pred_compensation_all
pred_compensation_all$gender_std_error_number <-  pred_compensation_all$avg_comp * pred_compensation_all$gender_std_error; pred_compensation_all



# ================================== Top 30 ==================================




top_30_Salary_data <- final_Salary_data %>%
  top_n(n = 3400, wt = total_yearly_compensation) #3409




# ------ top salary records -------

base_salary_top_basis <- top_30_Salary_data %>%
  filter(gender == "Male") %>%
  summarise(avg_base = exp(mean(log_salary))-1); base_salary_top_basis

compensation_top_basis <- top_30_Salary_data %>%
  filter(gender == "Male") %>%
  summarise(avg_comp = exp(mean(log_compensation))-1); compensation_top_basis

# --------------------------------------------------------------------------- log_salary --------------------------------------------------------------------------------
#------ Model with gender only ------

lm_1_base_salary_top <- lm(log_salary ~ gender + year, data = top_30_Salary_data)
lm_1_base_salary_top_summary <- summary(lm_1_base_salary_top); lm_1_base_salary_top_summary

lm_1_base_salary_top_summary_coef <- lm_1_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_1_base_salary_top_summary_coef
lm_1_base_salary_top_summary_std_error <- lm_1_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_1_base_salary_top_summary_std_error


lm_1_base_salary_top_avg_pred <- data.frame(gender_coef = lm_1_base_salary_top_summary_coef,
                                            gender_std_error = lm_1_base_salary_top_summary_std_error) ; lm_1_base_salary_top_avg_pred

pred_salary_top_gender_value_avg <- cbind(lm_1_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_gender_value_avg
pred_salary_top_gender_value_avg$factor <- 1; pred_salary_top_gender_value_avg

plot_salary_top_gender <- pred_salary_top_gender_value_avg ; plot_salary_top_gender




#------ Gender + race ------

lm_2_base_salary_top <- lm(log_salary ~ gender + Race + year, data = top_30_Salary_data)
lm_2_base_salary_top_summary <- summary(lm_2_base_salary_top); lm_2_base_salary_top_summary

lm_2_base_salary_top_summary_coef <- lm_2_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_2_base_salary_top_summary_coef
lm_2_base_salary_top_summary_std_error <- lm_2_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_2_base_salary_top_summary_std_error


lm_2_base_salary_top_avg_pred <- data.frame(gender_coef = lm_2_base_salary_top_summary_coef,
                                            gender_std_error = lm_2_base_salary_top_summary_std_error) ; lm_2_base_salary_top_avg_pred

pred_salary_top_race_value_avg <- cbind(lm_2_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_race_value_avg
pred_salary_top_race_value_avg$factor <- 2; pred_salary_top_race_value_avg

plot_salary_top_race <- pred_salary_top_race_value_avg ; plot_salary_top_race




#------ Gender + race + Education ------

lm_3_base_salary_top <- lm(log_salary ~ gender + Race + Education + year, data = top_30_Salary_data)
lm_3_base_salary_top_summary <- summary(lm_3_base_salary_top); lm_3_base_salary_top_summary

lm_3_base_salary_top_summary_coef <- lm_3_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_3_base_salary_top_summary_coef
lm_3_base_salary_top_summary_std_error <- lm_3_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_3_base_salary_top_summary_std_error


lm_3_base_salary_top_avg_pred <- data.frame(gender_coef = lm_3_base_salary_top_summary_coef,
                                            gender_std_error = lm_3_base_salary_top_summary_std_error) ; lm_3_base_salary_top_avg_pred

pred_salary_top_education_value_avg <- cbind(lm_3_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_education_value_avg
pred_salary_top_education_value_avg$factor <- 3; pred_salary_top_education_value_avg

plot_salary_top_education <- pred_salary_top_education_value_avg ; plot_salary_top_education





#------ Gender + Race + Education + Company ------

lm_4_base_salary_top <- lm(log_salary ~ gender + Race + Education + company + year, data = top_30_Salary_data)
lm_4_base_salary_top_summary <- summary(lm_4_base_salary_top); lm_4_base_salary_top_summary

lm_4_base_salary_top_summary_coef <- lm_4_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_4_base_salary_top_summary_coef
lm_4_base_salary_top_summary_std_error <- lm_4_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_4_base_salary_top_summary_std_error


lm_4_base_salary_top_avg_pred <- data.frame(gender_coef = lm_4_base_salary_top_summary_coef,
                                            gender_std_error = lm_4_base_salary_top_summary_std_error) ; lm_4_base_salary_top_avg_pred

pred_salary_top_company_value_avg <- cbind(lm_4_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_company_value_avg
pred_salary_top_company_value_avg$factor <- 4; pred_salary_top_company_value_avg

plot_salary_top_company <- pred_salary_top_company_value_avg ; plot_salary_top_company



#------ Gender + race + Education + Company + levels ------

lm_5_base_salary_top <- lm(log_salary ~ gender + Race + Education + company + year + s_level, data = top_30_Salary_data)
lm_5_base_salary_top_summary <- summary(lm_5_base_salary_top); lm_5_base_salary_top_summary

lm_5_base_salary_top_summary_coef <- lm_5_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_5_base_salary_top_summary_coef
lm_5_base_salary_top_summary_std_error <- lm_5_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_5_base_salary_top_summary_std_error


lm_5_base_salary_top_avg_pred <- data.frame(gender_coef = lm_5_base_salary_top_summary_coef,
                                            gender_std_error = lm_5_base_salary_top_summary_std_error) ; lm_5_base_salary_top_avg_pred

pred_salary_top_level_value_avg <- cbind(lm_5_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_level_value_avg
pred_salary_top_level_value_avg$factor <- 5; pred_salary_top_level_value_avg

plot_salary_top_level <- pred_salary_top_level_value_avg ; plot_salary_top_level





#------ Gender + race + Education + Company + Levels + location(city) ------

lm_location_base_salary_top <- lm(log_salary ~ gender + Race + Education + year + company + s_level + city, data = top_30_Salary_data)
lm_location_base_salary_top_summary <- summary(lm_location_base_salary_top); lm_location_base_salary_top_summary

lm_6_base_salary_top_summary_coef <- lm_location_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_6_base_salary_top_summary_coef
lm_6_base_salary_top_summary_std_error <- lm_location_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_6_base_salary_top_summary_std_error


lm_6_base_salary_top_avg_pred <- data.frame(gender_coef = lm_6_base_salary_top_summary_coef,
                                            gender_std_error = lm_6_base_salary_top_summary_std_error) ; lm_6_base_salary_top_avg_pred

pred_salary_top_city_value_avg <- cbind(lm_6_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_city_value_avg
pred_salary_top_city_value_avg$factor <- 6; pred_salary_top_city_value_avg

plot_salary_top_city <- pred_salary_top_city_value_avg ; plot_salary_top_city







#------ Gender + race + Education + Company + Levels + location(city) + Working Experience ------

lm_experience_base_salary_top <- lm(log_salary ~ gender + Race + Education + year + company + s_level + years_of_experience + city, data = top_30_Salary_data)
lm_experience_base_salary_top_summary <- summary(lm_experience_base_salary_top); lm_experience_base_salary_top_summary

lm_7_base_salary_top_summary_coef <- lm_experience_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_7_base_salary_top_summary_coef
lm_7_base_salary_top_summary_std_error <- lm_experience_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_7_base_salary_top_summary_std_error


lm_7_base_salary_top_avg_pred <- data.frame(gender_coef = lm_7_base_salary_top_summary_coef,
                                            gender_std_error = lm_7_base_salary_top_summary_std_error) ; lm_7_base_salary_top_avg_pred

pred_salary_top_Experience_value_avg <- cbind(lm_7_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_Experience_value_avg
pred_salary_top_Experience_value_avg$factor <- 7; pred_salary_top_Experience_value_avg

plot_salary_top_experience <- pred_salary_top_Experience_value_avg ; plot_salary_top_experience




#------ Gender + race + Education + Company + Levels + location(city) + Working Experience + Experience proportion ------
lm_experience_proportion_base_salary_top <- lm(log_salary ~ gender + Race + Education + year + company + s_level + years_of_experience + exp + city, data = top_30_Salary_data)
lm_experience_proportion_base_salary_top_summary <- summary(lm_experience_proportion_base_salary_top); lm_experience_proportion_base_salary_top_summary

lm_8_base_salary_top_summary_coef <- lm_experience_proportion_base_salary_top_summary$coefficients[ , 1]['genderFemale']; lm_8_base_salary_top_summary_coef
lm_8_base_salary_top_summary_std_error <- lm_experience_proportion_base_salary_top_summary$coefficients[ , 2]['genderFemale']; lm_8_base_salary_top_summary_std_error


lm_8_base_salary_top_avg_pred <- data.frame(gender_coef = lm_8_base_salary_top_summary_coef,
                                            gender_std_error = lm_8_base_salary_top_summary_std_error) ; lm_8_base_salary_top_avg_pred

pred_salary_top_proportion_value_avg <- cbind(lm_8_base_salary_top_avg_pred, base_salary_top_basis); pred_salary_top_proportion_value_avg
pred_salary_top_proportion_value_avg$factor <- 8; pred_salary_top_proportion_value_avg

plot_salary_top_experience_proportion <- pred_salary_top_proportion_value_avg ; plot_salary_top_experience_proportion




pred_base_salary_top <- rbind(plot_salary_top_gender, plot_salary_top_race, plot_salary_top_education,
                              plot_salary_top_company, plot_salary_top_level, plot_salary_top_city,
                              plot_salary_top_experience, plot_salary_top_experience_proportion); pred_base_salary_top

pred_base_salary_top$factor <- ordered(pred_base_salary_top$factor, levels=c(1,2,3,4,5,6,7,8)); pred_base_salary_top




pred_base_salary_top$gender_coef_number <-  pred_base_salary_top$avg_base * (-pred_base_salary_top$gender_coef); pred_base_salary_top
pred_base_salary_top$gender_std_error_number <-  pred_base_salary_top$avg_base * pred_base_salary_top$gender_std_error; pred_base_salary_top




# --------------------------------------------------------------------------- log_compensation --------------------------------------------------------------------------------
#------ Model with gender only ------

lm_1_compensation_top <- lm(log_compensation ~ gender + year, data = top_30_Salary_data)
lm_1_compensation_top_summary <- summary(lm_1_compensation_top); lm_1_compensation_top_summary

lm_1_compensation_top_summary_coef <- lm_1_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_1_compensation_top_summary_coef
lm_1_compensation_top_summary_std_error <- lm_1_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_1_compensation_top_summary_std_error


lm_1_compensation_top_avg_pred <- data.frame(gender_coef = lm_1_compensation_top_summary_coef,
                                             gender_std_error = lm_1_compensation_top_summary_std_error) ; lm_1_compensation_top_avg_pred

pred_salary_gender_value_avg <- cbind(lm_1_compensation_top_avg_pred, compensation_top_basis); pred_salary_gender_value_avg
pred_salary_gender_value_avg$factor <- 1; pred_salary_gender_value_avg

plot_salary_gender <- pred_salary_gender_value_avg ; plot_salary_gender




#------ Gender + race ------

lm_2_compensation_top <- lm(log_compensation ~ gender + Race + year, data = top_30_Salary_data)
lm_2_compensation_top_summary <- summary(lm_2_compensation_top); lm_2_compensation_top_summary

lm_2_compensation_top_summary_coef <- lm_2_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_2_compensation_top_summary_coef
lm_2_compensation_top_summary_std_error <- lm_2_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_2_compensation_top_summary_std_error


lm_2_compensation_top_avg_pred <- data.frame(gender_coef = lm_2_compensation_top_summary_coef,
                                             gender_std_error = lm_2_compensation_top_summary_std_error) ; lm_2_compensation_top_avg_pred

pred_salary_race_value_avg <- cbind(lm_2_compensation_top_avg_pred, compensation_top_basis); pred_salary_race_value_avg
pred_salary_race_value_avg$factor <- 2; pred_salary_race_value_avg

plot_salary_race <- pred_salary_race_value_avg ; plot_salary_race




#------ Gender + race + Education ------

lm_3_compensation_top <- lm(log_compensation ~ gender + Race + Education + year, data = top_30_Salary_data)
lm_3_compensation_top_summary <- summary(lm_3_compensation_top); lm_3_compensation_top_summary

lm_3_compensation_top_summary_coef <- lm_3_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_3_compensation_top_summary_coef
lm_3_compensation_top_summary_std_error <- lm_3_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_3_compensation_top_summary_std_error


lm_3_compensation_top_avg_pred <- data.frame(gender_coef = lm_3_compensation_top_summary_coef,
                                             gender_std_error = lm_3_compensation_top_summary_std_error) ; lm_3_compensation_top_avg_pred

pred_salary_education_value_avg <- cbind(lm_3_compensation_top_avg_pred, compensation_top_basis); pred_salary_education_value_avg
pred_salary_education_value_avg$factor <- 3; pred_salary_education_value_avg

plot_salary_education <- pred_salary_education_value_avg ; plot_salary_education





#------ Gender + Race + Education + Company ------

lm_4_compensation_top <- lm(log_compensation ~ gender + Race + Education + company + year, data = top_30_Salary_data)
lm_4_compensation_top_summary <- summary(lm_4_compensation_top); lm_4_compensation_top_summary

lm_4_compensation_top_summary_coef <- lm_4_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_4_compensation_top_summary_coef
lm_4_compensation_top_summary_std_error <- lm_4_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_4_compensation_top_summary_std_error


lm_4_compensation_top_avg_pred <- data.frame(gender_coef = lm_4_compensation_top_summary_coef,
                                             gender_std_error = lm_4_compensation_top_summary_std_error) ; lm_4_compensation_top_avg_pred

pred_salary_company_value_avg <- cbind(lm_4_compensation_top_avg_pred, compensation_top_basis); pred_salary_company_value_avg
pred_salary_company_value_avg$factor <- 4; pred_salary_company_value_avg

plot_salary_company <- pred_salary_company_value_avg ; plot_salary_company



#------ Gender + race + Education + Company + levels ------

lm_5_compensation_top <- lm(log_compensation ~ gender + Race + Education + company + year + s_level, data = top_30_Salary_data)
lm_5_compensation_top_summary <- summary(lm_5_compensation_top); lm_5_compensation_top_summary

lm_5_compensation_top_summary_coef <- lm_5_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_5_compensation_top_summary_coef
lm_5_compensation_top_summary_std_error <- lm_5_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_5_compensation_top_summary_std_error


lm_5_compensation_top_avg_pred <- data.frame(gender_coef = lm_5_compensation_top_summary_coef,
                                             gender_std_error = lm_5_compensation_top_summary_std_error) ; lm_5_compensation_top_avg_pred

pred_salary_level_value_avg <- cbind(lm_5_compensation_top_avg_pred, compensation_top_basis); pred_salary_level_value_avg
pred_salary_level_value_avg$factor <- 5; pred_salary_level_value_avg

plot_salary_level <- pred_salary_level_value_avg ; plot_salary_level





#------ Gender + race + Education + Company + Levels + location(city) ------

lm_location_compensation_top <- lm(log_compensation ~ gender + Race + Education + year + company + s_level + city, data = top_30_Salary_data)
lm_location_compensation_top_summary <- summary(lm_location_compensation_top); lm_location_compensation_top_summary

lm_6_compensation_top_summary_coef <- lm_location_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_6_compensation_top_summary_coef
lm_6_compensation_top_summary_std_error <- lm_location_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_6_compensation_top_summary_std_error


lm_6_compensation_top_avg_pred <- data.frame(gender_coef = lm_6_compensation_top_summary_coef,
                                             gender_std_error = lm_6_compensation_top_summary_std_error) ; lm_6_compensation_top_avg_pred

pred_salary_city_value_avg <- cbind(lm_6_compensation_top_avg_pred, compensation_top_basis); pred_salary_city_value_avg
pred_salary_city_value_avg$factor <- 6; pred_salary_city_value_avg

plot_salary_city <- pred_salary_city_value_avg ; plot_salary_city







#------ Gender + race + Education + Company + Levels + location(city) + Working Experience ------

lm_experience_compensation_top <- lm(log_compensation ~ gender + Race + Education + year + company + s_level + years_of_experience + city, data = top_30_Salary_data)
lm_experience_compensation_top_summary <- summary(lm_experience_compensation_top); lm_experience_compensation_top_summary

lm_7_compensation_top_summary_coef <- lm_experience_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_7_compensation_top_summary_coef
lm_7_compensation_top_summary_std_error <- lm_experience_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_7_compensation_top_summary_std_error


lm_7_compensation_top_avg_pred <- data.frame(gender_coef = lm_7_compensation_top_summary_coef,
                                             gender_std_error = lm_7_compensation_top_summary_std_error) ; lm_7_compensation_top_avg_pred

pred_salary_Experience_value_avg <- cbind(lm_7_compensation_top_avg_pred, compensation_top_basis); pred_salary_Experience_value_avg
pred_salary_Experience_value_avg$factor <- 7; pred_salary_Experience_value_avg

plot_salary_experience <- pred_salary_Experience_value_avg ; plot_salary_experience




#------ Gender + race + Education + Company + Levels + location(city) + Working Experience + Experience proportion ------
lm_experience_proportion_compensation_top <- lm(log_compensation ~ gender + Race + Education + year + company + s_level + years_of_experience + exp + city, data = top_30_Salary_data)
lm_experience_proportion_compensation_top_summary <- summary(lm_experience_proportion_compensation_top); lm_experience_proportion_compensation_top_summary

lm_8_compensation_top_summary_coef <- lm_experience_proportion_compensation_top_summary$coefficients[ , 1]['genderFemale']; lm_8_compensation_top_summary_coef
lm_8_compensation_top_summary_std_error <- lm_experience_proportion_compensation_top_summary$coefficients[ , 2]['genderFemale']; lm_8_compensation_top_summary_std_error


lm_8_compensation_top_avg_pred <- data.frame(gender_coef = lm_8_compensation_top_summary_coef,
                                             gender_std_error = lm_8_compensation_top_summary_std_error) ; lm_8_compensation_top_avg_pred

pred_salary_proportion_value_avg <- cbind(lm_8_compensation_top_avg_pred, compensation_top_basis); pred_salary_proportion_value_avg
pred_salary_proportion_value_avg$factor <- 8; pred_salary_proportion_value_avg

plot_salary_experience_proportion <- pred_salary_proportion_value_avg ; plot_salary_experience_proportion




pred_compensation_top <- rbind(plot_salary_gender, plot_salary_race, plot_salary_education,
                               plot_salary_company, plot_salary_level, plot_salary_city,
                               plot_salary_experience, plot_salary_experience_proportion); pred_compensation_top

pred_compensation_top$factor <- ordered(pred_compensation_top$factor, levels=c(1,2,3,4,5,6,7,8)); pred_compensation_top




pred_compensation_top$gender_coef_number <-  pred_compensation_top$avg_comp * (-pred_compensation_top$gender_coef); pred_compensation_top
pred_compensation_top$gender_std_error_number <-  pred_compensation_top$avg_comp * pred_compensation_top$gender_std_error; pred_compensation_top





# ----------------- plot --------------------



plot_base_salary_all <- pred_base_salary_all %>% 
  mutate(category = 'base_all') %>%
  dplyr::rename(basis = avg_base)
plot_base_salary_all$factor <- ordered(plot_base_salary_all$factor, levels=c(1,2,3,4,5,6,7,8))
plot_compensation_all <- pred_compensation_all  %>% 
  mutate(category = 'comp_all')%>%
  dplyr::rename(basis = avg_comp)
plot_compensation_all$factor <- ordered(plot_compensation_all$factor, levels=c(1,2,3,4,5,6,7,8))


plot_base_salary_30 <- pred_base_salary_top %>% 
  mutate(category = 'base_30')%>%
  dplyr::rename(basis = avg_base)
plot_base_salary_30$factor <- ordered(plot_base_salary_30$factor, levels=c(1,2,3,4,5,6,7,8))
plot_compensation_30 <-pred_compensation_top  %>% 
  mutate(category = 'comp_30')%>%
  dplyr::rename(basis = avg_comp)
plot_compensation_30$factor <- ordered(plot_compensation_30$factor, levels=c(1,2,3,4,5,6,7,8))

combined_base_comp_all <- rbind(plot_base_salary_all, plot_compensation_all)
combined_base_comp_all$factor <- ordered(combined_base_comp_all$factor, levels=c(1,2,3,4,5,6,7,8))

combined_base_comp_30 <- rbind(plot_base_salary_30, plot_compensation_30)
combined_base_comp_30$factor <- ordered(combined_base_comp_30$factor, levels=c(1,2,3,4,5,6,7,8))



combined_base_comp_final <- rbind(combined_base_comp_all, combined_base_comp_30)


sig_points_1 <- combined_base_comp_final %>%
  filter(category !='base_30')

sig_points_2 <- combined_base_comp_final %>%
  filter(category =='base_30')%>%
  filter(factor<5)

Not_sig_points <- combined_base_comp_final %>%
  filter(category =='base_30')%>%
  filter(factor>4)

combined_base_comp_final$factor <- ordered(combined_base_comp_final$factor, levels=c(1,2,3,4,5,6,7,8))


combined_base_comp_30 <- combined_base_comp_final %>% filter(category %in% c('base_30', "comp_30"))
combined_base_comp_all <- combined_base_comp_final %>% filter(category %in% c('base_all', "comp_all"))



ggplot(data = combined_base_comp_final, aes(x = factor, y = gender_coef_number, colour = category, group = category, 
                                            ymin = gender_coef_number-gender_std_error_number, ymax = gender_coef_number+gender_std_error_number))  + 
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = sig_points_1, show.legend = F)+
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = sig_points_2, show.legend = F)+
  geom_point(shape = 4,  fill = "white", size = 2, stroke = 3, data = Not_sig_points, show.legend = F)+
  geom_line(linetype = "dashed", data = combined_base_comp_30, size=1.1) + 
  geom_line(linetype = "solid", data = combined_base_comp_all, size=1.1) +
  geom_errorbar(width = 0.1, size = 1) +
  geom_hline(aes(yintercept = 0), size=1, linetype='dashed', col = 'black')+
  annotate("text", x = "2", y = 0, label = "No Gender Pay Gap", vjust = -.5,hjust = 1.5, size=12/.pt)+
  guides(size = "none", colour = guide_legend(nrow = 2, override.aes = list(size=8))) + 
  scale_y_continuous(labels = format.money, breaks = pretty_breaks(8))+
  scale_x_discrete(labels=c("1" = "Gender", "2" = "+ Race", "3" = "+ Education", "4" = "+ Company",
                            "5" = "+ Levels", "6" = "+ Location", "7" = "+ Exp.", "8" = "+ Prop. Exp.")) + 
  scale_color_manual(values=c("#ffcf0d", "#ff7f0d", "#93cfe4", "#0078b7"), labels = c("Base Salary (Top 30)", "Base Salary (All)", "Compensation (Top 30)", "Compensation (All)"))+
  ylab("Gender Coefficient (Predicted Gap)") + xlab("Stepwise Models")  +
  theme(axis.title = element_text(size=12),axis.text= element_text(size=12, colour = 'black'),plot.title= element_text(hjust = .5, size = 12), 
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        legend.position = 'bottom', legend.text = element_text(size=12), legend.title = element_blank(),  
        legend.key = element_rect(fill = "transparent", colour = "transparent"))



# ======================================= Fig 2 - interaction plot in main manuscript ======================================




rm(list = ls())

swe_ds_all_records <- read.csv("final_Salary_data_shareable.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')

swe_ds_all_records <- swe_ds_all_records %>%
  mutate(s_level = ifelse(standard_level == "DS", "DS", ifelse(standard_level == "SWE", "SWE", "Senior SWE")))

swe_ds_all_records <- swe_ds_all_records %>% filter(standard_level != "DS")


categorical_var_US <-c('company','city','gender','Race','Education','s_level')


swe_ds_all_records[categorical_var_US] <- lapply(swe_ds_all_records[categorical_var_US], as.factor)


swe_ds_all_records$gender <- relevel(swe_ds_all_records$gender, ref = "Male")
swe_ds_all_records$s_level <- relevel(swe_ds_all_records$s_level, ref = "SWE")
swe_ds_all_records$Race <- relevel(swe_ds_all_records$Race, ref = "Asian")
swe_ds_all_records$Education <- relevel(swe_ds_all_records$Education, ref = "MA or higher")
swe_ds_all_records$company <- relevel(swe_ds_all_records$company, ref = "amazon")
swe_ds_all_records$city <- relevel(swe_ds_all_records$city, ref = "Seattle")



# ============ compensation =========


lm_comp_mod<- lm(log_compensation ~ gender*s_level + Race + Education + company + years_of_experience + exp + city + year, data = swe_ds_all_records); summary(lm_comp_mod)

anova_lm_comp_mod_summary <- anova(lm_comp_mod); anova_lm_comp_mod_summary

em_comp_mod <- emmeans(lm_comp_mod, ~ gender*s_level, rg.limit = 500000)


# ------------------------------------- ggplot + Standard Error ------------------------------


emmeans_results <- as.data.frame(emmeans(lm_comp_mod, ~ gender*s_level,  by = 'gender', rg.limit = 500000)); emmeans_results


ggplot(data = emmeans_results, aes(x = s_level, y = emmean, colour = gender, group = gender, ymin = emmean-SE, ymax = emmean+SE))  + 
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = emmeans_results, show.legend = F)+
  geom_line(linetype = "dashed", data = emmeans_results, size=1.1) + 
  geom_errorbar(width = 0.05, size = 1.1) +
  guides(size = "none", colour = guide_legend(nrow = 2, override.aes = list(size=8))) + 
  scale_y_continuous(breaks = pretty_breaks(5))+
  scale_color_manual(values=c("#0078b7", "#ff1315"), labels = c('Men', 'Women')) +
  ylab("Predicted Compensation (Log Scale)") + xlab("")  +
  guides(fill = "none", alpha = "none", colour = guide_legend(override.aes = list(size=8, shape = 15))) + 
  theme(axis.title = element_text(size=12),axis.text= element_text(size=12, colour = 'black'),plot.title= element_text(hjust = .5, size = 12), 
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        legend.position = 'bottom', legend.text = element_text(size=12), legend.title = element_blank(),  
        legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.key.width = unit(0, "cm"))



# ======================================================================= SI - Table S4 ==================================================================


rm(list = ls())
set.seed(3334)

swe_ds_all_records <- read.csv("final_Salary_data_shareable.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')

swe_ds_all_records <- swe_ds_all_records %>%
  mutate(s_level = ifelse(standard_level == "DS", "DS", ifelse(standard_level == "SWE", "SWE", "Senior SWE")))



categorical_var_US <-c('company','city','gender','Race','Education','s_level')


swe_ds_all_records[categorical_var_US] <- lapply(swe_ds_all_records[categorical_var_US], as.factor)


swe_ds_all_records$gender <- relevel(swe_ds_all_records$gender, ref = "Male")
swe_ds_all_records$s_level <- relevel(swe_ds_all_records$s_level, ref = "SWE")
swe_ds_all_records$Race <- relevel(swe_ds_all_records$Race, ref = "Asian")
swe_ds_all_records$Education <- relevel(swe_ds_all_records$Education, ref = "MA or higher")
swe_ds_all_records$company <- relevel(swe_ds_all_records$company, ref = "amazon")
swe_ds_all_records$city <- relevel(swe_ds_all_records$city, ref = "Seattle")






# ------ base salary -------


lm_base_salary_SWE <- lm(log_salary ~ gender*s_level + Race + Education + company + years_of_experience + exp + city + year, data = swe_ds_all_records); summary(lm_base_salary_SWE)


# ---- compensation -----




lm_comp_mod<- lm(log_compensation ~ gender*s_level + Race + Education + company + years_of_experience + exp + city + year, data = swe_ds_all_records); summary(lm_comp_mod)


stargazer(lm_base_salary_SWE, lm_comp_mod, align=TRUE, star.cutoffs = c(0.05, 0.01, 0.001), type='text')






# ======================================================================= SI - Table S5 ==================================================================


rm(list = ls())
set.seed(3334)

swe_ds_all_records <- read.csv("final_Salary_data_shareable.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')

swe_ds_all_records <- swe_ds_all_records %>%
  mutate(s_level = ifelse(standard_level == "DS", "DS", ifelse(standard_level == "SWE", "SWE", "Senior SWE")))



categorical_var_US <-c('company','city','gender','Race','Education','s_level')


swe_ds_all_records[categorical_var_US] <- lapply(swe_ds_all_records[categorical_var_US], as.factor)


swe_ds_all_records$gender <- relevel(swe_ds_all_records$gender, ref = "Male")
swe_ds_all_records$s_level <- relevel(swe_ds_all_records$s_level, ref = "SWE")
swe_ds_all_records$Race <- relevel(swe_ds_all_records$Race, ref = "Asian")
swe_ds_all_records$Education <- relevel(swe_ds_all_records$Education, ref = "MA or higher")
swe_ds_all_records$company <- relevel(swe_ds_all_records$company, ref = "amazon")
swe_ds_all_records$city <- relevel(swe_ds_all_records$city, ref = "Seattle")



lm_comp_mod<- lm(log_compensation ~ gender*s_level + Race + Education + company + years_of_experience + exp + city + year, data = swe_ds_all_records); summary(lm_comp_mod)

anova(lm_comp_mod)





# ======================================================================= SI - Table S6 ==================================================================


rm(list = ls())
set.seed(3334)

swe_ds_all_records <- read.csv("final_Salary_data_shareable.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')

swe_ds_all_records <- swe_ds_all_records %>%
  mutate(s_level = ifelse(standard_level == "DS", "DS", ifelse(standard_level == "SWE", "SWE", "Senior SWE")))


swe_ds_all_records <- swe_ds_all_records %>% filter(standard_level != "DS")


categorical_var_US <-c('company','city','gender','Race','Education','s_level')


swe_ds_all_records[categorical_var_US] <- lapply(swe_ds_all_records[categorical_var_US], as.factor)


swe_ds_all_records$gender <- relevel(swe_ds_all_records$gender, ref = "Male")
swe_ds_all_records$s_level <- relevel(swe_ds_all_records$s_level, ref = "SWE")
swe_ds_all_records$Race <- relevel(swe_ds_all_records$Race, ref = "Asian")
swe_ds_all_records$Education <- relevel(swe_ds_all_records$Education, ref = "MA or higher")
swe_ds_all_records$company <- relevel(swe_ds_all_records$company, ref = "amazon")
swe_ds_all_records$city <- relevel(swe_ds_all_records$city, ref = "Seattle")



lm_comp_mod<- lm(log_compensation ~ gender*s_level + Race + Education + company + years_of_experience + exp + city + year, data = swe_ds_all_records); summary(lm_comp_mod)

anova(lm_comp_mod)













