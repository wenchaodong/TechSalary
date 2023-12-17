library(dplyr)
library(readr)
library(ggplot2)
library(sjmisc)
library(tidyverse)
library(scales)
library(ggthemes)
library(repr)
library(mediation)
library(plotrix) 
library(psych)
library(emmeans)
library(margins)
library(ggcorrplot)
library(reshape)


# ------------------------------ Moderation Analysis ------------------------------------



rm(list = ls())
survey_Data <- read.csv("selected_survey_dataframe.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')

categorical_var <-c('gender', "level", "standard_edu")
survey_Data[categorical_var] <- lapply(survey_Data[categorical_var], as.factor)
survey_Data$gender <- relevel(survey_Data$gender, ref = "Male")



mod_compensation_wlb_balance_2_lm <- lm(logged_compensation ~ Work.life.Balance_2+ gender +  Work.life.Balance_2*gender, data = survey_Data)
summary(mod_compensation_wlb_balance_2_lm)

anova_summary <- anova(mod_compensation_wlb_balance_2_lm); anova_summary

(wlb_list <- list(Work.life.Balance_2=c(-2, -1, 1, 2),gender=c("Male","Female")))


sim_slopes(model = mod_compensation_wlb_balance_2_lm, 
           pred = Work.life.Balance_2, modx = gender)

emmeans_results <- as.data.frame(emmeans(mod_compensation_wlb_balance_2_lm, ~ Work.life.Balance_2, at=wlb_list, by = 'gender')); emmeans_results

# plot by ggplot with standard error
ggplot(data = emmeans_results, aes(x = Work.life.Balance_2, y = emmean, colour = gender, group = gender, ymin = emmean-SE, ymax = emmean+SE))  + 
  geom_point(shape = 19,  fill = "white", size = 1, stroke = 3, data = emmeans_results)+
  geom_line(linetype = "dashed", data = emmeans_results, size=1.1) + 
  geom_errorbar(width = 0.08, size = 1.1) +
  guides(size = "none", colour = guide_legend(nrow = 2, override.aes = list(size=8))) + 
  scale_y_continuous(breaks = pretty_breaks(5))+
  scale_color_manual(values=c("#0078b7", "#ff1315"), labels = c('Men', 'Women')) +
  ylab("Predicted Compensation (Log Scale)") + xlab("Perceived Difficulty of Work-life Balance")  +
  guides(fill = "none", alpha = "none", colour = guide_legend(override.aes = list(size=8, shape = 15))) + 
  theme(axis.title = element_text(size=12),axis.text= element_text(size=12, colour = 'black'),plot.title= element_text(hjust = .5, size = 12), 
        panel.border = element_blank(),panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"), panel.background = element_blank(), 
        legend.position = 'bottom', legend.text = element_text(size=12), legend.title = element_blank(),  
        legend.key = element_rect(fill = "transparent", colour = "transparent"), legend.key.width = unit(0, "cm"))








# ---------------------------------- Factor Analysis + statistical analysis -----------------------------------------


rm(list = ls())
male_female_survey_df <- read.csv("selected_survey_dataframe.csv", header = TRUE, encoding = "UTF-8") %>% dplyr::select(-'X')
question_cols <- c("Work.life.Balance_1","Work.life.Balance_2", "Heavy.Workload_1","Heavy.Workload_2" ,"Management_1","Management_2" ,
                   "quit_1","women.senior_1","women.senior_2", "women.senior_3","women.senior_4","role.model_1","market.value_1",
                   "Salary.Negotiation_1","Salary.Negotiation_2","Salary.Negotiation_3")

numerical_data <- male_female_survey_df[question_cols]


male_female_survey_df <- male_female_survey_df %>%
  mutate(standard_level = ifelse(level == "Junior", "Junior", "Senior or Manager"))



categorical_var <-c('gender', "level", 'standard_level')
male_female_survey_df[categorical_var] <- lapply(male_female_survey_df[categorical_var], as.factor)

male_female_survey_df$gender <- relevel(male_female_survey_df$gender, ref = "Male")
male_female_survey_df$level <- relevel(male_female_survey_df$level, ref = "Junior")
male_female_survey_df$standard_level <- relevel(male_female_survey_df$standard_level, ref = "Senior or Manager")

male_female_survey_df$level <- ordered(male_female_survey_df$level, levels=c('Junior', 'Senior', 'Manager or higher'))



scree(numerical_data, factors = FALSE, pc = TRUE)


pca <- psych::pca(numerical_data, rotate="varimax", nfactors=5); pca
pca$values

stargazer::stargazer(as.data.frame.matrix(pca$loadings), summary = FALSE, type = 'text')

psych::alpha(numerical_data[, c("Work.life.Balance_1", "Work.life.Balance_2", "Heavy.Workload_1", "Management_2", "role.model_1")], check.keys = TRUE)
psych::alpha(numerical_data[, c("Management_1", "quit_1", "Salary.Negotiation_1")], check.keys = TRUE)
psych::alpha(numerical_data[, c("Heavy.Workload_2", "women.senior_1", "women.senior_3")], check.keys = TRUE)
psych::alpha(numerical_data[, c("market.value_1", "Salary.Negotiation_2", "Salary.Negotiation_3")], check.keys = TRUE)
psych::alpha(numerical_data[, c("women.senior_2", "women.senior_4")], check.keys = TRUE)



male_female_survey_df <- male_female_survey_df %>%
  mutate(RC2 = (Work.life.Balance_1 + Work.life.Balance_2 + Heavy.Workload_1 + Management_2 + role.model_1)/5) %>%
  mutate(RC1 = (Management_1 + quit_1 + Salary.Negotiation_1)/3) %>%
  mutate(RC3 = (Heavy.Workload_2 + women.senior_1 + women.senior_3)/3) %>%
  mutate(RC4 = (market.value_1 + Salary.Negotiation_2 + Salary.Negotiation_3)/3) %>%
  mutate(RC5 = (women.senior_2 + women.senior_4)/2)

pca.cols <- c("RC1", "RC2", "RC3" , "RC4", "RC5")

ggcorrplot::ggcorrplot(cor(male_female_survey_df[, pca.cols]),
                       outline.col = "white",
                       ggtheme = ggplot2::theme_bw,
                       #colors = c("#E46726", "white", "#6D9EC1"),
                       lab = TRUE)




melted.df <- melt(male_female_survey_df, measure.vars = pca.cols)

melted.df$comp <- factor(melted.df$variable, 
                         levels = c("RC1", "RC2", "RC3" , "RC4", "RC5"),
                         labels = c("Workplace\nSatisfaction\n(RC1)", "Work-life\nBalance\n(RC2)", "Competitive\nCulture\n(RC3)" , "Salary\nNegotiation\n(RC4)", "Men\nSeniors\n(RC5)"))

plt.colors <- c("#ff1f5b", "#009ade", "#af59ba", "#ffc61e", "#f28522")

ggplot(melted.df, aes(x=comp, y=value, fill=variable, color=variable)) +
  geom_boxplot(width=0.5, fill="#e8e8e8",
               fatten=4) +
  geom_jitter(width=0.05, alpha=0.2) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face="plain",size=15,color="black")) +
  theme(plot.margin = margin(0, 1, 0, 1)) +
  theme(axis.title.y = element_text(face="plain",size=15,color="black"),
        axis.text.y = element_text(face="plain",size=15,color="black"),
        legend.position = "none") +
  geom_hline(yintercept = 0) +
  theme(
    legend.position = "none"
  ) +
  scale_color_manual(
    values = plt.colors
  ) +
  ylab("Workplace Perception")



# ------ statistical analysis ------ 

# --- by gender ---

# Rc 4  ***  Salary Negotiation ***  sig
var.test(RC4~gender, data = male_female_survey_df)$p.value
RC4_t_test <- t.test(RC4~gender, data = male_female_survey_df, alternative = "two.sided", var.equal = TRUE); RC4_t_test

male_female_survey_df %>%
  group_by(gender) %>%
  summarise(sd_RC4 = sd(RC4))




# Rc 5 *** Men Seniors *** sig
var.test(RC5~gender, data = male_female_survey_df)$p.value
RC5_t_test <- t.test(RC5~gender, data = male_female_survey_df, alternative = "two.sided", var.equal = FALSE); RC5_t_test

male_female_survey_df %>%
  group_by(gender) %>%
  summarise(sd_RC5 = sd(RC5))



# women senior 2 *** Working with senior-level women *** sig
var.test(women.senior_2~gender, data = male_female_survey_df)$p.value
women.senior_2_t_test <- t.test(women.senior_2~gender, data = male_female_survey_df, alternative = "two.sided", var.equal = FALSE); women.senior_2_t_test

male_female_survey_df %>%
  group_by(gender) %>%
  summarise(sd_women.senior_2 = sd(women.senior_2))




# women senior 4 *** Working with senior-level men *** sig
var.test(women.senior_4~gender, data = male_female_survey_df)$p.value
women.senior_4_t_test <- t.test(women.senior_4~gender, data = male_female_survey_df, alternative = "two.sided", var.equal = TRUE); women.senior_4_t_test

male_female_survey_df %>%
  group_by(gender) %>%
  summarise(sd_women.senior_4 = sd(women.senior_4))





#  salary negotiation - 2 *** Salary negotiation power *** sig
var.test(Salary.Negotiation_2~gender, data = male_female_survey_df)$p.value
Salary.Negotiation_2_t_test <- t.test(Salary.Negotiation_2~gender, data = male_female_survey_df, alternative = "two.sided", var.equal = TRUE); Salary.Negotiation_2_t_test

male_female_survey_df %>%
  group_by(gender) %>%
  summarise(sd_Salary.Negotiation_2 = sd(Salary.Negotiation_2))



#  salary negotiation - 3 *** Willingness to negotiate *** sig
var.test(Salary.Negotiation_3~gender, data = male_female_survey_df)$p.value
Salary.Negotiation_3_t_test <- t.test(Salary.Negotiation_3~gender, data = male_female_survey_df, alternative = "two.sided", var.equal = TRUE); Salary.Negotiation_3_t_test

male_female_survey_df %>%
  group_by(gender) %>%
  summarise(sd_Salary.Negotiation_3 = sd(Salary.Negotiation_3))


# --- by level ---


#  heavy workload - 2 *** Competitive workplace *** sig
var.test(Heavy.Workload_2~standard_level, data = male_female_survey_df)$p.value
Heavy.Workload_2_t_test <- t.test(Heavy.Workload_2~standard_level, data = male_female_survey_df, alternative = "two.sided", var.equal = TRUE); Heavy.Workload_2_t_test

male_female_survey_df %>%
  group_by(standard_level) %>%
  summarise(sd_num_level = sd(Heavy.Workload_2))


#  salary negotiation - 3 *** Willingness to negotiate *** sig
var.test(Salary.Negotiation_3~standard_level, data = male_female_survey_df)$p.value
Salary.Negotiation_3_t_test <- t.test(Salary.Negotiation_3~standard_level, data = male_female_survey_df, alternative = "two.sided", var.equal = TRUE); Salary.Negotiation_3_t_test

male_female_survey_df %>%
  group_by(standard_level) %>%
  summarise(sd_Salary.Negotiation_3 = sd(Salary.Negotiation_3))






