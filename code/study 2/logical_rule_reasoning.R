library(dplyr)
library(readr)
library(sjmisc)
library(tidyverse)
library(scales)
library(arules)
library(repr)
library(patchwork)
library(repr)
library(arules)


# ====================================== 0.25 quantile =========================================
rm(list = ls())
review_Data <- read.csv("high_entropy_multi_q_25.csv", header = TRUE, encoding = "UTF-8")%>% dplyr::select(-'X')


# --------------- descriptive information ---------

topic_cols = c('topic_0', 'topic_1', 'topic_2', 'topic_3', 'topic_4', 'topic_5',
               'topic_6', 'topic_7', 'topic_8', 'topic_9', 'topic_10', 'topic_11',
               'topic_12', 'topic_13')

review_Data$topic_count <- rowSums(review_Data[, topic_cols] != 0)


summary(review_Data$topic_count)

review_Data %>%
  group_by(topic_count) %>%
  summarise(num = n())




# --------- association rule ---------


topic_cols = c('topic_0', 'topic_1', 'topic_2', 'topic_3', 'topic_4', 'topic_5',
               'topic_6', 'topic_7', 'topic_8', 'topic_9', 'topic_10', 'topic_11',
               'topic_12', 'topic_13')
topic_df <- review_Data[,topic_cols]
topic_df$ID <- seq(1, 5296, by=1)


df_topic_1 <- topic_df %>% dplyr::select(c('ID', 'topic_0')) %>% rename(item = topic_0)
df_topic_2 <- topic_df %>% dplyr::select(c('ID', 'topic_1')) %>% rename(item = topic_1)
df_topic_3 <- topic_df %>% dplyr::select(c('ID', 'topic_2')) %>% rename(item = topic_2)
df_topic_4 <- topic_df %>% dplyr::select(c('ID', 'topic_3')) %>% rename(item = topic_3)
df_topic_5 <- topic_df %>% dplyr::select(c('ID', 'topic_4')) %>% rename(item = topic_4)
df_topic_6 <- topic_df %>% dplyr::select(c('ID', 'topic_5')) %>% rename(item = topic_5)

df_topic_7 <- topic_df %>% dplyr::select(c('ID', 'topic_6')) %>% rename(item = topic_6)
df_topic_8 <- topic_df %>% dplyr::select(c('ID', 'topic_7')) %>% rename(item = topic_7)
df_topic_9 <- topic_df %>% dplyr::select(c('ID', 'topic_8')) %>% rename(item = topic_8)
df_topic_10 <- topic_df %>% dplyr::select(c('ID', 'topic_9')) %>% rename(item = topic_9)
df_topic_11 <- topic_df %>% dplyr::select(c('ID', 'topic_10')) %>% rename(item = topic_10)
df_topic_12 <- topic_df %>% dplyr::select(c('ID', 'topic_11')) %>% rename(item = topic_11)

df_topic_13 <- topic_df %>% dplyr::select(c('ID', 'topic_12')) %>% rename(item = topic_12)
df_topic_14 <- topic_df %>% dplyr::select(c('ID', 'topic_13')) %>% rename(item = topic_13)



transaction_df <- rbind(df_topic_1, df_topic_2, df_topic_3, df_topic_4, df_topic_5, df_topic_6,
                        df_topic_7, df_topic_8, df_topic_9, df_topic_10, df_topic_11, df_topic_12,
                        df_topic_13, df_topic_14)
complete_transaction_df <- transaction_df[-which(transaction_df$item == "0"), ]

transaction_topic <- as(split(complete_transaction_df[,"item"], complete_transaction_df[,"ID"]), "transactions")
transaction_topic
inspect(transaction_topic)




# ------ rhs == negative topics ------


# Contractor Treatment - topic_13
rule_rhs_topic_13 <- apriori(transaction_topic, parameter = list(supp=0.001, conf=0.9),
                             appearance = list(default='lhs', rhs='topic_13'))
rule_rhs_topic_13

sorted_rule_rhs_topic_13 <- sort(rule_rhs_topic_13, by='confidence')
inspect(sorted_rule_rhs_topic_13)

# Job insecurity - topic_9

rule_rhs_topic_9 <- apriori(transaction_topic, parameter = list(supp=0.001, conf=0.9),
                            appearance = list(default='lhs', rhs='topic_9'))
rule_rhs_topic_9

sorted_rule_rhs_topic_9 <- sort(rule_rhs_topic_9, by='confidence')
inspect(sorted_rule_rhs_topic_9)





# ====================================== 0.5 quantile =========================================
rm(list = ls())
review_Data <- read.csv("high_entropy_multi_q_5.csv", header = TRUE, encoding = "UTF-8")%>% dplyr::select(-'X')


# --------------- descriptive information ---------

topic_cols = c('topic_0', 'topic_1', 'topic_2', 'topic_3', 'topic_4', 'topic_5',
               'topic_6', 'topic_7', 'topic_8', 'topic_9', 'topic_10', 'topic_11',
               'topic_12', 'topic_13')

review_Data$topic_count <- rowSums(review_Data[, topic_cols] != 0)


summary(review_Data$topic_count)

review_Data %>%
  group_by(topic_count) %>%
  summarise(num = n())




# --------- association rule ---------


topic_cols = c('topic_0', 'topic_1', 'topic_2', 'topic_3', 'topic_4', 'topic_5',
               'topic_6', 'topic_7', 'topic_8', 'topic_9', 'topic_10', 'topic_11',
               'topic_12', 'topic_13')
topic_df <- review_Data[,topic_cols]
topic_df$ID <- seq(1, 5296, by=1)


df_topic_1 <- topic_df %>% dplyr::select(c('ID', 'topic_0')) %>% rename(item = topic_0)
df_topic_2 <- topic_df %>% dplyr::select(c('ID', 'topic_1')) %>% rename(item = topic_1)
df_topic_3 <- topic_df %>% dplyr::select(c('ID', 'topic_2')) %>% rename(item = topic_2)
df_topic_4 <- topic_df %>% dplyr::select(c('ID', 'topic_3')) %>% rename(item = topic_3)
df_topic_5 <- topic_df %>% dplyr::select(c('ID', 'topic_4')) %>% rename(item = topic_4)
df_topic_6 <- topic_df %>% dplyr::select(c('ID', 'topic_5')) %>% rename(item = topic_5)

df_topic_7 <- topic_df %>% dplyr::select(c('ID', 'topic_6')) %>% rename(item = topic_6)
df_topic_8 <- topic_df %>% dplyr::select(c('ID', 'topic_7')) %>% rename(item = topic_7)
df_topic_9 <- topic_df %>% dplyr::select(c('ID', 'topic_8')) %>% rename(item = topic_8)
df_topic_10 <- topic_df %>% dplyr::select(c('ID', 'topic_9')) %>% rename(item = topic_9)
df_topic_11 <- topic_df %>% dplyr::select(c('ID', 'topic_10')) %>% rename(item = topic_10)
df_topic_12 <- topic_df %>% dplyr::select(c('ID', 'topic_11')) %>% rename(item = topic_11)

df_topic_13 <- topic_df %>% dplyr::select(c('ID', 'topic_12')) %>% rename(item = topic_12)
df_topic_14 <- topic_df %>% dplyr::select(c('ID', 'topic_13')) %>% rename(item = topic_13)



transaction_df <- rbind(df_topic_1, df_topic_2, df_topic_3, df_topic_4, df_topic_5, df_topic_6,
                        df_topic_7, df_topic_8, df_topic_9, df_topic_10, df_topic_11, df_topic_12,
                        df_topic_13, df_topic_14)
complete_transaction_df <- transaction_df[-which(transaction_df$item == "0"), ]

transaction_topic <- as(split(complete_transaction_df[,"item"], complete_transaction_df[,"ID"]), "transactions")
transaction_topic
inspect(transaction_topic)




# ------ rhs == negative topics ------


# Contractor Treatment - topic_13
rule_rhs_topic_13 <- apriori(transaction_topic, parameter = list(supp=0.001, conf=0.9),
                             appearance = list(default='lhs', rhs='topic_13'))
rule_rhs_topic_13

sorted_rule_rhs_topic_13 <- sort(rule_rhs_topic_13, by='confidence')
inspect(sorted_rule_rhs_topic_13)

# Job insecurity - topic_9

rule_rhs_topic_9 <- apriori(transaction_topic, parameter = list(supp=0.001, conf=0.9),
                            appearance = list(default='lhs', rhs='topic_9'))
rule_rhs_topic_9

sorted_rule_rhs_topic_9 <- sort(rule_rhs_topic_9, by='confidence')
inspect(sorted_rule_rhs_topic_9)



