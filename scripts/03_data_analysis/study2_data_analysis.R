rm(list = ls())  

library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lmerTest)
library(lme4)
library(emmeans)
library(readr)

setwd("/Users/suelim/Documents/Research/Evaluation Vaping Messages/Evaluation_Vaping_Messages/data/02_Data/Study 2/")

df_total <- read_csv('final_data_file.csv')

#demographics
nrow(df_total)
df_total$Age <- as.numeric(df_total$Age)
mean(df_total$Age, na.rm=TRUE) 
sd(df_total$Age, na.rm=TRUE)
table(df_total$Sex) 
101/181
table(df_total$Experimental_Group)

#Create Effects Perception (EP), Negative Attitudes Towards AI, and Message Selection Variables
df_total <- df_total %>% mutate_at(c(9:118), as.numeric)

df_total$AI1_EP <- (df_total$Dis_AI1 + df_total$Con_AI1 + df_total$Unp_AI1)/3
df_total$AI2_EP <- (df_total$Dis_AI2 + df_total$Con_AI2 + df_total$Unp_AI2)/3
df_total$AI3_EP <- (df_total$Dis_AI3 + df_total$Con_AI3 + df_total$Unp_AI3)/3
df_total$AI4_EP <- (df_total$Dis_AI4 + df_total$Con_AI4 + df_total$Unp_AI4)/3
df_total$AI5_EP <- (df_total$Dis_AI5 + df_total$Con_AI5 + df_total$Unp_AI5)/3
df_total$AI6_EP <- (df_total$Dis_AI6 + df_total$Con_AI6 + df_total$Unp_AI6)/3
df_total$AI7_EP <- (df_total$Dis_AI7 + df_total$Con_AI7 + df_total$Unp_AI7)/3
df_total$AI8_EP <- (df_total$Dis_AI8 + df_total$Con_AI8 + df_total$Unp_AI8)/3
df_total$AI9_EP <- (df_total$Dis_AI9 + df_total$Con_AI9 + df_total$Unp_AI9)/3
df_total$AI10_EP <- (df_total$Dis_AI10 + df_total$Con_AI10 + df_total$Unp_AI10)/3 
df_total$AI11_EP <- (df_total$Dis_AI11 + df_total$Con_AI11 + df_total$Unp_AI11)/3
df_total$AI12_EP <- (df_total$Dis_AI12 + df_total$Con_AI12 + df_total$Unp_AI12)/3 
df_total$AI13_EP <- (df_total$Dis_AI13 + df_total$Con_AI13 + df_total$Unp_AI13)/3 
df_total$AI14_EP <- (df_total$Dis_AI14 + df_total$Con_AI14 + df_total$Unp_AI14)/3 
df_total$AI15_EP <- (df_total$Dis_AI15 + df_total$Con_AI15 + df_total$Unp_AI15)/3 

df_total$Human1_EP <- (df_total$Dis_Hum1 + df_total$Con_Hum1 + df_total$Unp_Hum1)/3
df_total$Human2_EP <- (df_total$Dis_Hum2 + df_total$Con_Hum2 + df_total$Unp_Hum2)/3
df_total$Human3_EP <- (df_total$Dis_Hum3 + df_total$Con_Hum3 + df_total$Unp_Hum3)/3
df_total$Human4_EP <- (df_total$Dis_Hum4 + df_total$Con_Hum4 + df_total$Unp_Hum4)/3 
df_total$Human5_EP <- (df_total$Dis_Hum5 + df_total$Con_Hum5 + df_total$Unp_Hum5)/3 
df_total$Human6_EP <- (df_total$Dis_Hum6 + df_total$Con_Hum6 + df_total$Unp_Hum6)/3 
df_total$Human7_EP <- (df_total$Dis_Hum7 + df_total$Con_Hum7 + df_total$Unp_Hum7)/3 
df_total$Human8_EP <- (df_total$Dis_Hum8 + df_total$Con_Hum8 + df_total$Unp_Hum8)/3
df_total$Human9_EP <- (df_total$Dis_Hum9 + df_total$Con_Hum9 + df_total$Unp_Hum9)/3
df_total$Human10_EP <- (df_total$Dis_Hum10 + df_total$Con_Hum10 + df_total$Unp_Hum10)/3 
df_total$Human11_EP <- (df_total$Dis_Hum11 + df_total$Con_Hum11 + df_total$Unp_Hum11)/3
df_total$Human12_EP <- (df_total$Dis_Hum12 + df_total$Con_Hum12 + df_total$Unp_Hum12)/3 
df_total$Human13_EP <- (df_total$Dis_Hum13 + df_total$Con_Hum13 + df_total$Unp_Hum13)/3 
df_total$Human14_EP <- (df_total$Dis_Hum14 + df_total$Con_Hum14 + df_total$Unp_Hum14)/3 
df_total$Human15_EP <- (df_total$Dis_Hum15 + df_total$Con_Hum15 + df_total$Unp_Hum15)/3 

df_total$AIper_neg <- (df_total$Per_AI3 + df_total$Per_AI6 + df_total$Per_AI8 + 
                         df_total$Per_AI9 + df_total$Per_AI10 + df_total$Per_AI15 + df_total$Per_AI19 +
                         df_total$Per_AI20)/8

AI_Message_Count <- 
  df_total[, c("Message1", "Message2", "Message3", "Message4", "Message5")] <= 15
df_total$AI_Message_Count <- rowSums(AI_Message_Count)
df_total <- df_total |> relocate(AI_Message_Count, .after = Message5)

#linear mixed effects model of EP scores
df_total_3EPs <- df_total[c(1:2, 127:157)]
df_total_3EPs_pivot <- df_total_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant, AIper_neg), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))

df_total$Participant <- factor(df_total$Participant)
df_total_3EPs_pivot$Experimental_Group <- factor(df_total_3EPs_pivot$Experimental_Group)
df_total_3EPs_pivot$AI_or_Hum <- factor(df_total_3EPs_pivot$AI_or_Hum)
df_total_3EPs_pivot$Experimental_Group <- relevel(df_total_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")
df_total_3EPs_pivot$AI_or_Hum <- relevel(df_total_3EPs_pivot$AI_or_Hum, ref = "AI")

total_EP3_ANOVA <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum * AIper_neg + (1 | Participant), data = df_total_3EPs_pivot)
summary(total_EP3_ANOVA)

##calculate mean and +- 1 SD for negative attitudes towards AI
mean(df_total$AIper_neg)
sd(df_total$AIper_neg)
mean(df_total$AIper_neg) - sd(df_total$AIper_neg)
mean(df_total$AIper_neg) + sd(df_total$AIper_neg)

##calculate pairwise comparison and estimated marginal means for data visualization
predict_mean <- 
  emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AIper_neg = c(1, 2, 3, 4, 5)))
predict_mean_df <- as.data.frame(predict_mean)

ggplot(predict_mean_df, aes(x = AIper_neg, y = emmean, color = AI_or_Hum)) +
  geom_line(size = 1.25) + 
  facet_wrap(~Experimental_Group) +
  labs(x = "Mean Negative Attitudes Towards AI", y = "Effects Perception", color = "Message Source") +
  theme_bw() + 
  scale_colour_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) +
  ylim(1,5)

pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.85)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.85)))

#Poisson regression of number of AI-generated messages selected
colnames(df_total)
df_total_MS <- df_total[c(1:2, 4:9, 157)]
df_total_MS$Experimental_Group <- factor(df_total_MS$Experimental_Group)
df_total_MS$Experimental_Group <- relevel(df_total_MS$Experimental_Group, ref = "Not Disclosed")

message_preference <-
  glm(AI_Message_Count ~ Experimental_Group * AIper_neg, data = df_total_MS, family = poisson(link = "log"))
summary(message_preference)

predict_count <- 
  emmeans(message_preference, ~ Experimental_Group * AIper_neg, at = list(AIper_neg = c(1, 2, 3, 4, 5)))
predict_count_regrid <- regrid(predict_count, transform = "response")
predict_count_df <- as.data.frame(predict_count_regrid)
ggplot(predict_count_df, aes(x = AIper_neg, y = rate)) +
  geom_line(size = 1.25) + 
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2)+
  facet_wrap(~Experimental_Group) +
  labs(x = "Mean Negative Attitudes Towards AI", y = "Predicted # of AI-Generated Messages Selected", color = "Message Source") +
  theme_bw() + 
  ylim(0,5)

results_list <- list()
for(ai_val in c(2.24, 3.04, 3.85)) {
  emm_current <- emmeans(message_preference, ~ Experimental_Group * AIper_neg, at = list(AIper_neg = ai_val), regrid = "response")
  comp_current <- pairs(emm_current, regrid = "response")
  results_list[[paste(ai_val, sep = "_")]] <- comp_current
}

for(name in names(results_list)) {
  cat("\nResults for", name, ":\n")
  print(results_list[[name]])
}
