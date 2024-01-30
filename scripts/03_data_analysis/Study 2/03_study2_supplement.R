library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lmerTest)
library(lme4)
library(emmeans)
library(readr)

#set directory and load file
setwd("/Users/suelim/Documents/Research/DONE/Evaluation_Vaping_Messages/github/Evaluation_Vaping_Messages/data/02_Data/study2")
df_total <- read_csv('02_cleaned_data_file/study2_data_file.csv')

#demographics
df_total_prolific <- 
  df_total  %>% filter(df_total$recruitment_platform == "Prolific")
table(df_total_prolific$Experimental_Group)
mean(df_total_prolific$Age, na.rm=TRUE)
sd(df_total_prolific$Age, na.rm=TRUE)
table(df_total_prolific$Sex) / length(df_total_prolific$Sex)

df_total_sona <- 
  df_total  %>% filter(df_total$recruitment_platform == "SONA")
table(df_total_sona$Experimental_Group)
mean(df_total_sona$Age, na.rm=TRUE)
sd(df_total_sona$Age, na.rm=TRUE)
table(df_total_sona$Sex) / length(df_total_sona$Sex)

#creating Effects Perception (EP) and Perception of AI variables
sum_AI_EP <- 0
sum_Human_EP <- 0
for (i in 1:15) {
  df_total[[paste0("AI", i, "_EP")]] <- 
    (df_total[[paste0("Dis_AI", i)]] + df_total[[paste0("Con_AI", i)]] + 
       df_total[[paste0("Unp_AI", i)]]) / 3
  sum_AI_EP <- sum_AI_EP + df_total[[paste0("AI", i, "_EP")]]
}
df_total$AI_mean_EP <- sum_AI_EP / 15
for (i in 1:15) {
  df_total[[paste0("Human", i, "_EP")]] <- 
    (df_total[[paste0("Dis_Hum", i)]] + df_total[[paste0("Con_Hum", i)]] + 
       df_total[[paste0("Unp_Hum", i)]]) / 3
  sum_Human_EP <- sum_Human_EP + df_total[[paste0("Human", i, "_EP")]]
}
df_total$Human_mean_EP <- sum_Human_EP / 15

df_total$AIper_neg <- 
  (df_total$Per_AI3_1 + df_total$Per_AI6_1 + df_total$Per_AI8_1 + df_total$Per_AI9_1 + 
     df_total$Per_AI10_1 + df_total$Per_AI15_1 + df_total$Per_AI19_1 + df_total$Per_AI20_1)/8

AI_Message_Count <- 
  df_total[, c("Message1", "Message2", "Message3", "Message4", "Message5")] <= 15
df_total$AI_Message_Count <- rowSums(AI_Message_Count)
df_total <- df_total |> relocate(AI_Message_Count, .after = Message5)
colnames(df_total)

#demographics
df_total_prolific <- 
  df_total  %>% filter(df_total$recruitment_platform == "Prolific")
table(df_total_prolific$Experimental_Group)
mean(df_total_prolific$Age, na.rm=TRUE)
sd(df_total_prolific$Age, na.rm=TRUE)
table(df_total_prolific$Sex) / length(df_total_prolific$Sex)
mean(df_total_prolific$AIper_neg, na.rm=TRUE)
sd(df_total_prolific$AIper_neg, na.rm=TRUE)

df_total_sona <- 
  df_total  %>% filter(df_total$recruitment_platform == "SONA")
table(df_total_sona$Experimental_Group)
mean(df_total_sona$Age, na.rm=TRUE)
sd(df_total_sona$Age, na.rm=TRUE)
table(df_total_sona$Sex) / length(df_total_sona$Sex)
mean(df_total_sona$AIper_neg, na.rm=TRUE)
sd(df_total_sona$AIper_neg, na.rm=TRUE)

#cronbach alpha
alpha_values_prolific <- numeric(30)
for (i in 1:15) {
  # Create a subset for each message (AI and Human)
  subset_ai_pro <- df_total_prolific[, c(paste("Dis_AI", i, sep=""), paste("Con_AI", i, sep=""), paste("Unp_AI", i, sep=""))]
  subset_hum_pro <- df_total_prolific[, c(paste("Dis_Hum", i, sep=""), paste("Con_Hum", i, sep=""), paste("Unp_Hum", i, sep=""))]
  # Calculate and store Cronbach's alpha for each message
  alpha_values_prolific[i] <- alpha(subset_ai_pro)$total$raw_alpha
  alpha_values_prolific[i + 15] <- alpha(subset_hum_pro)$total$raw_alpha
}
print(alpha_values_prolific)
mean(alpha_values_prolific)

alpha_values_sona <- numeric(30)
for (i in 1:15) {
  # Create a subset for each message (AI and Human)
  subset_ai_sona <- df_total_sona[, c(paste("Dis_AI", i, sep=""), paste("Con_AI", i, sep=""), paste("Unp_AI", i, sep=""))]
  subset_hum_sona <- df_total_sona[, c(paste("Dis_Hum", i, sep=""), paste("Con_Hum", i, sep=""), paste("Unp_Hum", i, sep=""))]
  # Calculate and store Cronbach's alpha for each message
  alpha_values_sona[i] <- alpha(subset_ai_sona)$total$raw_alpha
  alpha_values_sona[i + 15] <- alpha(subset_hum_sona)$total$raw_alpha
}
print(alpha_values_sona)
mean(alpha_values_sona)

df_new_prolific <- data.frame(
  Per_AI3_1 = df_total_prolific$Per_AI3_1,
  Per_AI6_1 = df_total_prolific$Per_AI6_1,
  Per_AI8_1 = df_total_prolific$Per_AI8_1,
  Per_AI9_1 = df_total_prolific$Per_AI9_1,
  Per_AI10_1 = df_total_prolific$Per_AI10_1,
  Per_AI15_1 = df_total_prolific$Per_AI15_1,
  Per_AI19_1 = df_total_prolific$Per_AI19_1,
  Per_AI20_1 = df_total_prolific$Per_AI20_1
)

df_new_sona <- data.frame(
  Per_AI3_1 = df_total_sona$Per_AI3_1,
  Per_AI6_1 = df_total_sona$Per_AI6_1,
  Per_AI8_1 = df_total_sona$Per_AI8_1,
  Per_AI9_1 = df_total_sona$Per_AI9_1,
  Per_AI10_1 = df_total_sona$Per_AI10_1,
  Per_AI15_1 = df_total_sona$Per_AI15_1,
  Per_AI19_1 = df_total_sona$Per_AI19_1,
  Per_AI20_1 = df_total_sona$Per_AI20_1
)
alpha_prolific <- alpha(df_new_prolific)$total$raw_alpha
alpha_sona <- alpha(df_new_sona)$total$raw_alpha


# Preparing data structure for mixed-effects modeling
df_total_3EPs <- df_total[c(1:2, 122:137, 139:153, 155)]
df_total_3EPs_pivot <- df_total_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant, AIper_neg, recruitment_platform), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))
df_total_3EPs_pivot$Participant <- factor(df_total_3EPs_pivot$Participant)
df_total_3EPs_pivot$Experimental_Group <- factor(df_total_3EPs_pivot$Experimental_Group)
df_total_3EPs_pivot$AI_or_Hum <- factor(df_total_3EPs_pivot$AI_or_Hum)
df_total_3EPs_pivot$Experimental_Group <- relevel(df_total_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")
df_total_3EPs_pivot$AI_or_Hum <- relevel(df_total_3EPs_pivot$AI_or_Hum, ref = "AI")
df_total_3EPs_pivot$Messages <- factor(df_total_3EPs_pivot$Messages)

total_EP3_ANOVA <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum*AIper_neg + (1 | Participant) + (1 | Messages), data = df_total_3EPs_pivot)
summary(total_EP3_ANOVA)

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

#Prolific sample
df_total_3EPs_pivot_prolific <- 
  df_total_3EPs_pivot |> 
  filter(df_total_3EPs_pivot$recruitment_platform == "Prolific")

##means and standard deviations of EP and ranking
df_total_3EPs_pivot_prolific %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")

df_total_3EPs_pivot_prolific %>%
  get_summary_stats(Effects_Perception, type = "full")

df_total_3EPs_pivot_prolific %>%
  get_summary_stats(AIper_neg, type = "full")

total_EP3_ANOVA_prolific <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum*AIper_neg + (1 | Participant) + (1 | Messages), data = df_total_3EPs_pivot_prolific)
summary(total_EP3_ANOVA_prolific)

##calculate pairwise comparison and estimated marginal means for data visualization
predict_mean_prolific <- 
  emmeans(total_EP3_ANOVA_prolific, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AIper_neg = c(1, 2, 3, 4, 5)))
predict_mean_prolific_df <- as.data.frame(predict_mean_prolific)
ggplot(predict_mean_prolific_df, aes(x = AIper_neg, y = emmean, color = AI_or_Hum)) +
  geom_line(size = 1.25) + 
  facet_wrap(~Experimental_Group) +
  labs(x = "Mean Negative Attitudes Towards AI", y = "Effects Perception", color = "Message Source") +
  theme_bw() + 
  scale_colour_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) +
  ylim(1,5)

##pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 2.24)))
##pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.04)))
##pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.84)))
##pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 2.24)))
##pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.04)))
##pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.84)))

#SONA sample
df_total_3EPs_pivot_sona <- 
  df_total_3EPs_pivot |> 
  filter(df_total_3EPs_pivot$recruitment_platform == "SONA")

##means and standard deviations of EP and ranking
df_total_3EPs_pivot_sona %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")

df_total_3EPs_pivot_sona %>%
  get_summary_stats(Effects_Perception, type = "full")

df_total_3EPs_pivot_sona %>%
  get_summary_stats(AIper_neg, type = "full")

total_EP3_ANOVA_sona <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum*AIper_neg + (1 | Participant) + (1 | Messages), data = df_total_3EPs_pivot_sona)
summary(total_EP3_ANOVA_sona)
predict_mean_sona <- 
  emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AIper_neg = c(1, 2, 3, 4, 5)))
predict_mean_sona_df <- as.data.frame(predict_mean_sona)
ggplot(predict_mean_sona_df, aes(x = AIper_neg, y = emmean, color = AI_or_Hum)) +
  geom_line(size = 1.25) + 
  facet_wrap(~Experimental_Group) +
  labs(x = "Mean Negative Attitudes Towards AI", y = "Effects Perception", color = "Message Source") +
  theme_bw() + 
  scale_colour_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) +
  ylim(1,5)

##calculate mean and +- 1 SD for negative attitudes towards AI
df_total_sona <- 
  df_total_3EPs_pivot |> 
  filter(df_total_3EPs_pivot$recruitment_platform == "SONA")
mean(df_total_sona$AIper_neg)
sd(df_total_sona$AIper_neg)
mean(df_total_sona$AIper_neg) - sd(df_total_sona$AIper_neg)
mean(df_total_sona$AIper_neg) + sd(df_total_sona$AIper_neg)

emmeans(total_EP3_ANOVA_sona, pairwise~Experimental_Group*AI_or_Hum*AIper_neg)

pairs(emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 2.51)))
pairs(emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.22)))
pairs(emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.93)))
pairs(emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 2.51)))
pairs(emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.22)))
pairs(emmeans(total_EP3_ANOVA_sona, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.93)))



#Poisson regression of number of AI-generated messages selected
colnames(df_total)
df_total_MS <- df_total[c(1:2, 11, 122, 155)]
df_total_MS$Experimental_Group <- factor(df_total_MS$Experimental_Group)
df_total_MS$Experimental_Group <- relevel(df_total_MS$Experimental_Group, ref = "Not Disclosed")

df_total_MS_prolific <- 
  df_total_MS |> 
  filter(df_total_MS$recruitment_platform == "Prolific")
message_preference_prolific <-
  glm(AI_Message_Count ~ Experimental_Group * AIper_neg, data = df_total_MS_prolific, family = poisson(link = "log"))
summary(message_preference_prolific)

df_total_MS_sona <- 
  df_total_MS |> 
  filter(df_total_MS$recruitment_platform == "SONA")
message_preference_sona <-
  glm(AI_Message_Count ~ Experimental_Group * AIper_neg, data = df_total_MS_sona, family = poisson(link = "log"))
summary(message_preference_sona)

predict_count <- 
  emmeans(message_preference, ~ Experimental_Group * AIper_neg, at = list(AIper_neg = c(1, 2, 3, 4, 5)))
predict_count_regrid <- regrid(predict_count, transform = "response")
predict_count_df <- as.data.frame(predict_count_regrid)
ggplot(predict_count_df, aes(x = AIper_neg, y = rate)) +
  geom_line(size = 1.25) + 
  geom_ribbon(aes(ymin = asymp.LCL, ymax = asymp.UCL), alpha = 0.2)+
  facet_wrap(~Experimental_Group) +
  labs(x = "Mean Negative Attitudes Towards AI", y = "Predicted # of AI-Generated Messages Selected", color = "Message Source") +
  theme_bw()

results_list <- list()
for(ai_val in c(2.24, 3.04, 3.84)) {
  emm_current <- emmeans(message_preference, ~ Experimental_Group * AIper_neg, at = list(AIper_neg = ai_val), regrid = "response")
  comp_current <- pairs(emm_current, regrid = "response")
  results_list[[paste(ai_val, sep = "_")]] <- comp_current
}

for(name in names(results_list)) {
  cat("\nResults for", name, ":\n")
  print(results_list[[name]])
}
