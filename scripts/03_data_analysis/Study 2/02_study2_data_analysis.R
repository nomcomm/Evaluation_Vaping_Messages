library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lmerTest)
library(lme4)
library(emmeans)
library(readr)

#set directory and load file
setwd("[Insert path to file]")
df_total <- read_csv('study2_data_file.csv')

#demographics
table(df_total$Experimental_Group)
mean(df_total$Age, na.rm=TRUE)
sd(df_total$Age, na.rm=TRUE)
table(df_total$Sex) / length(df_total$Sex)

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

# Mixed ANOVA
df_total_3EPs <- df_total[c(1:2, 122:136, 138:152, 154)]
df_total_3EPs_pivot <- df_total_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant, AIper_neg), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))

df_total_3EPs_pivot$Participant <- factor(df_total_3EPs_pivot$Participant)
df_total_3EPs_pivot$Experimental_Group <- factor(df_total_3EPs_pivot$Experimental_Group)
df_total_3EPs_pivot$AI_or_Hum <- factor(df_total_3EPs_pivot$AI_or_Hum)
df_total_3EPs_pivot$Experimental_Group <- relevel(df_total_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")
df_total_3EPs_pivot$AI_or_Hum <- relevel(df_total_3EPs_pivot$AI_or_Hum, ref = "AI")

total_EP3_ANOVA <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum * AIper_neg + (1 | Participant), data = df_total_3EPs_pivot)
summary(total_EP3_ANOVA)

##Data visualization
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
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.84)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.84)))

#Poisson regression of number of AI-generated messages selected
colnames(df_total)
df_total_MS <- df_total[c(1:2, 11, 154)]
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
for(ai_val in c(2.24, 3.04, 3.84)) {
  emm_current <- emmeans(message_preference, ~ Experimental_Group * AIper_neg, at = list(AIper_neg = ai_val), regrid = "response")
  comp_current <- pairs(emm_current, regrid = "response")
  results_list[[paste(ai_val, sep = "_")]] <- comp_current
}

for(name in names(results_list)) {
  cat("\nResults for", name, ":\n")
  print(results_list[[name]])
}
