library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(lmerTest)
library(lme4)
library(emmeans)
library(readr)
library(performance)
library(psych)
library(car)

#set directory and load file
setwd("/Users/suelim/Documents/Research/DONE/Evaluation_Vaping_Messages/github/Evaluation_Vaping_Messages/data/02_Data/study2")
df_total <- read_csv('02_cleaned_data_file/study2_data_file.csv')

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

mean(df_total$AIper_neg, na.rm=TRUE)
sd(df_total$AIper_neg, na.rm=TRUE)

AI_Message_Count <- 
  df_total[, c("Message1", "Message2", "Message3", "Message4", "Message5")] <= 15
df_total$AI_Message_Count <- rowSums(AI_Message_Count)
df_total <- df_total |> relocate(AI_Message_Count, .after = Message5)

#cronbach alpha for EP measure
alpha_values <- numeric(30)
for (i in 1:15) {
  # Create a subset for each message (AI and Human)
  subset_ai <- df_total[, c(paste("Dis_AI", i, sep=""), paste("Con_AI", i, sep=""), paste("Unp_AI", i, sep=""))]
  subset_hum <- df_total[, c(paste("Dis_Hum", i, sep=""), paste("Con_Hum", i, sep=""), paste("Unp_Hum", i, sep=""))]
  # Calculate and store Cronbach's alpha for each message
  alpha_values[i] <- alpha(subset_ai)$total$raw_alpha
  alpha_values[i + 15] <- alpha(subset_hum)$total$raw_alpha}
print(alpha_values)
mean(alpha_values)

#cronbach alpha for negative attitudes towards AI
df_new <- data.frame(
  Per_AI3_1 = df_total$Per_AI3_1,
  Per_AI6_1 = df_total$Per_AI6_1,
  Per_AI8_1 = df_total$Per_AI8_1,
  Per_AI9_1 = df_total$Per_AI9_1,
  Per_AI10_1 = df_total$Per_AI10_1,
  Per_AI15_1 = df_total$Per_AI15_1,
  Per_AI19_1 = df_total$Per_AI19_1,
  Per_AI20_1 = df_total$Per_AI20_1
)
alpha <- alpha(df_new)$total$raw_alpha

colnames(df_total)
# Restructing data structure
df_total_3EPs <- df_total[c(1:2, 123:137, 139:153, 155)]
df_total_3EPs_pivot <- df_total_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant, AIper_neg), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))

df_total_3EPs_pivot$Participant <- as.factor(df_total_3EPs_pivot$Participant)
df_total_3EPs_pivot$Experimental_Group <- factor(df_total_3EPs_pivot$Experimental_Group)
df_total_3EPs_pivot$AI_or_Hum <- as.factor(df_total_3EPs_pivot$AI_or_Hum)
df_total_3EPs_pivot$Experimental_Group <- relevel(df_total_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")
df_total_3EPs_pivot$AI_or_Hum <- relevel(df_total_3EPs_pivot$AI_or_Hum, ref = "AI")
df_total_3EPs_pivot$Messages <- factor(df_total_3EPs_pivot$Messages)

df_total_3EPs_pivot %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")


#mixed effects modeling
total_EP3_ANOVA <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum * AIper_neg + (1 + AI_or_Hum | Participant), data = df_total_3EPs_pivot)
summary(total_EP3_ANOVA)
performance(total_EP3_ANOVA)

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
  labs(
    x = "Mean Negative Attitudes Towards AI",
    y = "Effects Perception",
    color = "Message Source"
  ) +
  theme_classic() +
  scale_colour_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) +
  ylim(3, 5)

predict_mean2 <- 
  emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum)
predict_mean2_df <- as.data.frame(predict_mean2)

emmeans(total_EP3_ANOVA, pairwise~ Experimental_Group * AI_or_Hum)

ggplot(predict_mean2_df, aes(x = Experimental_Group, y = emmean)) +
  geom_line(aes(group = AI_or_Hum), position = position_dodge(0.3), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.3), size=3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.3)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    y = "Effects Perception",
    x = NULL,
    color = "Message Source"  # This line changes the legend title
  ) +
  theme_classic() +
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) +
  ylim(3, 5)

pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AIper_neg = 3.84)))

pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "AI", AIper_neg = 3.84)))

pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(AI_or_Hum = "Human", AIper_neg = 3.84)))

pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(Experimental_Group = "Not Disclosed", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(Experimental_Group = "Not Disclosed", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(Experimental_Group = "Not Disclosed", AIper_neg = 3.84)))

pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(Experimental_Group = "Disclosed", AIper_neg = 2.24)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(Experimental_Group = "Disclosed", AIper_neg = 3.04)))
pairs(emmeans(total_EP3_ANOVA, ~ Experimental_Group * AI_or_Hum * AIper_neg, at = list(Experimental_Group = "Disclosed", AIper_neg = 3.84)))

#Poisson regression of number of AI-generated messages selected
colnames(df_total)
df_total_MS <- df_total[c(1:2, 11, 155)]
df_total_MS$Experimental_Group <- as.factor(df_total_MS$Experimental_Group)
df_total_MS$Experimental_Group <- relevel(df_total_MS$Experimental_Group, ref = "Not Disclosed")

message_preference <-
  glm(AI_Message_Count ~ Experimental_Group * AIper_neg, data = df_total_MS, family = poisson(link = "log"))
summary(message_preference)
performance(message_preference)

predict_count <- 
  emmeans(message_preference, ~ Experimental_Group)
predict_count_regrid <- regrid(predict_count, transform = "response")
predict_count_df <- as.data.frame(predict_count_regrid)

ggplot(predict_count_df, aes(x = Experimental_Group, y = rate)) +
  geom_point(position = position_dodge(0.3), size=3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL),
    width = 0.2,
    position = position_dodge(0.3)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(
    y = "Predicted # of AI-Generated Messages Selected",
    x = NULL
  ) + 
  theme_classic() +
  ylim(0, 5)

#Supplementary Material A: Demographics by recruitment platform
df_total_prolific <-
  df_total  %>% filter(df_total$recruitment_platform == "Prolific")
mean(df_total_prolific$Age, na.rm=TRUE)
sd(df_total_prolific$Age, na.rm=TRUE)
table(df_total_prolific$Sex) / length(df_total_prolific$Sex)

df_total_sona <-
  df_total  %>% filter(df_total$recruitment_platform == "SONA")
mean(df_total_sona$Age, na.rm=TRUE)
sd(df_total_sona$Age, na.rm=TRUE)
table(df_total_sona$Sex) / length(df_total_sona$Sex)
