library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(WebPower)
library(readr)
library(lmerTest)
library(lme4)
library(ordinal)
library(emmeans)
library(psych)

#set directory and load file
setwd("/Users/suelim/Documents/Research/DONE/Evaluation_Vaping_Messages/github/Evaluation_Vaping_Messages/data/02_Data/study1/02_cleaned_data_file")
df_total <- read_csv('study1_data_file.csv')

#creating Effects Perception (EP) and Ranking variables
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

sum_Rank_AI <- 0
sum_Rank_Human <- 0
for (i in 1:15) {
  sum_Rank_AI <- sum_Rank_AI + df_total[[paste0("Rank_AI", i)]]
}
df_total$AI_MSG_Rank_mean <- sum_Rank_AI / 15
for (i in 1:15) {
  sum_Rank_Human <- sum_Rank_Human + df_total[[paste0("Rank_Human", i)]]
}
df_total$Hum_MSG_Rank_mean <- sum_Rank_Human / 15

colnames(df_total)

#demographics, separated by sample
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

#cronbach alpha reliability calculations
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


# Preparing data structure for mixed-effects modeling
df_total_AI_3EPs <- df_total[c(1:2, 155:170, 172:186)]
df_total_AI_3EPs_pivot <- df_total_AI_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant, recruitment_platform), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))
df_total_AI_3EPs_pivot$AI_or_Hum <- as.factor(df_total_AI_3EPs_pivot$AI_or_Hum)
df_total_AI_3EPs_pivot$AI_or_Hum <- relevel(df_total_AI_3EPs_pivot$AI_or_Hum, ref = "AI")
df_total_AI_3EPs_pivot$Participant <- as.factor(df_total_AI_3EPs_pivot$Participant)
df_total_AI_3EPs_pivot$Experimental_Group <- as.factor(df_total_AI_3EPs_pivot$Experimental_Group)
df_total_AI_3EPs_pivot$Experimental_Group <- relevel(df_total_AI_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")
df_total_AI_3EPs_pivot$Messages <- as.factor(df_total_AI_3EPs_pivot$Messages)
df_total_AI_3EPs_pivot$recruitment_platform <- as.factor(df_total_AI_3EPs_pivot$recruitment_platform)

#EP_Mixed_Effects <- 
  #lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum + (1 | Participant) + (1 | Messages), data = df_total_AI_3EPs_pivot)
#summary(EP_Mixed_Effects)
#EP_predict <- 
  #emmeans(EP_Mixed_Effects, ~ Experimental_Group * AI_or_Hum)
# EP_predict_df <- as.data.frame(EP_predict)
# ggplot(EP_predict_df, aes(x = AI_or_Hum, y = emmean)) +
#   geom_line(aes(group = Experimental_Group), position = position_dodge(0.9), color="dimgray") +
#   geom_point(aes(color = AI_or_Hum), position = position_dodge(0.9), size=3) +
#   geom_errorbar(
#     aes(ymin = asymp.LCL, ymax = asymp.LCL, color = AI_or_Hum),
#     width = 0.2,
#     position = position_dodge(0.9)
#   ) +
#   theme(plot.title = element_text(hjust = 0.5)) +
#   facet_wrap(~Experimental_Group) + 
#   labs(y = "Effects Perception",
#        x = NULL) + 
#   theme_bw() + 
#   scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) + 
#   theme(legend.position = "none") +   ylim(3,5)
# emmeans(EP_Mixed_Effects, pairwise~Experimental_Group*AI_or_Hum, adjust="tukey")


#Prolific sample
df_total_AI_3EPs_pivot_prolific <-  
  df_total_AI_3EPs_pivot %>% 
  filter(df_total_AI_3EPs_pivot$recruitment_platform == "Prolific")

##means and standard deviations of EP and ranking
df_total_AI_3EPs_pivot_prolific %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")

df_total_AI_3EPs_pivot_prolific %>%
  get_summary_stats(Effects_Perception, type = "full")

##mixed effects modeling
EP_Mixed_Effects_prolific <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum + (1 | Participant) + (1 | Messages), data = df_total_AI_3EPs_pivot_prolific)
summary(EP_Mixed_Effects_prolific)
EP_predict_prolific <- 
  emmeans(EP_Mixed_Effects_prolific, ~ Experimental_Group * AI_or_Hum)
EP_predict_prolific_df <- as.data.frame(EP_predict_prolific)
ggplot(EP_predict_prolific_df, aes(x = AI_or_Hum, y = emmean)) +
  geom_line(aes(group = Experimental_Group), position = position_dodge(0.9), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.9), size=3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Experimental_Group) + 
  labs(y = "Effects Perception",
       x = NULL) + 
  theme_bw() + 
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) + 
  theme(legend.position = "none") +   ylim(3,5)


#SONA sample
df_total_AI_3EPs_pivot_sona <- 
  df_total_AI_3EPs_pivot %>% 
  filter(df_total_AI_3EPs_pivot$recruitment_platform == "SONA")

##means and standard deviations of EP and ranking
df_total_AI_3EPs_pivot_sona %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")

df_total_AI_3EPs_pivot_sona %>%
  get_summary_stats(Effects_Perception, type = "full")

##mixed effects model
EP_Mixed_Effects_sona <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum + (1 | Participant) + (1 | Messages), data = df_total_AI_3EPs_pivot_sona)
summary(EP_Mixed_Effects_sona)
EP_predict_sona <- 
  emmeans(EP_Mixed_Effects_sona, ~ Experimental_Group * AI_or_Hum)
EP_predict_sona_df <- as.data.frame(EP_predict_sona)
ggplot(EP_predict_sona_df, aes(x = AI_or_Hum, y = emmean)) +
  geom_line(aes(group = Experimental_Group), position = position_dodge(0.9), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.9), size=3) +
  geom_errorbar(
    aes(ymin = lower.CL, ymax = upper.CL, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Experimental_Group) + 
  labs(y = "Effects Perception",
       x = NULL) + 
  theme_bw() + 
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) + 
  theme(legend.position = "none") +   ylim(3,5)
emmeans(EP_Mixed_Effects_sona, pairwise~Experimental_Group*AI_or_Hum, adjust="tukey")

#Preparing data structure for ordinal regression
df_total_rank <- df_total[c(1:2, 5:34, 155)]
df_total_rank_pivot <- df_total_rank |> 
  pivot_longer(!c(Experimental_Group, Participant, recruitment_platform), names_to="Messages", 
               values_to="Rank") 
df_total_rank_pivot$Messages <- sub("Rank_", "", df_total_rank_pivot$Messages)
df_total_rank_pivot <- df_total_rank_pivot |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))
df_total_rank_pivot$Rank <- factor(df_total_rank_pivot$Rank)
df_total_rank_pivot$Rank <- ordered(df_total_rank_pivot$Rank)
df_total_rank_pivot$AI_or_Hum <- factor(df_total_rank_pivot$AI_or_Hum)
df_total_rank_pivot$Experimental_Group <- factor(df_total_rank_pivot$Experimental_Group)
df_total_rank_pivot$Experimental_Group <- relevel(df_total_rank_pivot$Experimental_Group, ref = "Not Disclosed")

# ordinal_reg <- clmm(Rank ~ Experimental_Group*AI_or_Hum + (1|Participant) + (1|Messages), link="logit", data = df_total_rank_pivot)
# summary(ordinal_reg)
# emmeans(ordinal_reg, ~ Experimental_Group*AI_or_Hum, mode = "mean.class")

#Prolific
df_total_rank_pivot_prolific <- 
  df_total_rank_pivot |> 
  filter(df_total_rank_pivot$recruitment_platform == "Prolific")

ordinal_reg_prolific <- clmm(Rank ~ Experimental_Group*AI_or_Hum + (1|Messages), link="logit", data = df_total_rank_pivot_prolific)
summary(ordinal_reg_prolific)
rank_predict_prolific <- emmeans(ordinal_reg_prolific, ~ Experimental_Group*AI_or_Hum, mode = "mean.class")
rank_predict_prolific_df <- as.data.frame(rank_predict_prolific)

ggplot(rank_predict_prolific_df, aes(x = AI_or_Hum, y = mean.class)) +
  geom_line(aes(group = Experimental_Group), position = position_dodge(0.9), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.9), size=3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Experimental_Group) + 
  labs(y = "Ranking (1=Best, 30=Worst)",
       x = NULL) + 
  theme_bw() + 
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) + 
  theme(legend.position = "none") +
  ylim(10,20)

df_total_rank_pivot_sona <- 
  df_total_rank_pivot |> 
  filter(df_total_rank_pivot$recruitment_platform == "SONA")
ordinal_reg_sona <- clmm(Rank ~ Experimental_Group*AI_or_Hum + (1|Messages), link="logit", data = df_total_rank_pivot_sona)
summary(ordinal_reg_sona)
rank_predict_sona <- emmeans(ordinal_reg_sona, ~ Experimental_Group*AI_or_Hum, mode = "mean.class")
rank_predict_sona_df <- as.data.frame(rank_predict_sona)

ggplot(rank_predict_sona_df, aes(x = AI_or_Hum, y = mean.class)) +
  geom_line(aes(group = Experimental_Group), position = position_dodge(0.9), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.9), size=3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Experimental_Group) + 
  labs(y = "Ranking (1=Best, 30=Worst)",
       x = NULL) + 
  theme_bw() + 
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) + 
  theme(legend.position = "none") +
  ylim(10,20)
