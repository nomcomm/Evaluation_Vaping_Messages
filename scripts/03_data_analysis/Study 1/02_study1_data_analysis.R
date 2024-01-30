library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(WebPower)
library(readr)
library(lmerTest)
library(lme4)
library(ordinal)

#power analysis for mixed ANOVA
wp.rmanova(ng = 2, nm = 3, f = 0.25, nscor = 1, alpha = 0.05, power = .8, type = 2)

#set directory and load file
setwd("/Users/suelim/Documents/Research/DONE/Evaluation_Vaping_Messages/github/Evaluation_Vaping_Messages/data/02_Data/study1/02_cleaned_data_file")
df_total <- read_csv('study1_data_file.csv')

#demographics
table(df_total$Experimental_Group)
mean(df_total$Age, na.rm=TRUE)
sd(df_total$Age, na.rm=TRUE)
table(df_total$Sex) / length(df_total$Sex)

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

#cronbach alpha reliability calculations
alpha_values <- numeric(30)
for (i in 1:15) {
  # Create a subset for each message (AI and Human)
  subset_ai <- df_total[, c(paste("Dis_AI", i, sep=""), paste("Con_AI", i, sep=""), paste("Unp_AI", i, sep=""))]
  subset_hum <- df_total[, c(paste("Dis_Hum", i, sep=""), paste("Con_Hum", i, sep=""), paste("Unp_Hum", i, sep=""))]
  # Calculate and store Cronbach's alpha for each message
  alpha_values[i] <- alpha(subset_ai)$total$raw_alpha
  alpha_values[i + 15] <- alpha(subset_hum)$total$raw_alpha
}
print(alpha_values)
mean(alpha_values)

# Mixed ANOVA
df_total_AI_3EPs <- df_total[c(1:2, 156:170, 172:186)]
df_total_AI_3EPs_pivot <- df_total_AI_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))
df_total_AI_3EPs_pivot$AI_or_Hum <- as.factor(df_total_AI_3EPs_pivot$AI_or_Hum)
df_total_AI_3EPs_pivot$AI_or_Hum <- relevel(df_total_AI_3EPs_pivot$AI_or_Hum, ref = "AI")
df_total_AI_3EPs_pivot$Participant <- as.factor(df_total_AI_3EPs_pivot$Participant)
df_total_AI_3EPs_pivot$Experimental_Group <- as.factor(df_total_AI_3EPs_pivot$Experimental_Group)
df_total_AI_3EPs_pivot$Experimental_Group <- relevel(df_total_AI_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")
df_total_AI_3EPs_pivot$Messages <- as.factor(df_total_AI_3EPs_pivot$Messages)

df_total_AI_3EPs_pivot %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")

#EP_Mixed_Effects <- 
EP_Mixed_Effects <- 
  lmer(Effects_Perception ~ Experimental_Group * AI_or_Hum + (1 | Participant) + (1 | Messages), data = df_total_AI_3EPs_pivot)
Anova(EP_Mixed_Effects, type="3")
EP_predict <-
  emmeans(EP_Mixed_Effects, ~ Experimental_Group * AI_or_Hum)
EP_predict_df <- as.data.frame(EP_predict)

ggplot(EP_predict_df, aes(x = Experimental_Group, y = emmean)) +
  geom_line(aes(group = AI_or_Hum), position = position_dodge(0.3), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.3), size=3) +
  geom_errorbar(
    aes(ymin = asymp.LCL, ymax = asymp.UCL, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.3)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  #facet_wrap(~Experimental_Group) + 
  labs(
    y = "Effects Perception",
    x = NULL,
    color = "Message Source"  # This line changes the legend title
  ) + 
  theme_bw() +
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) +
  ylim(3.9, 4.6)
emmeans(EP_Mixed_Effects, pairwise~Experimental_Group*AI_or_Hum, adjust="tukey")

# ez::ezANOVA(
#   data = df_total_AI_3EPs_pivot,
#   wid = .(Participant),
#   within = .(AI_or_Hum),
#   between = .(Experimental_Group),
#   dv = .(Effects_Perception),
#   type = 3
# )


#Mixed-Effects Ordinal Regression
df_total_rank <- df_total[c(1:2, 5:34, 155)]
df_total_rank_pivot <- df_total_rank |> 
  pivot_longer(!c(Experimental_Group, Participant, recruitment_platform), names_to="Messages", 
               values_to="Rank") 
df_total_rank_pivot$Messages <- sub("Rank_", "", df_total_rank_pivot$Messages)
df_total_rank_pivot <- df_total_rank_pivot |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))

df_total_rank_pivot %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Rank, type = "full")

df_total_rank_pivot$Rank <- as.factor(df_total_rank_pivot$Rank)
df_total_rank_pivot$Rank <- ordered(df_total_rank_pivot$Rank)
df_total_rank_pivot$AI_or_Hum <- factor(df_total_rank_pivot$AI_or_Hum)
df_total_rank_pivot$Experimental_Group <- factor(df_total_rank_pivot$Experimental_Group)
df_total_rank_pivot$Experimental_Group <- relevel(df_total_rank_pivot$Experimental_Group, ref = "Not Disclosed")

ordinal_reg <- 
  clmm(Rank ~ Experimental_Group*AI_or_Hum + (1|Participant) + (1|Messages), link="logit", threshold = "equidistant", data = df_total_rank_pivot)
summary(ordinal_reg)








# df_total_KrusWall <- 
#   df_total[c("Experimental_Group", "Participant", "AI_MSG_Rank_mean", "Hum_MSG_Rank_mean")]
# df_total_KrusWall_pivot <- 
#   df_total_KrusWall |> 
#   pivot_longer(!c(Experimental_Group, Participant), names_to="AI_or_Hum", values_to="Rank")
# df_total_KrusWall_pivot$Experimental_Group <- 
#   factor(df_total_KrusWall_pivot$Experimental_Group, levels = c("Not Disclosed", "Disclosed"))
# df_total_KrusWall$rankdiff <- 
#   df_total_KrusWall$AI_MSG_Rank_mean - df_total_KrusWall$Hum_MSG_Rank_mean
# 
# group_by(df_total_KrusWall, Experimental_Group) %>% 
#   dplyr::summarise(
#     count = n(),
#     mean = mean(rankdiff, na.rm = TRUE),
#     sd = sd(rankdiff, na.rm = TRUE),
#     median = median(rankdiff, na.rm = TRUE),
#     IQR = IQR(rankdiff, na.rm = TRUE)
#   )
# 
# wilcox.test(rankdiff ~ Experimental_Group, data = df_total_KrusWall, exact=FALSE)

##Data visualization
df_total_KrusWall_pivot |>
  ggplot(aes(x=AI_or_Hum, y=Rank, fill = AI_or_Hum)) +
  geom_boxplot() +
  theme(
    plot.title = element_text(size=15,hjust=0.5),
    axis.title.y=element_text(size=11,hjust=0.5)
  ) +
  xlab("") + 
  #ylim(0,25) +
  labs(fill = "Source", y= "Preference Rank (1 = Best, 30 = Worse)") + 
  scale_x_discrete(labels=c(AI_MSG_Rank_mean = "AI", Hum_MSG_Rank_mean = "Human")) +
  facet_wrap(~Experimental_Group) + 
  scale_fill_manual(values = c("AI_MSG_Rank_mean" = "steelblue", "Hum_MSG_Rank_mean" = "darkgreen")) + 
  theme_bw() + 
  theme(legend.position = "none")

