rm(list = ls()) 

library(tidyr)
library(ggplot2)
library(dplyr)
library(rstatix)
library(WebPower)
library(readr)

setwd("/Users/suelim/Documents/Research/Evaluation Vaping Messages/Evaluation_Vaping_Messages/data/02_Data/Study 1")

#power analysis for mixed ANOVA
wp.rmanova(ng = 2, nm = 2, f = 0.25, nscor = 1, alpha = 0.05, power = .8, type = 2)

#demographics
df_total <- read_csv('final_data_file.csv')
table(df_total$Experimental_Group)
df_total$Age <- as.numeric(df_total$Age)
mean(df_total$Age, na.rm=TRUE)
sd(df_total$Age, na.rm=TRUE)
nrow(df_total)
table(df_total$Sex)
84/142
table(df_total$Ethnicity)
96/142

#creating Effects Perception (EP) and Ranking variables
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

df_total$AI_MSG_Rank_mean <- 
  (df_total$'Ranking_AI1' + df_total$'Ranking_AI2' + df_total$'Ranking_AI3' + 
     df_total$'Ranking_AI4' + df_total$'Ranking_AI5' + df_total$'Ranking_AI6' + 
     df_total$'Ranking_AI7' + df_total$'Ranking_AI8' + df_total$'Ranking_AI9' + 
     df_total$'Ranking_AI10' + df_total$'Ranking_AI11' + df_total$'Ranking_AI12' + 
     df_total$'Ranking_AI13' + df_total$'Ranking_AI14' + df_total$'Ranking_AI15')/15

df_total$Hum_MSG_Rank_mean <- 
  (df_total$'Ranking_Human1' + df_total$'Ranking_Human2' + df_total$'Ranking_Human3' + 
     df_total$'Ranking_Human4' + df_total$'Ranking_Human5' + df_total$'Ranking_Human6' + 
     df_total$'Ranking_Human7' + df_total$'Ranking_Human8' + df_total$'Ranking_Human9' + 
     df_total$'Ranking_Human10' + df_total$'Ranking_Human11' + df_total$'Ranking_Human12' + 
     df_total$'Ranking_Human13' + df_total$'Ranking_Human14' + df_total$'Ranking_Human15')/15

# Mixed ANOVA
colnames(df_total)
df_total_AI_3EPs <- df_total[c(1:2, 157:186)]
df_total_AI_3EPs_pivot <- df_total_AI_3EPs |> 
  pivot_longer(!c(Experimental_Group, Participant), names_to="Messages", 
               values_to="Effects_Perception") |> 
  mutate(AI_or_Hum = ifelse(grepl("^AI", Messages), "AI", "Human"))
df_total_AI_3EPs_pivot$AI_or_Hum <- as.factor(df_total_AI_3EPs_pivot$AI_or_Hum)
df_total_AI_3EPs_pivot$AI_or_Hum <- relevel(df_total_AI_3EPs_pivot$AI_or_Hum, ref = "AI")
df_total_AI_3EPs_pivot$Participant <- as.factor(df_total_AI_3EPs_pivot$Participant)
df_total_AI_3EPs_pivot$Experimental_Group <- as.factor(df_total_AI_3EPs_pivot$Experimental_Group)
df_total_AI_3EPs_pivot$Experimental_Group <- relevel(df_total_AI_3EPs_pivot$Experimental_Group, ref = "Not Disclosed")

ez::ezANOVA(
  data = df_total_AI_3EPs_pivot,
  wid = .(Participant),
  within = .(AI_or_Hum),
  between = .(Experimental_Group),
  dv = .(Effects_Perception),
  type = 3
)

##Data visualization
means_CI <-
  df_total_AI_3EPs_pivot %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_ci") %>%
  mutate(
    lower_CI = mean - ci,
    upper_CI = mean + ci
  )

ggplot(means_CI, aes(x = AI_or_Hum, y = mean)) +
  geom_line(aes(group = Experimental_Group), position = position_dodge(0.9), color="dimgray") +
  geom_point(aes(color = AI_or_Hum), position = position_dodge(0.9)) +
  geom_errorbar(
    aes(ymin = lower_CI, ymax = upper_CI, color = AI_or_Hum),
    width = 0.2,
    position = position_dodge(0.9)
  ) +
  theme(plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~Experimental_Group) + 
  ylim(3,5) + 
  labs(y = "Effects Perception",
       x = "Message Source") + 
  theme_bw() + 
  scale_color_manual(values = c("AI" = "steelblue", "Human" = "darkgreen")) + 
  theme(legend.position = "none")


##Post-hoc t-tests
means <-
  df_total_AI_3EPs_pivot %>%
  group_by(Experimental_Group, AI_or_Hum) %>%
  get_summary_stats(Effects_Perception, type = "mean_sd")
means

df_total_AI_3EPs_pivot_AI <- df_total_AI_3EPs_pivot %>% filter(AI_or_Hum == "AI")
df_total_AI_3EPs_pivot_Hum <- df_total_AI_3EPs_pivot %>% filter(AI_or_Hum == "Human")

t.test(Effects_Perception ~ Experimental_Group, data = df_total_AI_3EPs_pivot_AI)
t.test(Effects_Perception ~ Experimental_Group, data = df_total_AI_3EPs_pivot_Hum)


#Wilcoxon Rank Sum Analysis
df_total_KrusWall <- df_total[c("Experimental_Group", "Participant", "AI_MSG_Rank_mean", "Hum_MSG_Rank_mean")]
df_total_KrusWall_pivot <- df_total_KrusWall |> pivot_longer(!c(Experimental_Group, Participant), names_to="AI_or_Hum", values_to="Rank")
df_total_KrusWall_pivot$Experimental_Group <- factor(df_total_KrusWall_pivot$Experimental_Group, levels = c("Not Disclosed", "Disclosed"))
df_total_KrusWall$rankdiff <- df_total_KrusWall$AI_MSG_Rank_mean - df_total_KrusWall$Hum_MSG_Rank_mean

group_by(df_total_KrusWall, Experimental_Group) %>% 
  dplyr::summarise(
    count = n(),
    mean = mean(rankdiff, na.rm = TRUE),
    sd = sd(rankdiff, na.rm = TRUE),
    median = median(rankdiff, na.rm = TRUE),
    IQR = IQR(rankdiff, na.rm = TRUE)
  )

wilcox.test(rankdiff ~ Experimental_Group, data = df_total_KrusWall, exact=FALSE)

##Data visualization
df_total_KrusWall_pivot |>
  ggplot(aes(x=AI_or_Hum, y=Rank, fill = AI_or_Hum)) +
  geom_boxplot() +
  theme(
    plot.title = element_text(size=15,hjust=0.5),
    axis.title.y=element_text(size=11,hjust=0.5)
  ) +
  xlab("") + 
  ylim(0,25) +
  labs(fill = "Source", y= "Preference Rank (1 = Best, 30 = Worse)") + 
  scale_x_discrete(labels=c(AI_MSG_Rank_mean = "AI", Hum_MSG_Rank_mean = "Human")) +
  facet_wrap(~Experimental_Group) + 
  scale_fill_manual(values = c("AI_MSG_Rank_mean" = "steelblue", "Hum_MSG_Rank_mean" = "darkgreen")) + 
  theme_bw() + 
  theme(legend.position = "none")