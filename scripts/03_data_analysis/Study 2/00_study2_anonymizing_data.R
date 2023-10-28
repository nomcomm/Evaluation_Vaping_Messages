library(readr)
library(dplyr)
library(tidyr)

setwd("/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/02_Data/Study 2/00_raw_data_files")

df_sona <- read_csv('study2_sona.csv')
df_sona2 <- df_sona[-c(1, 2),]
df_sona3 <- df_sona2[c(3, 5:7, 19:221, 225:226, 231)]
#rename columns and levels
df_sona3$Sex <- recode_factor(df_sona3$Sex, "1" = "Male", "2" = "Female",  
                              "3" = "Other", "5" = "Other", "8" = "Prefer not to say", 
                              "1,2,3,4,5" = "Other")
df_sona3 <- df_sona3 %>%
  separate(`C_Message Rank`, into = c("C_Message1", "C_Message2", "C_Message3", "C_Message4", "C_Message5"), sep = ",", remove = FALSE)
df_sona3 <- df_sona3 %>%
  separate(`T_Message Ranking`, into = c("T_Message1", "T_Message2", "T_Message3", "T_Message4", "T_Message5"), sep = ",", remove = FALSE)
write.csv(df_sona3, "/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/02_Data/Study 2/01_raw_data_anonymized/study2_sona_anonymized.csv", row.names = FALSE)

df_pro <- read_csv('study2_prolific.csv')
df_pro_demo <- read_csv('study2_prolific_demo.csv')
df_pro_demo2 <- df_pro_demo[c(2, 12, 13)]
df_prolific <- merge(df_pro, df_pro_demo2, by.x = "Prolific_ID", by.y = "Participant id")
df_prolific2 <- df_prolific[c(4, 6:8, 20:225, 230:232)]
df_prolific2$Age <- recode_factor(df_prolific2$Age, "DATA_EXPIRED" = "NA")
df_prolific2$Sex <- recode_factor(df_prolific2$Sex, "CONSENT_REVOKED" = "NA")
df_prolific2 <- df_prolific2 %>% rename(C_Attention_Check1 = Attention_Check1_1)
df_prolific2 <- df_prolific2 %>% rename(T_Attention_Check1 = Attention_Check1_T_1)
df_prolific2 <- df_prolific2 %>%
  separate(`C_Message Rank`, into = c("C_Message1", "C_Message2", "C_Message3", "C_Message4", "C_Message5"), sep = ",", remove = FALSE)
df_prolific2 <- df_prolific2 %>%
  separate(`T_Message Ranking`, into = c("T_Message1", "T_Message2", "T_Message3", "T_Message4", "T_Message5"), sep = ",", remove = FALSE)
write.csv(df_prolific2, "/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/02_Data/Study 2/01_raw_data_anonymized/study2_prolific_anonymized.csv", row.names = FALSE)
