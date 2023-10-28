library(readr)
library(dplyr)

setwd("/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/02_Data/Study 1/00_raw_data_files")

df_sona <- read_csv('study1_sona.csv')
df_sona2 <- df_sona[-c(1, 2),]
df_sona3 <- df_sona2[c(3, 5:7, 18:319)]
#rename columns and levels
names(df_sona3)[5:154] <- paste0("C_", names(df_sona3)[5:154])
names(df_sona3)[155:304] <- paste0("T_", names(df_sona3)[155:304])
names(df_sona3) <- gsub("_1\\.\\.\\.[0-9]+$", "", names(df_sona3))
names(df_sona3)[names(df_sona3) %in% c("Q1", "Q2")] <- c("Age", "Sex")
df_sona3$Sex <- recode_factor(df_sona3$Sex, "1" = "Male", "2" = "Female",  
                              "3" = "Other", "5" = "Other", "8" = "Prefer not to say", 
                              "2,5" = "Other", "3,8" = "Other")
write.csv(df_sona3, "/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/02_Data/Study 1/01_raw_data_anonymized/study1_sona_anonymized.csv", row.names = FALSE)

df_pro <- read_csv('study1_prolific.csv')
df_pro_demo <- read_csv('study1_prolific_demo.csv')
df_pro_demo2 <- df_pro_demo[c(2, 12)]
df_prolific <- merge(df_pro, df_pro_demo2, by.x = "Prolific_ID", by.y = "Participant id")
df_prolific2 <- df_prolific[c(4, 6:8, 20:320, 322:323)]
df_prolific2$Sex <- recode_factor(df_prolific2$Sex, "CONSENT_REVOKED" = "NA")
write.csv(df_prolific2, "/Users/suelim/Documents/Research/Evaluation_Vaping_Messages/data/02_Data/Study 1/01_raw_data_anonymized/study1_prolific_anonymized.csv", row.names = FALSE)
