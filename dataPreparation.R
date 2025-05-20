# Install required packages if not already installed
if (!require("here")) install.packages("here")
if (!require("readxl")) install.packages("readxl")
if (!require("dplyr")) install.packages("dplyr")
if (!require("stringr")) install.packages("stringr")

# Load packages
library(here)
library(readxl)
library(dplyr)
library(stringr)

# Set wd
#setwd("Users/tania/OneDrive/Skrivebord/Thesis-R")

# This is an initial script for testing our R
df_conditions <- read_excel(here("conditionsR.xlsx"))
df_participants <- read_excel(here("participantsR.xlsx"))
df_tasks <- read_excel(here("tasksR.xlsx"))

# Step 1: Add topic info to each task based on question_number
df_tasks <- df_tasks %>%
  mutate(topic = case_when(
    question_number %in% 1:5 ~ "Music",
    question_number %in% 6:10 ~ "Health",
    question_number %in% 11:15 ~ "Geography",
    question_number %in% 16:20 ~ "Physics"
  ))

# Step 2: Merge condition info based on participant + topic
df_tasks_with_conditions <- df_tasks %>%
  left_join(df_conditions, by = c("participant", "topic"))

# Step 3: Merge participant-level info
df_everything <- df_tasks_with_conditions %>%
  left_join(df_participants, by = "participant")

# Condition name
df_everything <- df_everything %>%
  mutate(condition_label = case_when(
    condition == "A" ~ "CertainI",
    condition == "B" ~ "UncertainI",
    condition == "C" ~ "CertainSystem",
    condition == "D" ~ "UncertainSystem",
    TRUE ~ NA_character_
  ))

# Add condition_label after condition
df_everything <- df_everything %>%
  select(participant, session_duration, latin_square, age, gender, education,
         comfort, ai_usage, Neg1, Pos1, Neg2, Pos2, Neg3, Pos3, Neg4, Pos4,
         question_number, topic, condition, condition_label, everything())

# Remove the date in session_duration
df_everything <- df_everything %>%
  mutate(session_duration = str_extract(session_duration, "\\d{2}:\\d{2}:\\d{2}"))

# Trust measures - Ability, Benevolence and Integrity. Calculating a final score.
# Total Competence score
df_everything<- df_everything %>%
  mutate(across(c(C1, C2, C3, C4), 
                ~as.numeric(str_extract(., "^\\d+"))))

df_everything$Competence <- (df_everything$C1 + df_everything$C2 + df_everything$C3 + df_everything$C4) / 4

# Total Benovelence score
df_everything<- df_everything %>%
  mutate(across(c(B1, B2, B3), 
                ~as.numeric(str_extract(., "^\\d+"))))

df_everything$Benevolence <- (df_everything$B1 + df_everything$B2 + df_everything$B3) / 3

# Total Integrity score
df_everything<- df_everything %>%
  mutate(across(c(I1, I2, I3, I4), 
                ~as.numeric(str_extract(., "^\\d+"))))

df_everything$Integrity <- (df_everything$I1 + df_everything$I2 + df_everything$I3 + df_everything$I4) / 4

# Total GAAIS score
df_everything <- df_everything %>%
  mutate(across(c(Neg1, Pos1, Neg2, Pos2, Neg3, Pos3, Neg4, Pos4),
                ~as.numeric(str_extract(., "^\\d+"))))

df_everything$GAAIS <- (df_everything$Neg1 + df_everything$Pos1 + df_everything$Neg2 + df_everything$Pos2
                        + df_everything$Neg3 + df_everything$Pos3 + df_everything$Neg4 + df_everything$Pos4) / 8

# Renamed to asked_chat
df_everything <- df_everything %>%
  rename(asked_chat = prompt_count)

# If both are truthful, they are in agreement. If only one is truthful, they are in disagreement
df_everything <- df_everything %>%
  mutate(source_agreement = case_when(
    chatbot_lying == 0 & google_lying == 0 ~ "Agreement",
    chatbot_lying != google_lying ~ "Disagreement",
    TRUE ~ NA_character_  
  ))

# If participant's answer is aligned with the correct answer or not
df_everything <- df_everything %>%
  mutate(answer_correct = case_when(
    answer_yesno == 0 & correct_answer == 0 ~ "Correct",
    answer_yesno == 1 & correct_answer == 1 ~ "Correct",
    answer_yesno != correct_answer ~ "Incorrect",
    TRUE ~ NA_character_  
  ))

# No = 0, Yes = 1
df_everything <- df_everything %>%
  mutate(answer_yesno = if_else(answer_yesno  == 0, 1, 0))

# No = 0, Yes = 1
df_everything <- df_everything %>%
  mutate(correct_answer = if_else(correct_answer  == 0, 1, 0))

# Rename to answer_noyes
df_everything <- df_everything %>%
  rename(answer_noyes = answer_yesno)

#df_everything<- df_everything %>%
# mutate(primary_source = recode(primary_source,
#                                '0' = "Chatbot",
#                                '1' = "Google",
#                                '2' = "Prior knowledge"))

# Mean confidence for each condition for each participant
df_everything<- df_everything%>%
  group_by(condition_label, participant) %>%
  mutate(mean_confidence = mean(confidence, na.rm = TRUE)) %>%
  ungroup()

# Which condition the participant started in
df_everything <- df_everything %>%
  mutate(first_condition = case_when(
    latin_square == "1" ~ "CertainI",
    latin_square == "2" ~ "UncertainI",
    latin_square == "3" ~ "CertainSystem",
    latin_square == "4" ~ "UncertainSystem",
    TRUE ~ NA_character_
  ))

# New dataframe for analysis
df_analysis <- df_everything %>% select(participant, age, gender, education, comfort, ai_usage, GAAIS, question_number,
                                        topic, condition, condition_label, first_condition, asked_chat, google_checked,
                                        confidence, mean_confidence, answer_correct, primary_source, source_agreement,
                                        Competence, Benevolence, Integrity)

source("analysisTest.R")