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

# Add descriptive label for each condition
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

# Show the value in only the first row of every participant to improve readability
# Comment out when we need the data
survey_vars <- c("participant", "session_duration", "latin_square", "age", "gender", "education", "comfort", "ai_usage", "Pos1", "Pos2", "Pos3", "Pos4", 
                 "Neg1", "Neg2", "Neg3", "Neg4")

df_everything <- df_everything %>%
  group_by(participant) %>%  # Change this to your actual column name if needed
  mutate(across(any_of(survey_vars), ~ ifelse(row_number() == 1, as.character(.), ""))) %>%
  ungroup()

# Show the value only every 5th row
# Comment out when we need the data
condition_vars <- c("topic", "condition", "condition_label", 
                    "B1", "B2", "B3", 
                    "I1", "I2", "I3", "I4", 
                    "C1", "C2", "C3", "C4")

df_everything <- df_everything %>%
  group_by(participant) %>%
  mutate(task_row = row_number()) %>%
  mutate(across(any_of(condition_vars), ~ ifelse(task_row %% 5 == 1, as.character(.), ""))) %>%
  select(-task_row) %>%
  ungroup()

# Remove the date in session_duration
df_everything <- df_everything %>%
  mutate(session_duration = str_extract(session_duration, "\\d{2}:\\d{2}:\\d{2}"))