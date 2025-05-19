# Install required packages if not already installed
if (!require("here")) install.packages("here") # For loading files
if (!require("readxl")) install.packages("readxl") # for reading xlsx files
if (!require("dplyr")) install.packages("dplyr") # for data wrangling
if (!require("tidyr")) install.packages("tidyr")

# Load packages
library(here)
library(readxl)
library(dplyr)
library(tidyr)

# Load and store each file as a dataframe
df_conditions <- read_excel(here("conditionsR.xlsx"))
df_participants <- read_excel(here("participantsR.xlsx"))
df_tasks <- read_excel(here("tasksR.xlsx"))



# ==== COMBINE ALL DATA ====


# == 1. Add topic to each task

question_topic_map <- tibble::tibble(
  question_number = 1:20,
  topic = rep(c("Music", "Health", "Geography", "Physics"), each = 5)
)
# Add topic to each task row
df_tasks <- df_tasks %>%
  left_join(question_topic_map, by = "question_number")


# == 2. Add condition (A/B/C/D) to each task based on topic and participant
df_tasks <- df_tasks %>%
  left_join(df_conditions %>% select(participant, topic, condition), 
            by = c("participant", "topic"))


# == 3. Rename condition letters with new labels

condition_label_map <- c(
  A = "certainI",
  B = "uncertainI",
  C = "certainSystem",
  D = "uncertainSystem"
)

# Apply mapping to both conditions and tasks
df_conditions <- df_conditions %>%
  mutate(condition_label = condition_label_map[condition])

df_tasks <- df_tasks %>%
  mutate(condition_label = condition_label_map[condition])


# == 4. Add participant-level info into tasks
df_tasks <- df_tasks %>%
  left_join(df_participants, by = "participant")


# == 5. Add condition-level trust measures (only for that condition)
df_tasks <- df_tasks %>%
  left_join(
    df_conditions %>% 
      select(participant, condition_label, B1, B2, B3, I1, I2, I3, I4, C1, C2, C3, C4),
    by = c("participant", "condition_label")
  )


# == 6. Save combined table
# Now df_tasks is your fully combined long-format dataset:
df_everything <- df_tasks

# Preview structure and some rows
str(df_everything)
head(df_everything)