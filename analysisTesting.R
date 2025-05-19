# Install required packages if not already installed
if (!require("here")) install.packages("here")
if (!require("readxl")) install.packages("readxl")

# Load packages
library(here)
library(readxl)

# This is an initial script for testing our R
df_conditions <- read_excel(here("conditionsR.xlsx"))
df_participants <- read_excel(here("participantsR.xlsx"))
df_tasks <- read_excel(here("tasksR.xlsx"))