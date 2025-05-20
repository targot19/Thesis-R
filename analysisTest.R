# List of required packages
required_packages <- c(
  "dplyr", "ggplot2", "tidyr", "readr", "tibble", "stringr",
  "lmerTest", "lubridate", "sjPlot", "emmeans", "MuMIn",
  "ggeffects", "broom", "stargazer", "here", "readxl", "lme4"
)

# Install any packages not already installed
installed_packages <- rownames(installed.packages())
for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
}

# Load packages
library(here)
library(readxl)
library(dplyr)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(tibble)
library(stringr)
library(lme4)
library(lubridate)
library(sjPlot)
library(emmeans)
library(car)
library(lmerTest)
library(MuMIn)
library(ggeffects)
library(broom)
library(here)
library(readxl)

# Shapiro tests that Naja recommended we do to test whether the data is normal
shapirotest <- shapiro.test(df_analysis$confidence)
print(shapirotest)

shapirotest <- shapiro.test(df_analysis$Benevolence)
print(shapirotest)

shapirotest <- shapiro.test(df_analysis$Integrity)
print(shapirotest)

shapirotest <- shapiro.test(df_analysis$Competence)
print(shapirotest)

shapirotest <- shapiro.test(df_analysis$GAAIS)
print(shapirotest)

# ANOVA test of Benevolence (trust)
anova_result <- aov(Benevolence ~ topic + age + gender + education + condition + first_condition + GAAIS + source_agreement, data = df_analysis)
summary(anova_result)

# ANOVA test of Integrity (trust)
anova_result <- aov(Integrity ~ topic + age + gender + education + condition + first_condition + GAAIS + source_agreement, data = df_analysis)
summary(anova_result)

# ANOVA test of Competence (trust)
anova_result <- aov(Competence ~ topic + age + gender + education + condition + first_condition + GAAIS + source_agreement, data = df_analysis)
summary(anova_result)

# ANOVA test of Confidence (task level)
anova_result <- aov(confidence ~ topic + google_checked + condition + first_condition + GAAIS + source_agreement, data = df_analysis)
summary(anova_result)

# ANOVA test of which primary source the user chose
aov_result <- aov(primary_source ~ topic + google_checked + condition + first_condition + GAAIS + source_agreement,
                  data = df_analysis)
summary(aov_result)

# We're not sure about this test - we set it up with Naja, but does not seem to be usable right now
BenevolenceModel <- lmer(Benevolence ~ confidence + 
                          condition + 
                          first_condition +
                          GAAIS + 
                          source_agreement +
                          Integrity +
                          Competence +
                          (1 | participant), data = df_analysis)

summary(BenevolenceModel)