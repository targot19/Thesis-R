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

# Friedman test on Competence
competence_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_competence = mean(Competence), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_competence)

friedman.test(as.matrix(competence_summary[,-1]))  # Exclude participant column

# Step 1: Aggregate for plotting
plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(Competence),  # Change to any trust measure
    se = sd(Competence) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Step 2: Plot
ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean Competence Score by Condition",
    x = "Condition",
    y = "Mean Competence Score"
  ) +
  theme_minimal()

# Friedman test on Integrity
integrity_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_integrity = mean(Integrity), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_integrity)

friedman.test(as.matrix(integrity_summary[,-1]))  # Exclude participant column

# Step 1: Aggregate for plotting
plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(Integrity),  # Change to any trust measure
    se = sd(Integrity) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Step 2: Plot
ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean Integrity Score by Condition",
    x = "Condition",
    y = "Mean Integrity Score"
  ) +
  theme_minimal()

# Friedman test on Benevolence
benevolence_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_benevolence = mean(Benevolence), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_benevolence)

friedman.test(as.matrix(benevolence_summary[,-1]))  # Exclude participant column

# Step 1: Aggregate for plotting
plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(Benevolence),  # Change to any trust measure
    se = sd(Benevolence) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Step 2: Plot
ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean Benevolence Score by Condition",
    x = "Condition",
    y = "Mean Benevolence Score"
  ) +
  theme_minimal()

# Friedman test on asked_chat
asked_chat_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_asked_chat = mean(asked_chat), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_asked_chat)

friedman.test(as.matrix(asked_chat_summary[,-1]))  # Exclude participant column

# Step 1: Aggregate for plotting
plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(asked_chat),  # Change to any trust measure
    se = sd(asked_chat) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Step 2: Plot
ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean asked_chat Score by Condition",
    x = "Condition",
    y = "Mean asked_chat Score"
  ) +
  theme_minimal()

# Friedman test on google_checked
google_checked_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_google_checked = mean(google_checked), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_google_checked)

friedman.test(as.matrix(google_checked_summary[,-1]))  # Exclude participant column

# Step 1: Aggregate for plotting
plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(google_checked),  # Change to any trust measure
    se = sd(google_checked) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Step 2: Plot
ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean google_checked Score by Condition",
    x = "Condition",
    y = "Mean google_checked Score"
  ) +
  theme_minimal()

# Friedman test on primary_source
primary_source_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_primary_source = mean(primary_source), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_primary_source)

friedman.test(as.matrix(primary_source_summary[,-1]))  # Exclude participant column

# Step 1: Aggregate for plotting
plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(primary_source),  # Change to any trust measure
    se = sd(primary_source) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

# Step 2: Plot
ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean primary_source Score by Condition",
    x = "Condition",
    y = "Mean primary_source Score"
  ) +
  theme_minimal()

ggplot(df_analysis, aes(x = condition_label, y = Competence)) +  # Replace with your measure
  geom_violin(fill = "#E69F00", alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(
    title = "Distribution of Competence Scores by Condition",
    x = "Condition",
    y = "Competence"
  ) +
  theme_minimal()

df_analysis %>%
  group_by(condition_label) %>%
  summarise(p_value = shapiro.test(Competence)$p.value)

# Example linear mixed model with GAAIS and condition
model_competence <- lmer(Competence ~ condition_label * GAAIS + (1|participant), data = df_analysis)
summary(model_competence)

# For Integrity
model_integrity <- lmer(Integrity ~ condition_label * GAAIS + (1|participant), data = df_analysis)
summary(model_integrity)

# For Benevolence
model_benevolence <- lmer(Benevolence ~ condition_label * GAAIS + (1|participant), data = df_analysis)
summary(model_benevolence)

# Plot model predictions
# Plot model predictions
plot(ggpredict(model_competence, terms = c("condition_label", "GAAIS")))
plot(ggpredict(model_integrity, terms = c("condition_label", "GAAIS")))

# Friedman test on source_agreement
primary_source_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_source_agreement = mean(source_agreement), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_source_agreement)

friedman.test(as.matrix(primary_source_summary[,-1]))  # Exclude participant column

# Friedman test on first_condition
first_condition_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_first_condition = mean(first_condition), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_first_condition)

friedman.test(as.matrix(first_condition_summary[,-1]))  # Exclude participant column

# Friedman test on GAAIS
GAAIS_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_GAAIS = mean(GAAIS), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_GAAIS)

friedman.test(as.matrix(GAAIS_summary[,-1]))  # Exclude participant column

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