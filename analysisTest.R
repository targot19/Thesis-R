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

# Dunno about this one
df_analysis %>%
  group_by(condition_label) %>%
  summarise(p_value = shapiro.test(Competence)$p.value)

# Create a function to plot histogram + QQ plot for a variable
plot_normality <- function(variable, var_name) {
  par(mfrow = c(1, 2))  # Two plots side by side
  
  # Histogram with density curve
  hist(variable, breaks = 20, main = paste("Histogram of", var_name),
       xlab = var_name, probability = TRUE, col = "lightblue", border = "white")
  lines(density(variable), col = "darkblue", lwd = 2)
  
  # Q-Q plot
  qqnorm(variable, main = paste("Q-Q Plot of", var_name))
  qqline(variable, col = "red", lwd = 2)
  
  par(mfrow = c(1, 1))  # Reset plot layout
}

plot_normality(df_analysis$confidence, "Confidence")
plot_normality(df_analysis$Benevolence, "Benevolence")
plot_normality(df_analysis$Integrity, "Integrity")
plot_normality(df_analysis$Competence, "Competence")
plot_normality(df_analysis$GAAIS, "GAAIS")



# Friedman test on Competence
competence_summary <- df_analysis %>%
  group_by(participant, condition_label) %>%
  summarise(mean_competence = mean(Competence), .groups = "drop") %>%
  pivot_wider(names_from = condition_label, values_from = mean_competence)

friedman.test(as.matrix(competence_summary[,-1]))

plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(Competence),  # Change to any trust measure
    se = sd(Competence) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

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

friedman.test(as.matrix(integrity_summary[,-1]))

plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(Integrity),  # Change to any trust measure
    se = sd(Integrity) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

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

friedman.test(as.matrix(benevolence_summary[,-1]))

plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(Benevolence),  # Change to any trust measure
    se = sd(Benevolence) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

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

friedman.test(as.matrix(asked_chat_summary[,-1]))

plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(asked_chat),  # Change to any trust measure
    se = sd(asked_chat) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

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

friedman.test(as.matrix(google_checked_summary[,-1]))

plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(google_checked),  # Change to any trust measure
    se = sd(google_checked) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

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

friedman.test(as.matrix(primary_source_summary[,-1]))

plot_data <- df_analysis %>%
  group_by(condition_label) %>%
  summarise(
    mean_trust = mean(primary_source),  # Change to any trust measure
    se = sd(primary_source) / sqrt(n()),  # Standard Error
    .groups = "drop"
  )

ggplot(plot_data, aes(x = condition_label, y = mean_trust)) +
  geom_bar(stat = "identity", fill = "#69b3a2", width = 0.6) +
  geom_errorbar(aes(ymin = mean_trust - se, ymax = mean_trust + se), width = 0.2) +
  labs(
    title = "Mean primary_source Score by Condition",
    x = "Condition",
    y = "Mean primary_source Score"
  ) +
  theme_minimal()


# Some weird violin plot...
ggplot(df_analysis, aes(x = condition_label, y = Competence)) +
  geom_violin(fill = "#E69F00", alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(
    title = "Distribution of Competence Scores by Condition",
    x = "Condition",
    y = "Competence"
  ) +
  theme_minimal()


# Example linear mixed model with GAAIS and condition
# For Competence
model_competence <- lmer(Competence ~ condition_label * GAAIS + (1|participant), data = df_analysis)
summary(model_competence)

# For Integrity
model_integrity <- lmer(Integrity ~ condition_label * GAAIS + (1|participant), data = df_analysis)
summary(model_integrity)

# For Benevolence
model_benevolence <- lmer(Benevolence ~ condition_label * GAAIS + (1|participant), data = df_analysis)
summary(model_benevolence)

# Plot model predictions that we don't understand
plot(ggpredict(model_competence, terms = c("condition_label", "GAAIS")))
plot(ggpredict(model_integrity, terms = c("condition_label", "GAAIS")))
plot(ggpredict(model_benevolence, terms = c("condition_label", "GAAIS")))


# Check confidence when primary source is the chatbot
chatbot_confidence <- subset(df_analysis, primary_source == 0)
nrow(chatbot_confidence)
shapiro.test(chatbot_confidence$confidence)  # check normality
wilcox.test(chatbot_confidence$confidence, mu = 50)  # non-parametric alternative


# Create labeled subsets
chatbot_confidence <- df_analysis %>% 
  filter(primary_source == 0) %>% 
  mutate(Source = "Chatbot")
mean(chatbot_confidence$confidence)

google_confidence <- df_analysis %>% 
  filter(primary_source == 1) %>% 
  mutate(Source = "Google")
mean(google_confidence$confidence)

prior_confidence <- df_analysis %>% 
  filter(primary_source == 2) %>% 
  mutate(Source = "Prior Knowledge")
mean(prior_confidence$confidence)

# Combine into one data frame
combined_confidence <- bind_rows(chatbot_confidence, google_confidence, prior_confidence)

combined_confidence %>%
  group_by(Source) %>%
  summarise(Median_Confidence = median(confidence))

# Plot on confidence scores
ggplot(combined_confidence, aes(x = Source, y = confidence, fill = Source)) +
  geom_boxplot(outlier.color = "red", alpha = 0.6) +
  geom_hline(yintercept = 50, linetype = "dashed", color = "darkred", size = 1) +
  labs(title = "Confidence Scores by Primary Source",
       y = "Confidence Score",
       x = "Primary Source") +
  theme_minimal() +
  theme(legend.position = "none")

kruskal.test(confidence ~ Source, data = combined_confidence)
pairwise.wilcox.test(combined_confidence$confidence, combined_confidence$Source,
                     p.adjust.method = "bonferroni")


# Dunno about this...
ggplot(chatbot_confidence, aes(x = factor(1), y = confidence)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red") +
  geom_hline(yintercept = 50, linetype = "dashed", color = "darkred", size = 1) +
  labs(x = "Chatbot Users", y = "Confidence Score",
       title = "Confidence Scores of Chatbot Users with Reference Line at 50") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

wilcox.test(confidence ~ primary_source, data = df_analysis)


# NOT SIGNIFICANT - General confidence scores across all conditions
kruskal.test(confidence ~ condition_label, data = df_analysis)
pairwise.wilcox.test(df_analysis$confidence, df_analysis$condition_label,
                     p.adjust.method = "bonferroni")


# NA on these tests in p-value

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


# ANOVA tests that I think we should not run since our data is not normally distributed

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


# ====== GAAIS affect on Trust overall (with one C, I, B score pr. participant)
# Summarise trust per participant and keep GAAIS
trust_with_factors <- df_analysis %>%
  group_by(participant) %>%
  summarise(
    competence_avg = mean(Competence, na.rm = TRUE),
    integrity_avg = mean(Integrity, na.rm = TRUE),
    benevolence_avg = mean(Benevolence, na.rm = TRUE),
    GAAIS = first(GAAIS),  # keep original name
    comfort = first(comfort),
    ai_usage = first(ai_usage)
  )

# Spearman correlation between GAAIS and each trust dimension
cor.test(trust_with_factors$GAAIS, trust_with_gaais$competence_avg, method = "spearman")
cor.test(trust_with_factors$GAAIS, trust_with_gaais$integrity_avg, method = "spearman")
cor.test(trust_with_factors$GAAIS, trust_with_gaais$benevolence_avg, method = "spearman")

# == Plot:
# Everything in 1 plot:
trust_long <- trust_with_gaais %>%
  pivot_longer(cols = c(competence_avg, integrity_avg, benevolence_avg),
               names_to = "trust_type",
               values_to = "trust_score")

ggplot(trust_long, aes(x = GAAIS, y = trust_score, color = trust_type)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "GAAIS vs Trust Ratings",
       x = "GAAIS Score",
       y = "Average Trust Rating",
       color = "Trust Dimension") +
  theme_minimal()

# 3 plots:
ggplot(trust_with_gaais, aes(x = GAAIS, y = competence_avg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "GAAIS vs Competence",
       x = "GAAIS Score",
       y = "Average Competence Rating") +
  theme_minimal()

ggplot(trust_with_gaais, aes(x = GAAIS, y = integrity_avg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "darkgreen") +
  labs(title = "GAAIS vs Integrity",
       x = "GAAIS Score",
       y = "Average Integrity Rating") +
  theme_minimal()

ggplot(trust_with_gaais, aes(x = GAAIS, y = benevolence_avg)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "GAAIS vs Benevolence",
       x = "GAAIS Score",
       y = "Average Benevolence Rating") +
  theme_minimal()


# ===== Look at correlation between comfort with digital technology + AI usage and overall Trust
trust_with_factors <- df_analysis %>%
  group_by(participant) %>%
  summarise(
    competence_avg = mean(Competence, na.rm = TRUE),
    integrity_avg = mean(Integrity, na.rm = TRUE),
    benevolence_avg = mean(Benevolence, na.rm = TRUE),
    GAAIS = first(GAAIS),  # keep original name
    comfort = first(comfort),
    ai_usage = first(ai_usage)
  )

# Competence by comfort
kruskal.test(competence_avg ~ comfort, data = trust_with_factors)

# Competence by AI usage
kruskal.test(competence_avg ~ ai_usage, data = trust_with_factors)

# Integrity by comfort
kruskal.test(integrity_avg ~ comfort, data = trust_with_factors)

# Integrity by AI usage
kruskal.test(integrity_avg ~ ai_usage, data = trust_with_factors)

# Benevolence by comfort
kruskal.test(benevolence_avg ~ comfort, data = trust_with_factors)

# Benevolence by AI usage
kruskal.test(benevolence_avg ~ ai_usage, data = trust_with_factors)


# ===== Chart over division of sources chosen for each condition

df_analysis <- df_analysis %>%
  mutate(primary_source_label = case_when(
    primary_source == 0 ~ "Chatbot",
    primary_source == 1 ~ "Google",
    primary_source == 2 ~ "Prior Knowledge",
    TRUE ~ "Other"
  ))


source_counts <- df_analysis %>%
  group_by(condition_label, primary_source_label) %>%
  summarise(count = n(), .groups = "drop")


ggplot(source_counts, aes(x = condition_label, y = count, fill = primary_source_label)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Primary Source Used by Condition",
       x = "Condition",
       y = "Count",
       fill = "Primary Source") +
  theme_minimal()

ggplot(source_counts, aes(x = "", y = count, fill = primary_source_label)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ condition_label) +
  labs(title = "Primary Source Usage by Condition",
       fill = "Primary Source") +
  theme_void()
