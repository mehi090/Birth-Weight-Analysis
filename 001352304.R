# Load necessary libraries
if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
if (!require(gmodels)) install.packages("gmodels", dependencies=TRUE)
if (!require(gridExtra)) install.packages("gridExtra", dependencies=TRUE)
if (!require(patchwork)) install.packages("patchwork", dependencies=TRUE)
# Load necessary libraries
if (!require(tidyverse)) install.packages("tidyverse", dependencies=TRUE)
if (!require(knitr)) install.packages("knitr", dependencies=TRUE)
if (!require(kableExtra)) install.packages("kableExtra", dependencies=TRUE)

library(tidyverse)   # Includes dplyr, ggplot2, readr, and more
library(knitr)       # For neat table formatting
library(kableExtra)  
library(tidyverse)   # Includes dplyr, ggplot2, readr, and more
library(gmodels)     # For contingency tables
library(gridExtra)   # For multiple plots
library(patchwork)   # For arranging plots

# SECTION 1: DATA LOADING & PREPROCESSING ------------------------------

# Load dataset dynamically (Avoid hardcoded file paths)
file_name <- file.choose()  # Manually select the dataset file
birthwt_data <- read_csv(file_name)

# Display dataset structure
glimpse(birthwt_data)

# Convert categorical variables to factors
birthwt_data <- birthwt_data %>%
  mutate(across(c(factorA, factorC, hypertension, uterine.irr, birthwt.below.2500), as.factor))

# Summarize key statistics (mean, median, standard deviation) for all numerical variables
numerical_summary <- birthwt_data %>%
  summarise(across(where(is.numeric), 
                   list(Mean = mean, Median = median, Std_Dev = sd), 
                   na.rm = TRUE))

# Convert summary to a data frame for better display
numerical_summary_df <- as.data.frame(t(numerical_summary))  # Transpose for readability
colnames(numerical_summary_df) <- c("Value")  # Rename column

# Display the formatted numerical summary table
cat("\nSummary Statistics of Numerical Variables:\n")
print(numerical_summary_df)

# SECTION 2: COMPARING MEANS --------------------------------------------

# Define a reusable theme
plot_theme <- theme_minimal()

# Boxplot: Birth Weight vs. Smoking Status
ggplot(birthwt_data, aes(x = factorC, y = birthwt.grams, fill = factorC)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Birth Weight Comparison: Smoking vs. Non-Smoking Mothers",
       x = "Mother Smokes (Yes/No)", y = "Birth Weight (grams)") +
  scale_fill_manual(values = c("lightblue", "pink")) +
  plot_theme

# Summary statistics for smoking vs. non-smoking mothers
summary_table <- birthwt_data %>%
  group_by(factorC) %>%
  summarise(Mean_BirthWeight = mean(birthwt.grams, na.rm = TRUE),
            SD_BirthWeight = sd(birthwt.grams, na.rm = TRUE),
            SE_BirthWeight = SD_BirthWeight / sqrt(n()))

print(summary_table)

# Independent Two-Sample T-Test
t_test_result <- t.test(birthwt.grams ~ factorC, data = birthwt_data, var.equal = FALSE)
print(t_test_result)

# SECTION 3: NORMALITY TESTING & WILCOXON TEST --------------------------

# QQ-Plot & Histogram for Normality Check
qq_plot <- ggplot(birthwt_data, aes(sample = birthwt.grams)) +
  geom_qq() +
  geom_qq_line(color = "red") +
  labs(title = "QQ-Plot for Birth Weight", x = "Theoretical Quantiles", y = "Sample Quantiles") +
  plot_theme

hist_plot <- ggplot(birthwt_data, aes(x = birthwt.grams)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "blue", alpha = 0.5, color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Birth Weight", x = "Birth Weight (grams)", y = "Density") +
  plot_theme

# Display both plots together
qq_plot + hist_plot

# Wilcoxon Rank-Sum Test (Non-parametric alternative to t-test)
wilcox_result <- wilcox.test(birthwt.grams ~ factorC, data = birthwt_data, exact = FALSE)
print(wilcox_result)

# Compare Wilcoxon vs. T-Test
cat("\nT-Test p-value:", t_test_result$p.value)
cat("\nWilcoxon Test p-value:", wilcox_result$p.value)

# SECTION 4: HYPOTHESIS TESTING FOR CATEGORICAL DATA --------------------

# 2×2 Contingency Table: Smoking vs. Low Birth Weight
contingency_table <- table(birthwt_data$birthwt.below.2500, birthwt_data$factorC)
colnames(contingency_table) <- c("Non-Smoker", "Smoker")
rownames(contingency_table) <- c("Normal Birth Weight", "Low Birth Weight")

# Display the contingency table
cat("\n2×2 Contingency Table: Low Birth Weight vs. Smoking Status\n")
print(contingency_table)

# Fisher’s Exact Test & Chi-Squared Test
fisher_test <- fisher.test(contingency_table)
chi_sq_test <- chisq.test(contingency_table)

print(fisher_test)
print(chi_sq_test)

# SECTION 5: SIMULATION & P-VALUE ANALYSIS ------------------------------

# Simulate Data with No Treatment Effect
set.seed(123)
control_group <- rnorm(30, mean = 50, sd = 10)
treatment_group <- rnorm(30, mean = 50, sd = 10)  # Same mean as control

t_test_no_effect <- t.test(control_group, treatment_group)
print(t_test_no_effect)

# Simulate 10,000 Datasets with Moderate Effect
set.seed(1234)
num_simulations <- 10000
p_values <- replicate(num_simulations, {
  control <- rnorm(30, mean = 50, sd = 10)
  treatment <- rnorm(30, mean = 55, sd = 10)
  t.test(control, treatment)$p.value
})

# Plot Histogram of P-values
ggplot(data.frame(p_values), aes(x = p_values)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.5, color = "black") +
  labs(title = "Histogram of P-values from 10,000 Simulations", x = "P-value", y = "Frequency") +
  plot_theme

# Calculate Statistical Power
power <- mean(p_values < 0.05)
cat("\nProportion of significant results (p < 0.05):", power)

# BONUS QUESTION: SMALL SAMPLE ANALYSIS ---------------------------------

# Select a Small Sample (30 Observations)
set.seed(42)
sample_size <- 30
small_sample <- birthwt_data %>% sample_n(sample_size)

# Perform T-Test & Wilcoxon Test on Small Sample
t_test_small <- t.test(birthwt.grams ~ factorC, data = small_sample, var.equal = FALSE)
wilcox_test_small <- wilcox.test(birthwt.grams ~ factorC, data = small_sample, exact = FALSE)

# Print Results
print(t_test_small)
print(wilcox_test_small)

# Compare Small Sample p-values
cat("\nT-Test p-value (Small Sample):", t_test_small$p.value)
cat("\nWilcoxon Test p-value (Small Sample):", wilcox_test_small$p.value)


