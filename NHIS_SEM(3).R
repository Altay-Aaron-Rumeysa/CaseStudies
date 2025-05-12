# -----------------------------------------
# Load Required Libraries
# -----------------------------------------
install.packages("lavaan")
install.packages("readr")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("psych")
install.packages("GGally")
install.packages("semPlot")

library(semPlot)
library(lavaan)   # For SEM modeling
library(readr)    # For reading CSV files
library(dplyr)    # For data manipulation
library(ggplot2)  # For plotting
library(psych)    # For descriptive statistics
library(GGally)   # For correlation plots

# -----------------------------------------
# Load NHIS 2022 Adult Dataset
# -----------------------------------------
nhis_data <- read_delim("C:/Users/Altay/Downloads/adult22.csv", delim = ";")

# Inspect the first few rows (optional)
head(nhis_data)

# -----------------------------------------
# Create SES and interaction variables
# -----------------------------------------
nhis_data <- nhis_data %>%
  mutate(
    # Binary insurance: 1 = insured, 0 = not insured
    INS_BIN = ifelse(HICOV_A == 1, 1, 0),
    
    # SES composite score (standardized components)
    SES_SCORE = scale(EDUCP_A) + scale(POVRATTC_A),
    
    # Interaction term: SES × insurance status
    SESxINS = SES_SCORE * INS_BIN
  )


# Create SES category: Low vs High based on median split (important table)
nhis_data <- nhis_data %>%
  mutate(SES_cat = ifelse(SES_SCORE >= median(SES_SCORE, na.rm = TRUE), "High SES", "Low SES"))

table(nhis_data$SES_cat, nhis_data$INS_BIN)


# -----------------------------------------
# Model 1: Do people "perform health"?
# -----------------------------------------
model_perform_health <- '
  ObjectiveHealth =~ DIBEV_A + HYPEV_A
  PerceivedHealth =~ PHSTAT_A + PHQCAT_A + LSATIS4_A

  PerceivedHealth ~ EDUCP_A + POVRATTC_A
  ObjectiveHealth ~ EDUCP_A + POVRATTC_A
  PerceivedHealth ~ ObjectiveHealth
'

fit_perform_health <- sem(model_perform_health, data = nhis_data, missing = "fiml")
summary(fit_perform_health, standardized = TRUE, fit.measures = TRUE)

# Plot Model 1 (Performing Health)
semPaths(
  fit_perform_health,
  what = "std",              # Standardized coefficients
  layout = "tree",            # Layout style
  edge.label.cex = 0.8,       # Size of labels
  sizeMan = 5,                # Size of observed variables
  sizeLat = 7,                # Size of latent variables
  nCharNodes = 0,             # Show full variable names
  residuals = TRUE,           # Show error terms
  intercepts = FALSE          # Hide intercepts for clarity
)


# -----------------------------------------
# Model 2: Does SES protect against the negative health effects of being uninsured?
# -----------------------------------------
model_ses_insurance <- '
  PHSTAT_A ~ SES_SCORE + INS_BIN + SESxINS
'

fit_ses_insurance <- sem(model_ses_insurance, data = nhis_data, missing = "fiml")
summary(fit_ses_insurance, standardized = TRUE, fit.measures = TRUE)

# Plot Model 2 (SES buffers insurance effect)
semPaths(
  fit_ses_insurance,
  what = "std",
  layout = "circle",          # Circle layout (simple for small models)
  edge.label.cex = 1.0,
  sizeMan = 6,
  sizeLat = 8,
  nCharNodes = 0,
  residuals = TRUE,
  intercepts = FALSE
)


# -----------------------------------------
# Descriptive Stats and Plots for Model 1 Variables
# -----------------------------------------
model1_vars <- nhis_data %>%
  select(DIBEV_A, HYPEV_A, PHSTAT_A, PHQCAT_A, LSATIS4_A, EDUCP_A, POVRATTC_A)

# Summary
summary(model1_vars)
describe(model1_vars)

# Histograms
model1_vars %>%
  tidyr::gather(variable, value) %>%
  ggplot(aes(x = value)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme_minimal()

# Correlation matrix for perceived health indicators
model1_vars %>%
  select(PHSTAT_A, PHQCAT_A, LSATIS4_A) %>%
  ggpairs()

# Boxplots: SES vs PHSTAT_A
nhis_data %>%
  ggplot(aes(x = as.factor(EDUCP_A), y = PHSTAT_A)) +
  geom_boxplot() +
  labs(x = "Education Level", y = "Self-Rated Health") +
  theme_minimal()

nhis_data %>%
  ggplot(aes(x = as.factor(POVRATTC_A), y = PHSTAT_A)) +
  geom_boxplot() +
  labs(x = "Poverty Ratio Category", y = "Self-Rated Health") +
  theme_minimal()

# -----------------------------------------
# Descriptive Stats and Plots for Model 3 Variables
# -----------------------------------------
model3_vars <- nhis_data %>%
  select(PHSTAT_A, INS_BIN, SES_SCORE)

# Summary
summary(model3_vars)
describe(model3_vars)

# Histogram: PHSTAT_A by Insurance Status
nhis_data %>%
  ggplot(aes(x = PHSTAT_A, fill = as.factor(INS_BIN))) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(fill = "Insurance Status", x = "Self-Rated Health", y = "Count") +
  theme_minimal()

# Scatterplot: SES_SCORE vs PHSTAT_A colored by Insurance
nhis_data %>%
  ggplot(aes(x = SES_SCORE, y = PHSTAT_A, color = as.factor(INS_BIN))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(color = "Insurance", x = "SES Score", y = "Self-Rated Health") +
  theme_minimal()

# Interaction plot manually (approximate)
nhis_data %>%
  mutate(SES_quartile = ntile(SES_SCORE, 4)) %>%
  group_by(SES_quartile, INS_BIN) %>%
  summarise(mean_health = mean(PHSTAT_A, na.rm = TRUE)) %>%
  ggplot(aes(x = SES_quartile, y = mean_health, color = as.factor(INS_BIN), group = INS_BIN)) +
  geom_line() +
  geom_point() +
  labs(x = "SES Quartile", y = "Mean Self-Rated Health", color = "Insurance Status") +
  theme_minimal()

