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
# Plots
# -----------------------------------------
nhis_data <- nhis_data %>%
  mutate(
    # Reverse PHSTAT_A so that higher values = better health
    PHSTAT_REVERSED = max(PHSTAT_A, na.rm = TRUE) + 1 - PHSTAT_A,
    
    # Create standardized SES composite
    SES_SCORE = scale(EDUCP_A) + scale(POVRATTC_A),
    
    # SES grouping (tertiles)
    SES_GROUP = case_when(
      SES_SCORE < quantile(SES_SCORE, 0.33, na.rm = TRUE) ~ "Low SES",
      SES_SCORE > quantile(SES_SCORE, 0.66, na.rm = TRUE) ~ "High SES",
      TRUE ~ "Mid SES"
    ),
    
    # Insurance label
    INS_LABEL = ifelse(HICOV_A == 1, "Uninsured", "Insured")
  )

plot_data <- nhis_data %>%
  group_by(SES_GROUP, INS_LABEL) %>%
  summarise(
    mean_health = mean(PHSTAT_REVERSED, na.rm = TRUE),
    se_health = sd(PHSTAT_REVERSED, na.rm = TRUE) / sqrt(n())
  ) %>%
  ungroup()

# Ensure factor levels are ordered
plot_data$INS_LABEL <- factor(plot_data$INS_LABEL, levels = c("Uninsured", "Insured"))
plot_data$SES_GROUP <- factor(plot_data$SES_GROUP, levels = c("Low SES", "Mid SES", "High SES"))

# Create the plot
ggplot(plot_data, aes(x = SES_GROUP, y = mean_health, group = INS_LABEL, color = INS_LABEL)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_health - se_health, ymax = mean_health + se_health), width = 0.2) +
  labs(
    title = "Interaction of SES and Insurance on Perceived Health",
    x = "Socioeconomic Status Group",
    y = "Mean Perceived Health (Higher = Better)",
    color = "Insurance Status"
  ) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = c("Insured" = "#1f78b4", "Uninsured" = "#e31a1c"))




