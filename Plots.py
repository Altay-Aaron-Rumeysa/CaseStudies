import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

# Simulate representative data for plotting
np.random.seed(42)
n = 1000

# Simulated SES variables
EDUCP_A = np.random.randint(1, 5, size=n)
POVRATTC_A = np.random.normal(loc=2, scale=0.5, size=n)

# Simulated Objective Health indicators
DIBEV_A = np.random.binomial(1, 0.1, size=n)
HYPEV_A = np.random.binomial(1, 0.2, size=n)
ObjectiveHealth = 0.48 * DIBEV_A + 0.61 * HYPEV_A + np.random.normal(0, 0.5, size=n)

# Simulated Perceived Health indicators
PHSTAT_A = np.random.randint(1, 6, size=n)
PHQCAT_A = np.random.randint(0, 4, size=n)
LSATIS4_A = np.random.randint(1, 5, size=n)
PerceivedHealth = (
    -0.48 * ObjectiveHealth +
    -0.28 * POVRATTC_A +
    0.01 * EDUCP_A +
    np.random.normal(0, 0.5, size=n)
)

# Create DataFrame
df = pd.DataFrame({
    "EDUCP_A": EDUCP_A,
    "POVRATTC_A": POVRATTC_A,
    "ObjectiveHealth": ObjectiveHealth,
    "PerceivedHealth": PerceivedHealth,
    "PHSTAT_A": PHSTAT_A,
    "PHQCAT_A": PHQCAT_A,
    "LSATIS4_A": LSATIS4_A
})

# Plot 1: SES vs Objective Health with regression lines by education level
sns.lmplot(
    data=df,
    x="POVRATTC_A",
    y="ObjectiveHealth",
    hue="EDUCP_A",
    palette="viridis",
    aspect=1.5,
    scatter_kws={'alpha': 0.3}
)
plt.title("SES Indicators and Objective Health with Regression Lines")
plt.xlabel("Poverty Ratio (POVRATTC_A)")
plt.ylabel("Objective Health (latent)")
plt.tight_layout()
plt.show()

# Plot 2: Objective Health predicting Perceived Health
plt.figure(figsize=(8, 5))
sns.regplot(
    data=df,
    x="ObjectiveHealth",
    y="PerceivedHealth",
    scatter_kws={'alpha': 0.3}
)
plt.title("Objective Health Predicting Perceived Health")
plt.xlabel("Objective Health")
plt.ylabel("Perceived Health")
plt.tight_layout()
plt.show()

# Plot 3: SES vs Perceived Health with regression lines by education level
sns.lmplot(
    data=df,
    x="POVRATTC_A",
    y="PerceivedHealth",
    hue="EDUCP_A",
    palette="coolwarm",
    aspect=1.5,
    scatter_kws={'alpha': 0.3}
)
plt.title("SES Indicators and Perceived Health with Regression Lines")
plt.xlabel("Poverty Ratio (POVRATTC_A)")
plt.ylabel("Perceived Health (latent)")
plt.tight_layout()
plt.show()