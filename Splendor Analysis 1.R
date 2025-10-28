# ============================================
# INSURANCE POLICIES ANALYSIS PROJECT
# ============================================

# Load essential libraries
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggrepel)
library(nnet)
library(cluster)
library(RColorBrewer)

# --------------------------------------------
# Load and Prepare Data
# --------------------------------------------
Insurance_Policies <- read_excel("data/Insurance Policies Project.xlsx", 
                                 sheet = "Insurance Policies Project") %>%
  na.omit() %>%
  mutate(across(c(MaritalStatus, CarUse, Gender, Parent, Education, 
                  CarMake, CarModel, CarColor, CoverageZone), as.factor))

str(Insurance_Policies)
summary(Insurance_Policies)

# --------------------------------------------
# 1. CLAIM ANALYSIS BY DEMOGRAPHICS
# --------------------------------------------
Claims_by_Demographics <- Insurance_Policies %>%
  group_by(Age, Gender, MaritalStatus) %>%
  summarise(
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    AvgClaimAmount = mean(ClaimAmount, na.rm = TRUE),
    .groups = 'drop'
  )

View(Claims_by_Demographics)

# Claim Frequency by Car Features
claims_by_car_summary <- Insurance_Policies %>%
  group_by(CarMake, CarModel, CarUse, CarYear) %>%
  summarise(
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    AvgClaimAmount = mean(ClaimAmount, na.rm = TRUE),
    .groups = 'drop'
  )

View(claims_by_car_summary)

# --------------------------------------------
# 2. RISK ASSESSMENT MODEL
# --------------------------------------------
threshold <- 2
Insurance_Policies <- Insurance_Policies %>%
  mutate(RiskProfile = ifelse(ClaimFreq > threshold, 1, 0))

reg_model <- glm(RiskProfile ~ HouseholdIncome + Education + CoverageZone,
                 data = Insurance_Policies, family = binomial)
summary(reg_model)

# --------------------------------------------
# DEMOGRAPHIC SUMMARY VISUALS
# --------------------------------------------
Insurance_Policies <- Insurance_Policies %>%
  mutate(AgeGroup = cut(Age, 
                        breaks = c(-Inf, 25, 35, 45, 55, 65, Inf),
                        labels = c("<25", "25-34", "35-44", "45-54", "55-64", "65+")))

summary_statistics <- Insurance_Policies %>%
  group_by(AgeGroup, Gender, MaritalStatus, CarUse, Education, CoverageZone) %>%
  summarise(
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    AvgClaimAmount = mean(ClaimAmount, na.rm = TRUE),
    .groups = 'drop'
  )

# Visualization: Average Claim Frequency by Age Group
ggplot(summary_statistics, aes(x = AgeGroup, y = AvgClaimFreq)) +
  geom_bar(stat = "identity", fill = "#69b3a2", color = "black") +
  labs(title = "Average Claim Frequency by Age Group",
       x = "Age Group", y = "Average Claim Frequency") +
  theme_minimal(base_size = 14)

# --------------------------------------------
# 3. PREMIUM VS RISK ANALYSIS
# --------------------------------------------
Insurance_Policies <- Insurance_Policies %>%
  mutate(
    RiskCategory = case_when(
      ClaimFreq <= 10 & ClaimAmount <= 5000 ~ "Low Risk",
      ClaimFreq > 10 & ClaimAmount > 5000 ~ "High Risk",
      TRUE ~ "Medium Risk"
    )
  )

premium_analysis <- Insurance_Policies %>%
  group_by(RiskCategory, CoverageZone) %>%
  summarise(
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    AvgClaimAmount = mean(ClaimAmount, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(premium_analysis, aes(x = RiskCategory, y = AvgClaimFreq, fill = CoverageZone)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Average Claim Frequency by Risk and Zone",
       x = "Risk Category", y = "Average Claim Frequency") +
  theme_minimal(base_size = 14)

# --------------------------------------------
# 4. CUSTOMER SEGMENTATION & MARKETING
# --------------------------------------------
# K-Means Clustering
kmeans_data <- Insurance_Policies %>%
  select(Age, HouseholdIncome, ClaimFreq) %>%
  scale() %>%
  na.omit()

set.seed(123)
kmeans_result <- kmeans(kmeans_data, centers = 3, nstart = 20)
Insurance_Policies$Cluster <- kmeans_result$cluster

cluster_summary <- Insurance_Policies %>%
  group_by(Cluster) %>%
  summarise(
    AvgAge = mean(Age, na.rm = TRUE),
    AvgIncome = mean(HouseholdIncome, na.rm = TRUE),
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    .groups = 'drop'
  )

print(cluster_summary)

# Visualization: Cluster Segmentation
ggplot(Insurance_Policies, aes(x = Age, y = HouseholdIncome, color = factor(Cluster))) +
  geom_point(alpha = 0.6) +
  labs(title = "Customer Segmentation: Age vs Household Income",
       x = "Age", y = "Household Income", color = "Cluster") +
  theme_minimal(base_size = 14)

# --------------------------------------------
# 5. POLICYHOLDERS WITH CHILDREN DRIVING
# --------------------------------------------
children_driving <- Insurance_Policies %>%
  mutate(HasChildrenDriving = ifelse(KidsDriving > 0, "Yes", "No")) %>%
  group_by(HasChildrenDriving) %>%
  summarise(
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    AvgClaimAmount = mean(ClaimAmount, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(children_driving, aes(x = HasChildrenDriving, y = AvgClaimFreq, fill = HasChildrenDriving)) +
  geom_bar(stat = "identity") +
  labs(title = "Claim Frequency by Children Driving", x = "Children Driving", y = "Average Claim Frequency") +
  theme_minimal(base_size = 14)

# --------------------------------------------
# 6. REGIONAL CLAIM TRENDS
# --------------------------------------------
regional_summary <- Insurance_Policies %>%
  group_by(CoverageZone) %>%
  summarise(
    AvgClaimFreq = mean(ClaimFreq, na.rm = TRUE),
    AvgClaimAmount = mean(ClaimAmount, na.rm = TRUE),
    .groups = 'drop'
  )

ggplot(regional_summary, aes(x = CoverageZone, y = AvgClaimAmount, fill = CoverageZone)) +
  geom_col() +
  coord_flip() +
  labs(title = "Average Claim Amount by Coverage Zone",
       x = "Coverage Zone", y = "Average Claim Amount") +
  theme_minimal(base_size = 14)
