#Load Packages
library(ggplot2)
library(corrplot)
library(dplyr)
library(glmnet)
library(pROC)
library(caret)
library(car)     
library(broom)  

# Load data
health_data <- read.csv("health_dataset.csv")

#Data cleaning

#First we eliminate valid skips and non responses as well as age groups below 17 and above 65
health_data <- health_data[health_data$Fruit_veg_con != 9999.6 & health_data$Fruit_veg_con != 9999.9, ]
health_data <- health_data[(health_data$Age != 5 & health_data$Age !=1), ]

# Choosing variables
health_data <- health_data %>% 
  select(Gender, Age, Mental_health_state, BMI_18_above, Diabetic, Fatigue_syndrome, Anxiety_disorder, Stress_level, Sleep_apnea, Fruit_veg_con, Cannabies_use)

# Gender 1 for male 2 for female
# Age 2 - (18-34) 3 - (35-49) and 4 is (50-64)
# Mental health state: Poor (0), Fair (1), Good (2) very good (3) and excellent is (4) not answer is (9)
# BMI, Underweight/normal (1) overweight/obese (2)
# diabetes yes (1), no (2), don't know (7), refusal (8), not stated (9)
# Fatigue syndrome yes (1), no (2), don't know (7), refusal (8), not stated (9)
# Has anxiety disorder yes (1), no (2), don't know (7), refusal (8)
# Stress level : Not stressful at all (1), Not very stressful (2), A bit stressful (3), Quite a bit stressful (4),
# Extremely stressful (5), Don’t know (7), Refusal (8)
# Has sleep apnea yes (1), no (2), don't know (7), refusal (8)
# Fruit and vegetable consumption (0,300)
# Cannabis use in the last 12 months yes (1), no (2), don't know (7), refusal (8), not stated (9)

# removing more valid skips and non-responses
health_data_clean <- health_data %>%
  filter(
    Mental_health_state %in% c(0,1,2,3,4),
    Diabetic %in% c(1,2),
    Fatigue_syndrome %in% c(1,2),
    Anxiety_disorder %in% c(1,2),
    Stress_level %in% c(1,2,3,4,5),
    Sleep_apnea %in% c(1,2),
    Cannabies_use %in% c(1,2),
    Gender %in% c(1,2),
    Age %in% c(2,3,4),
    BMI_18_above %in% c(1,2),
    Fruit_veg_con >= 0 & Fruit_veg_con <= 300
  )

#Convert and label variables for logistic regression
health_data_clean <- health_data_clean %>%
  mutate(
    Fatigue_syndrome = factor(Fatigue_syndrome, levels = c(2,1), labels = c("No", "Yes")),
    Gender = factor(Gender, levels = c(1,2), labels = c("Male", "Female")),
    Age = factor(Age, levels = c(2,3,4), labels = c("18-34","35-49","50-64")),
    Mental_health_state = factor(Mental_health_state, 
                                 levels = c(0,1,2,3,4), 
                                 ordered = TRUE,
                                 labels = c("Poor","Fair","Good","Very Good","Excellent")),
    BMI_18_above = factor(BMI_18_above, levels = c(1,2), labels = c("Underweight/Normal","Overweight/Obese")),
    Diabetic = factor(Diabetic, levels = c(2,1), labels = c("No", "Yes")),
    Anxiety_disorder = factor(Anxiety_disorder, levels = c(2,1), labels = c("No", "Yes")),
    Stress_level = factor(Stress_level,
                          levels = c(1,2,3,4,5),
                          ordered = TRUE,
                          labels = c("Not stressful at all",
                                     "Not very stressful",
                                     "A bit stressful",
                                     "Quite a bit stressful",
                                     "Extremely stressful")),
    Sleep_apnea = factor(Sleep_apnea, levels = c(2,1), labels = c("No", "Yes")),
    Cannabies_use = factor(Cannabies_use, levels = c(2,1), labels = c("No", "Yes"))
  )


# Fit logistic regression model
logistic_model <- glm(Fatigue_syndrome ~ Age + Gender + BMI_18_above + Diabetic + Anxiety_disorder + Sleep_apnea + Stress_level + Mental_health_state + Fruit_veg_con + Cannabies_use,
                      data = health_data_clean,
                      family = binomial(link = "logit"))

# Summary of the model
summary(logistic_model)

# Exponentiate coefficients to obtain Odds Ratios
exp(coef(logistic_model))


#Computing AIC variable selection
step_model <- step(logistic_model, direction = "both", trace=FALSE)

# LASSO variable selection
x <- model.matrix(Fatigue_syndrome ~ . - 1, data = health_data_clean)
y <- health_data_clean$Fatigue_syndrome

cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
lasso_model <- glmnet(x, y, family = "binomial", alpha = 1, lambda = cv_lasso$lambda.min)
coef(lasso_model)


# Weighted logistic regression with selected variables

#Add weights: higher weight for rare class
health_data_clean$case_weight <- ifelse(health_data_clean$Fatigue_syndrome == "Yes", 10, 1)

# Fit weighted logistic regression
weighted_model <- glm(Fatigue_syndrome ~ Age + Gender + Anxiety_disorder + Sleep_apnea +
                        Stress_level + Mental_health_state + Fruit_veg_con + Cannabies_use,
                      data = health_data_clean,
                      weights = case_weight,
                      family = binomial)

summary(weighted_model)


# Predict probabilities from weighted model
predicted_probs <- predict(weighted_model, type = "response")
predicted_class <- ifelse(predicted_probs > 0.5, "Yes", "No")

# Create confusion matrix
confusionMatrix(factor(predicted_class, levels = c("No", "Yes")),
                factor(health_data_clean$Fatigue_syndrome, levels = c("No", "Yes")))

# ROC curve and AUC
roc_obj <- roc(health_data_clean$Fatigue_syndrome, predicted_probs)
plot(roc_obj, col = "blue", main = "ROC Curve for Weighted Logistic Regression")
auc(roc_obj)


# Get optimal threshold using Youden's J
best_coords <- coords(roc_obj, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity"))

# Apply optimal threshold
optimal_threshold <- best_coords["threshold"]
predicted_best <- ifelse(predicted_probs > as.numeric(optimal_threshold), "Yes", "No")
confusionMatrix(factor(predicted_best, levels = c("No", "Yes")),
                factor(health_data_clean$Fatigue_syndrome, levels = c("No", "Yes")))


# PLOTS

# AGE by CFS
p_age <- ggplot(health_data_clean, aes(x = Age, fill = Fatigue_syndrome)) +
  geom_bar(position = "fill") +
  labs(title = "CFS Prevalence by Age", y = "Proportion") +
  theme_minimal()
ggsave("age_by_cfs.png", plot = p_age, width = 7, height = 5)

# GENDER by CFS
p_gender <- ggplot(health_data_clean, aes(x = Gender, fill = Fatigue_syndrome)) +
  geom_bar(position = "fill") +
  labs(title = "CFS Prevalence by Gender", y = "Proportion") +
  theme_minimal()
ggsave("gender_by_cfs.png", plot = p_gender, width = 7, height = 5)

# FRUIT/VEG Boxplot
p_fv <- ggplot(health_data_clean, aes(x = Fatigue_syndrome, y = Fruit_veg_con, fill = Fatigue_syndrome)) +
  geom_boxplot() +
  labs(title = "Fruit & Vegetable Consumption by CFS Status") +
  theme_minimal()
ggsave("fruitveg_boxplot.png", plot = p_fv, width = 7, height = 5)

# ROC Curve
roc_obj <- roc(health_data_clean$Fatigue_syndrome, predicted_probs)
png("roc_curve.png", width = 700, height = 500)
plot(roc_obj, col = "blue", main = "ROC Curve for Weighted Logistic Regression")
dev.off()

# Deviance residuals
png("deviance_residuals.png", width = 700, height = 500)
plot(residuals(weighted_model, type = "deviance"), 
     ylab = "Deviance Residuals", xlab = "Index",
     main = "Deviance Residuals")
abline(h = 0, col = "red", lty = 2)
dev.off()

# Pearson residuals vs. fitted values
png("pearson_residuals.png", width = 700, height = 500)
plot(fitted(weighted_model), residuals(weighted_model, type = "pearson"), 
     xlab = "Fitted Values", ylab = "Pearson Residuals",
     main = "Standardized Pearson Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)
dev.off()

# Cook's distance
png("cooks_distance.png", width = 700, height = 500)
plot(cooks.distance(weighted_model), 
     main = "Cook's Distance", ylab = "Cook's Distance", xlab = "Index")
abline(h = 4/length(weighted_model$fitted.values), col = "red", lty = 2)
dev.off()

# Leverage vs. Cook’s Distance
influence <- influence.measures(weighted_model)
leverage <- hatvalues(weighted_model)
cooks <- cooks.distance(weighted_model)

png("leverage_vs_influence.png", width = 700, height = 500)
plot(leverage, cooks, 
     xlab = "Leverage", ylab = "Cook's Distance",
     main = "Leverage vs. Cook's Distance")
abline(h = 4/length(weighted_model$fitted.values), col = "red", lty = 2)
abline(v = 2*mean(leverage), col = "blue", lty = 2)
dev.off()