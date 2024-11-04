library(readxl)
library(dplyr)
library(car)

setwd("/Users/min/Library/Mobile Documents/com~apple~CloudDocs/Study/python/climate")
df <- read_excel("climate_survey-2018.xlsx")

# data 
X_cols = c('concern', 'satis_mean2', 'aware_tot', 
          'occur', 'risk', 'risk_me', 'eco_at')
Y_cols = c('prac_tot')
C_cols = c('sex', 'age', 'area', 'edu', 'job', 'child', 'income', 'pol')

data <- df[, c(X_cols, Y_cols, C_cols)]
data <- na.omit(data)

# mean centering
center_cols <- c("concern", "satis_mean2", "aware_tot", "occur", 
                 "risk", "risk_me", "eco_at", 
                 "prac_tot", 
                 "age", "edu", "income", "pol")

data_centered <- data %>% 
  mutate(across(all_of(center_cols), ~ . - mean(., na.rm = TRUE)))

# linear regression
formula <- as.formula(paste(Y_cols, "~", paste(X_cols, collapse = "+")))
## model fit
model <- lm(formula, data = data_centered)
summary(model)
## VIF
vif_values <- vif(model)
print(vif_values)

# control variables
formulaC <- as.formula(paste(Y_cols, "~", paste(C_cols, collapse = "+")))
## model fit
modelC <- lm(formulaC, data = data_centered)
summary(modelC)
## VIF
vif_values_C <- vif(modelC)
print(vif_values_C)

## change factor
data_centered$area <- as.factor(data_centered$area)
data_centered$job <- as.factor(data_centered$job)

## model fit
modelC_factor <- lm(formulaC, data = data_centered)
summary(modelC_factor)
## VIF
vif_values_Cfactor <- vif(modelC_factor)
print(vif_values_Cfactor)

# linear regression add control variables
## sex and age
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, "sex", "age"), collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## add area
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, "sex", "age", "area"), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## add pol
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, "sex", "age", "area", "pol"), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## VIF
vif_values_addC <- vif(model_addC)
print(vif_values_addC)

# * 안뜨는 독립 변인 제외 
X_cols = c('concern', 'aware_tot', 'risk_me', 'eco_at')
C_cols = c('sex', 'age', 'area', 'pol')

formula <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, C_cols), 
                                       collapse = "+")))

model <- lm(formula, data = data_centered)
summary(model)

## VIF
vif_values <- vif(model)
print(vif_values)
