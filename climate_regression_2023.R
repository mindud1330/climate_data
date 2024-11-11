library(readxl)
library(dplyr)
library(car)
library(lm.beta)

setwd("/Users/min/Library/Mobile Documents/com~apple~CloudDocs/Study/python/climate")
df <- read_excel("climate_survey-2023.xlsx")

# data 
X_cols = c('concern', 'aware_cause', 'aware_solve', 
           'satis_mean2', 'aware_tot', 
           'risk', 'risk_me', 'impact_n', 'import_me',
           'damage_me', 'damage_family', 'damage_commun', # damage
           'damage_country', 'damage_country_high', 'damage_country_low', #damage_country
           'damage_future', 'damage_org', #damage
           'eco_at', 'behave_me', 'behave_other')
Y_cols = c('prac_tot')
C_cols = c('sex', 'age', 'area', 'edu', 'job', 'child', 'income', 'pol', 
           'media_trust', 'life_satis')

data <- df[, c(X_cols, Y_cols, C_cols)]
data <- na.omit(data)

# mean centering
center_cols <- c('concern', 'aware_cause', 'aware_solve', 
                 "satis_mean2", "aware_tot", 
                 "risk", "risk_me", "impact_n", 'import_me',
                 'damage_me', 'damage_family', 'damage_commun', # damage
                 'damage_country', 'damage_country_high', 'damage_country_low', #damage_country
                 'damage_future', 'damage_org', #damage
                 'eco_at', 'behave_me', 'behave_other',
                 "prac_tot", 
                 "age", "edu", "income", "pol", 
                 'media_trust', 'life_satis')

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

# *
X_cols = c('concern', 'aware_tot', 
           'impact_n', 
           'damage_future', 'damage_org', #damage
           'eco_at', 'behave_me', 'behave_other')

## interaction
# linear regression
formula_int <- as.formula(paste(Y_cols, "~", 
                                paste(X_cols, collapse = "+"), 
                                '+ damage_future:damage_org'))
## model fit
model_int <- lm(formula_int, data = data_centered)
summary(model_int)
## VIF
vif_values_int <- vif(model_int)
print(vif_values_int)


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
C_cols = c('age', 'edu', 'child', 'income', 'pol',
           'media_trust', 'life_satis')
## add pol
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, C_cols), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## VIF
vif_values_addC <- vif(model_addC)
print(vif_values_addC)

## beta
formula <- as.formula(paste(Y_cols, "~", paste(X_cols, collapse = "+")))
model <- lm(formula, data = data_centered)
model_beta <- lm.beta(model)

print(model_beta)
