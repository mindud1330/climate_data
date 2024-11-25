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
           'eco_at', 'prior_eco',
           'behave_me', 'behave_other')
damage_cols = c('damage_me', 'damage_family', 'damage_commun', 
                'damage_country', 'damage_country_high', 'damage_country_low',
                'damage_future', 'damage_org')
distance_cols = c('distance_index', 'distance_weighted')
motive_cols = c('motive_moral', 'motive_me', 'motive_com')
Y_cols = c('prac_tot')
C_cols = c('sex', 'age', 'area', 'edu', 'job', 'child', 'income', 'pol', 
           'media_trust', 'life_satis')

data <- df[, c(X_cols, damage_cols, distance_cols, motive_cols, Y_cols, C_cols)]
data <- na.omit(data)

# mean centering
center_cols <- c('concern', 'aware_cause', 'aware_solve', 
                 "satis_mean2", "aware_tot", 
                 "risk", "risk_me", "impact_n", 'import_me',
                 'damage_me', 'damage_family', 'damage_commun', # damage
                 'damage_country', 'damage_country_high', 'damage_country_low', #damage_country
                 'damage_future', 'damage_org', #damage
                 'eco_at', 'prior_eco',
                 'behave_me', 'behave_other',
                 'motive_moral', 'motive_me', 'motive_com', 
                 "prac_tot", 
                 "age", "edu", "income", "pol", 
                 'media_trust', 'life_satis')

data_centered <- data %>% 
  mutate(across(all_of(center_cols), ~ . - mean(., na.rm = TRUE)))

data_centered['concern_prac_diff'] <- data_centered['prac_tot'] - data_centered['concern']
Y_cols = c('concern_prac_diff')
X_cols = c('aware_cause', 'aware_solve', 
           'satis_mean2', 'aware_tot', 
           'risk', 'risk_me', 'impact_n', 'import_me',
           'eco_at', 'prior_eco',
           'behave_me', 'behave_other')
damage_cols = c('damage_future', 'damage_org')
distance_cols = c('distance_weighted')

# linear regression
formula <- as.formula(paste(Y_cols, "~", 
                            paste(c(X_cols, damage_cols, distance_cols, motive_cols), collapse = "+")))
## model fit
model <- lm(formula, data = data_centered)
summary(model)
## VIF
vif_values <- vif(model)
print(vif_values)

# linear regression add control variables
C_cols = c('sex', 'age', 'edu', 'child', 'income', 'pol',
           'media_trust', 'life_satis')
## add pol
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, damage_cols, distance_cols, motive_cols, C_cols), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## VIF
vif_values_addC <- vif(model_addC)
print(vif_values_addC)

# distance
X_cols = c('concern', 'aware_cause', 'aware_solve', 
           'satis_mean2', 'aware_tot', 
           'risk', 'risk_me', 'impact_n', 'import_me',
           'damage_future', 'damage_org', #damage
           'eco_at', 'prior_eco',
           'behave_me', 'behave_other')
distance_cols = c('distance_index')
distance_cols = c('distance_weighted')

# linear regression add control variables
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, damage_cols, distance_cols, motive_cols, C_cols), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## VIF
vif_values_addC <- vif(model_addC)
print(vif_values_addC)

# *
X_cols = c('satis_mean2', 'aware_tot', 'impact_n', 
           'distance_weighted',
           'damage_future', 'damage_org', #damage
           'eco_at', 'prior_eco', 
           'behave_other', 
           'motive_moral', 'motive_me', 'motive_com')

# linear regression
formula <- as.formula(paste(Y_cols, "~", 
                            paste(c(X_cols, C_cols), 
                                  collapse = "+")))
## model fit
model <- lm(formula, data = data_centered)
summary(model)
## VIF
vif_values <- vif(model)
print(vif_values)

# linear regression with interaction
interactions <- c('motive_moral:motive_me', 'motive_moral:motive_com', 'motive_me:motive_com', 
                  'motive_moral:motive_me:motive_com', 
                  'impact_n:life_satis', 'eco_at:life_satis', 'behave_other:life_satis', 
                  'aware_tot:distance_weighted', 'impact_n:distance_weighted', 
                  'damage_future:distance_weighted', 
                  'motive_me:distance_weighted')
interactions <- c('motive_moral:motive_me', 'motive_moral:motive_com', 'motive_me:motive_com', 
                  'motive_moral:motive_me:motive_com', 
                  'impact_n:life_satis', 'eco_at:life_satis', 'behave_other:life_satis', 
                  'aware_tot:prior_eco', 'impact_n:prior_eco')
interactions <- c('motive_moral:motive_me', 'motive_moral:motive_com', 'motive_me:motive_com', 
                  'motive_moral:motive_me:motive_com', 
                  'impact_n:life_satis', 'eco_at:life_satis', 'behave_other:life_satis', 
                  'aware_tot:motive_moral', 'impact_n:motive_moral', 
                  'damage_future:motive_moral')
interactions <- c('motive_moral:motive_me', 'motive_moral:motive_com', 'motive_me:motive_com', 
                  'motive_moral:motive_me:motive_com', 
                  'impact_n:life_satis', 'eco_at:life_satis', 'behave_other:life_satis', 
                  'aware_tot:motive_me', 'impact_n:motive_me')
interactions <- c('motive_moral:motive_me', 'motive_moral:motive_com', 'motive_me:motive_com', 
                  'motive_moral:motive_me:motive_com', 
                  'impact_n:life_satis', 'eco_at:life_satis', 'behave_other:life_satis', 
                  'aware_tot:motive_me', 'impact_n:motive_me', 
                  'aware_tot:motive_com', 'impact_n:motive_com')
formula_int <- as.formula(paste(Y_cols, "~", 
                                paste(c(X_cols, damage_cols, distance_cols, C_cols, interactions), collapse = "+")))

## model fit
model_int <- lm(formula_int, data = data_centered)
summary(model_int)
