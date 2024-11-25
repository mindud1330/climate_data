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
           'eco_at', 'prior_eco',
           'behave_me', 'behave_other')
psychological_cols = c('distance_index', 'distance_weighted')
motive_cols = c('motive_moral', 'motive_me', 'motive_com')
Y_cols = c('prac_tot')
C_cols = c('sex', 'age', 'area', 'edu', 'job', 'child', 'income', 'pol', 
           'media_trust', 'life_satis')

data <- df[, c(X_cols, psychological_cols, motive_cols, Y_cols, C_cols)]
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

# linear regression
#X_col <- c("motive_me", "risk_me")
# level 1
#formula <- as.formula(paste("Y_cols", "~", "motive_me + risk_me"))
#model_mod <- lm(formula, data = data_centered)
#summary(model_mod)
# level 2
#formula <- as.formula(paste("Y_cols", "~", "motive_me * risk_me"))
#model_mod <- lm(formula, data = data_centered)
#summary(model_mod)

# linear regression
formula <- as.formula(paste("Y_cols", "~", 
                            paste(c(X_cols, motive_cols), collapse = "+")))
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
formula_addC <- as.formula(paste("Y_cols", "~", 
                                 paste(c(X_cols, motive_cols, C_cols), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## VIF
vif_values_addC <- vif(model_addC)
print(vif_values_addC)

# *
X_cols = c('concern', 'satis_mean2', 'aware_tot', 'impact_n', 
           'damage_country', 'damage_country_high',
           'damage_future', 'damage_org', #damage
           'eco_at', 'prior_eco', 
           'behave_other', 
           'motive_me')

# linear regression
formula <- as.formula(paste("Y_cols", "~", 
                            paste(c(X_cols, C_cols), 
                                  collapse = "+")))
## model fit
model <- lm(formula, data = data_centered)
summary(model)
## VIF
vif_values <- vif(model)
print(vif_values)

# linear regression with interaction
interactions <- c("aware_tot:media_trust",
                  "impact_n:life_satis", "eco_at:life_satis",
                  "damage_country:motive_com",
                  "damage_future:motive_com", "damage_org:motive_com")
formula_int <- as.formula(paste("Y_cols", "~", 
                                paste(c(X_cols, C_cols, interactions), collapse = "+")))
### interaction_cols
#### .: impact_n:life_satis
#### *: eco_at:life_satis
## model fit
model_int <- lm(formula_int, data = data_centered)
summary(model_int)
