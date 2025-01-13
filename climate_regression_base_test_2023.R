library(readxl)
library(dplyr)
library(car)
library(lmtest)

setwd("/Users/min/Library/Mobile Documents/com~apple~CloudDocs/Study/python/climate")
df <- read_excel("climate_survey-2023.xlsx")

# data 
X_cols = c('concern', 'aware_cause', 'aware_solve', 'aware_tot', 
           'satis_mean2', 
           'risk', 'risk_me', 'impact_n', 'import_me',
           'eco_at', 'prior_eco',
           'behave_me', 'behave_other', 
           'control_n')
motive_cols = c('motive_profit', 'motive_norm', 'motive_pleasure', 
                'motive_bio', 'motive_selfish', 'motive_prosocial', 
                'motive_selfinterest', 'motive_environmental',
                'motive_me', 'motive_com')
Y_cols = c('prac_tot', 'prac_tot2', 'prac_ratio', 'prac_intent')

data <- df[, c(X_cols, motive_cols, Y_cols, C_cols)]
data <- na.omit(data)

data['motive_socialinterest'] <- data$motive_norm + data$motive_prosocial

data['motive_diff_self_soc'] <- data$motive_selfinterest - data$motive_socialinterest
data['motive_diff_soc_self'] <- data$motive_socialinterest - data$motive_selfinterest

data['motive_diff_self_env'] <- data$motive_selfinterest - data$motive_environmental
data['motive_diff_env_self'] <- data$motive_environmental - data$motive_selfinterest

data['motive_diff_com_me'] <- data$motive_com - data$motive_me
data['motive_diff_me_com'] <- data$motive_me - data$motive_com

motive_cols = c('motive_profit', 'motive_norm', 'motive_pleasure', 
                'motive_bio', 'motive_selfish', 'motive_prosocial', 
                'motive_selfinterest', 'motive_socialinterest', 'motive_environmental', 
                'motive_me', 'motive_com')

# mean centering
center_cols <- c('concern', 'aware_cause', 'aware_solve', 
                 "satis_mean2", "aware_tot", 
                 "risk", "risk_me", "impact_n", 'import_me',
                 'eco_at', 'prior_eco',
                 'behave_me', 'behave_other',
                 'control_n', 
                 "prac_tot", "prac_tot2", "prac_intent", 
                 motive_cols)

data_centered <- data %>% 
  mutate(across(all_of(c(center_cols, motive_cols)), ~ . - mean(., na.rm = TRUE)))

# what is variables
X_col = 'aware_tot'
M_cols = c('eco_at')
Y_col = 'prac_ratio'
motive_cols = c('motive_profit', 'motive_norm', 'motive_pleasure', 
                'motive_bio', 'motive_selfish', 'motive_prosocial')
motive_cols2 = c('motive_selfinterest', 'motive_socialinterest', 'motive_environmental')


# X, Y
model <- lm(prac_ratio ~ aware_tot, data = data_centered)
# X, M
model <- lm(eco_at ~ aware_tot, data = data_centered)
# M, Y
model <- lm(prac_ratio ~ eco_at, data = data_centered)
# X, M, Y
model <- lm(prac_ratio ~ aware_tot + eco_at, data = data_centered)

## Linearity
plot(model, which = 1)  # Residuals vs Fitted plot

## Independence
durbinWatsonTest(model)

## Normality
plot(model, which = 2)  # Q-Q plot
shapiro.test(residuals(model))

## Homoscedasticity
plot(model, which = 3)  # Scale-Location plot
bptest(model)

## Influential Observations
plot(model, which = 4)  # Cook's distance plot

## Multicollinearity
vif(model)
