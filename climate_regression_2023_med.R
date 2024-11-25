library(readxl)
library(dplyr)
library(car)
library(lm.beta)
library(Hmisc)
library(corrplot)

setwd("/Users/min/Library/Mobile Documents/com~apple~CloudDocs/Study/python/climate")
df <- read_excel("climate_survey-2023.xlsx")

# data 
X_cols = c('concern', 'aware_cause', 'aware_solve', 'aware_tot', 
           'satis_mean2', 
           'risk', 'risk_me', 'impact_n', 'import_me',
           'eco_at', 'prior_eco',
           'behave_me', 'behave_other', 
           'control_n')
damage_cols = c('damage_me', 'damage_family', 'damage_commun', 
           'damage_country', 'damage_country_high', 'damage_country_low', #damage_country
           'damage_future', 'damage_org')
psychological_cols = c('distance_index', 'distance_weighted')
motive_cols = c('motive_profit', 'motive_norm', 'motive_pleasure', 
                'motive_bio', 'motive_selfish', 'motive_prosocial', 
                'motive_selfinterest', 'motive_environmental',
                'motive_me', 'motive_com')
Y_cols = c('prac_tot', 'prac_tot2', 'prac_ratio', 'prac_intent')
C_cols = c('sex', 'age', 'area', 'edu', 'job', 'child', 'income', 'pol', 
           'media_trust', 'life_satis')

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
                 "age", "edu", "income", "pol", 
                 'media_trust', 'life_satis', motive_cols)

data_centered <- data %>% 
  mutate(across(all_of(c(center_cols, motive_cols)), ~ . - mean(., na.rm = TRUE)))

## change factor
data_centered$area <- as.factor(data_centered$area)
data_centered$job <- as.factor(data_centered$job)

# what is variables
X_col = 'aware_tot'
M_cols = c('eco_at')
Y_col = 'prac_ratio'
XC_cols = c('concern', 'import_me')
C_cols = c('sex', 'age', 'edu', 'child', 'income', 'pol')
motive_cols = c('motive_profit', 'motive_norm', 'motive_pleasure', 
                'motive_bio', 'motive_selfish', 'motive_prosocial')
motive_cols2 = c('motive_selfinterest', 'motive_socialinterest', 'motive_environmental')

# model
## mod
process(data = data_centered, 
        y = Y_col, x = M_cols, w = "motive_diff_soc_self", 
        cov = c(C_cols),
        model = 1)

## medi
process(data = data_centered, 
        y = Y_col, x = X_col, m = M_cols, 
        cov = c(C_cols),
        model = 4)

### medi2
process(data = data_centered, 
        y = Y_col, x = X_col, m = c(M_cols, "motive_selfinterest"), 
        cov = c(XC_cols, C_cols),
        model = 6)

## mod-med 
### X->M
process(data = data_centered, 
        y = Y_col, x = X_col, m = M_cols, w = "motive_selfinterest", 
        cov = c(XC_cols, C_cols),
        model = 7)

### M->Y
process(data = data_centered, 
        y = Y_col, x = X_col, m = M_cols, w = "motive_diff_soc_self", 
        cov = c(C_cols),
        model = 14)

### M->Y & X->Y
process(data = data_centered, 
        y = Y_col, x = X_col, m = M_cols, w = "motive_diff_soc_self", 
        cov = c(C_cols),
        model = 15)

plot(data$eco_at, data$prac_ratio)


# linear regression
X_cols = c('aware_tot', 'eco_at')
formula <- as.formula(paste(Y_cols, "~", 
                            paste(c(X_cols, motive_cols, C_cols), collapse = "+")))
## model fit
model <- lm(formula, data = data_centered)
summary(model)
## VIF
vif_values <- vif(model)
print(vif_values)

# linear regression add control variables
motive_cols = c('motive_profit', 'motive_norm', 'motive_pleasure', 
                'motive_bio', 'motive_selfish', 'motive_prosocial')
C_cols = c('sex', 'age', 'edu', 'child', 'income', 'pol')
## add pol
formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(motive_cols, C_cols), 
                                       collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

## VIF
vif_values_addC <- vif(model_addC)
print(vif_values_addC)

# visual
boxplot(data[motive_cols], 
        main = "motive")
hist(data$motive_prosocial)
hist(data$age)
shapiro.test(data$motive_prosocial2)

# linear regression with interaction
X_cols = c("aware_tot", "eco_at", "control_n")
motive_cols = c("motive_profit", "motive_pleasure", "motive_prosocial")
interactions = c("eco_at:motive_profit", "eco_at:motive_pleasure", "eco_at:motive_prosocial")

formula_addC <- as.formula(paste(Y_cols, "~", 
                                 paste(c(X_cols, motive_cols, C_cols, "control:motive_prosocial"), collapse = "+")))

model_addC <- lm(formula_addC, data = data_centered)
summary(model_addC)

lm.beta(model_addC)

mean(data$aware_tot)
sd(data$aware_tot)
mean(data$eco_at)
sd(data$eco_at)
mean(data$prac_ratio)
sd(data$prac_ratio)
mean(data$motive_selfinterest)
sd(data$motive_selfinterest)
mean(data$motive_socialinterest)
sd(data$motive_socialinterest)

# correlation
cor_matrix <- cor(data[c(X_cols, Y_col)])
print(cor_matrix)

cor_result <- rcorr(as.matrix(data[c(X_cols, Y_col)]))
corrplot(cor_result$r, 
         method = "circle", 
         p.mat = cor_result$P,   
         sig.level = 0.05,        
         insig = "blank",         
         addCoef.col = "black",   
         number.cex = 0.8)        

cor_aware_at <- cor.test(data$aware_tot, data$eco_at, method = "spearman") # 0.161***
cor_aware_prac <- cor.test(data$aware_tot, data$prac_ratio) # 0.234***
cor_at_prac <- cor.test(data$eco_at, data$prac_ratio) # 0.427***
cor_aware_mot_priv <- cor.test(data$aware_tot, data$motive_selfinterest) # 0.178***
cor_aware_mot_soc <- cor.test(data$aware_tot, data$motive_socialinterest) # 0.185***
cor_at_mot_priv <- cor.test(data$eco_at, data$motive_selfinterest) # 0.389***
cor_at_mot_soc <- cor.test(data$eco_at, data$motive_socialinterest) # 0.433***
cor_mot_priv_prac <- cor.test(data$motive_selfinterest, data$prac_ratio) # 0.464***
cor_mot_soc_prac <- cor.test(data$motive_socialinterest, data$prac_ratio) # 0.423***
cor_mot_priv_soc <- cor.test(data$motive_selfinterest, data$motive_socialinterest) # 0.659***
cor_aware_at
cor_aware_prac
cor_at_prac
cor_aware_mot_priv
cor_aware_mot_soc
cor_at_mot_priv
cor_at_mot_soc
cor_mot_priv_prac
cor_mot_soc_prac
cor_mot_priv_soc

