library(lubridate)
library(tidyverse)
library(dplyr)

orig <- read.delim("sample_orig_2007.txt", header = FALSE, sep = "|", stringsAsFactors = TRUE)
svcg <- read.delim("sample_svcg_2007.txt", header = FALSE, sep = "|", stringsAsFactors = TRUE)

# Cleaning
names(svcg) <- c('id','month','upb','status','age','timetomat','settledate','modification',
                       'zerocode','zerodate','currentrate','niupb','paidthrough','mirecovery','salegains','nonmirecovery',
                       'allcosts','legalcosts','maintcosts','taxcosts','misccosts','loss','modloss','modadjusted',
                       'deferral','eltv','lastbalance','interestatdefault','disaster','assistancetype','monmodloss','iupb')

svcg$zerocode <- as.factor(svcg$zerocode)

names(orig) <- c('fico','firstpay','newowner','maturity','msa','prop_insured','units','occupancy',
                       'cltv','dti','balance','ltv','rate','channel','ppm_flag','fixed',
                       'state','hometype','zip3','id','purpose','term','numborrowers','seller',
                       'servicer','super','reliefid','program','refinance','valuation','io','micanceled')

svcg_subset <- svcg %>% select("id", "status", "age", "settledate", "loss")

# 1B
svcg_subset <- svcg_subset %>% filter(age <= 36)

#1C
svcg_subset <- svcg_subset %>%
  group_by(id) %>%
  mutate(default_flag = if_else(
    any(!(status %in% c("0", "1", "2")), na.rm = TRUE) | any(loss < 0, na.rm = TRUE), 
    1, 
    0
  )) %>%
  ungroup()

#1D
svcg_subset <- svcg_subset %>%
  group_by(id) %>%
  mutate(
    defect_flag = if_else(
      !is.na(any(settledate)),  # Settlement exists  within the first 36 months
      1,  # If true, flag as 1 (valid settlement within first 36 months)
      0   # If false, flag as 0 (settlement did not happen within 36 months)
    )
  ) %>%
  ungroup()

#1E
flags <- svcg_subset %>%
  select(id, default_flag, defect_flag) %>%
  distinct()

loans_with_no_data <- orig %>%
  filter(!id %in% svcg_subset$id) %>%
  select(id)

orig_filtered <- orig %>%
  filter(!id %in% loans_with_no_data$id)

orig_flags <- orig_filtered %>%
  left_join(flags, by = "id")

#2A
summary(orig_flags$fico)
summary(orig_flags$cltv)
summary(orig_flags$dti)
summary(orig_flags$balance)
summary(orig_flags$rate)

orig_flags <- orig_flags %>%
  mutate(
    fico = ifelse(fico < 300 | fico > 850, NA, fico),
    cltv = ifelse(cltv == 999, NA, cltv),
    dti = ifelse(dti == 999, NA, dti),
    newowner = ifelse(newowner == 9, NA, newowner),
    occupancy = ifelse(occupancy == 9, NA, occupancy)
  )

orig_clean <- orig_flags %>%
  filter(!is.na(fico) & !is.na(cltv) & !is.na(dti))

logit_model <- glm(
  default_flag ~ fico + cltv + dti + balance + rate,
  data = orig_clean,
  family = binomial(link = "logit")
)

summary(logit_model)

#2B
null_deviance <- 31028
residual_deviance <- 26419
df <- 5
deviance_diff <- null_deviance - residual_deviance
p_value <- pchisq(deviance_diff, df, lower.tail = FALSE)

deviance_diff  
p_value       

#3A
probit_model <- glm(default_flag ~ fico + cltv + dti + balance + rate, 
                    family = binomial(link = "probit"), 
                    data = orig_clean)

summary(probit_model)

# probit does slightly better -- lower AIC than logit

#3B

# Logit
logit_probs <- predict(logit_model, type = "response")

logit_pred <- ifelse(logit_probs > 0.5, 1, 0)

logit_conf_matrix <- table(Predicted = logit_pred, Actual = orig_clean$default_flag)

logit_conf_matrix

# Probit
probit_probs <- predict(probit_model, type = "link")  # Linear predictor (z-scores)
probit_probs <- pnorm(probit_probs)  # Convert z-scores to probabilities using the normal CDF

probit_pred <- ifelse(probit_probs > 0.5, 1, 0)

probit_conf_matrix <- table(Predicted = probit_pred, Actual = orig_clean$default_flag)

probit_conf_matrix

#4B
orig_clean$expected_loss <- ifelse(orig_clean$defect_flag == 1, 0.25 * orig_clean$balance * logit_probs, 0)

orig_clean$observed_loss <- ifelse(orig_clean$default_flag == 1 & orig_clean$defect_flag == 1, 0.25 * orig_clean$balance, 0)

total_expected_loss <- sum(orig_clean$expected_loss)
total_observed_loss <- sum(orig_clean$observed_loss)

scaled_expected_loss <- total_expected_loss * (1000000 / nrow(orig_clean))
scaled_observed_loss <- total_observed_loss * (1000000 / nrow(orig_clean))

#5A
summary(orig_clean$newowner)
summary(orig_clean$prop_insured)
summary(orig_clean$term)
summary(orig_clean$occupancy)

new_clean <- orig_flags %>%
  filter(!is.na(fico) & !is.na(cltv) & !is.na(dti) & !is.na(newowner) & !is.na(occupancy))

new_clean$newowner <- as.factor(new_clean$newowner)

new_model <- glm(
  default_flag ~ fico + cltv + dti + balance + rate + newowner + prop_insured + term + fico*cltv + occupancy,
  data = new_clean,
  family = binomial(link = "logit")
)

summary(new_model)

new_probs <- predict(new_model, type = "response")

new_pred <- ifelse(new_probs > 0.5, 1, 0)

new_conf_matrix <- table(Predicted = new_pred, Actual = new_clean$default_flag)

new_conf_matrix

