rm(list=ls())

#Load libraries
library(haven)
library(knitr)
library(lattice)
library(tidyverse)
library(here)
library(flextable)
library(devtools)
library(lavaan)
library(ggplot2)
library(plm)
library(naniar)
library(purrr)
library(psych)
library(interactions)

#**************************************************************************************************************************/
##########  PAPER MEPOP - Efficacy  ##########################################################
#**************************************************************************************************************************/

############ WAVE 1 ############

#Import Data

data_w1 <- read_sav("Data_W1.sav")

# ID
data_w1$id <- data_w1$CodPanelista

# Age

data_w1$age_num <- data_w1$age
describe(data_w1$age_num)

# Socioeconomic Status

data_w1$ses <- data_w1$RECO_NSE
table(data_w1$ses)

# Education

data_w1$educ <- data_w1$P60
table(data_w1$educ)


# Sex (1=women)

data_w1 <- data_w1%>%
  mutate(sex = ifelse(SEX == 2, 1,
                      ifelse(SEX == 1, 0, NA)))
table(data_w1$SEX)
table(data_w1$sex)

# Ideology

table(data_w1$P32)
data_w1$ideology <- ifelse(data_w1$P32 == 99, NA, data_w1$P32)
table(data_w1$ideology)


# Online Political Efficacy

data_w1$ope1 <- data_w1$P59_1
data_w1$ope2 <- data_w1$P59_2
data_w1$ope3 <- data_w1$P59_3
data_w1$ope4 <- data_w1$P59_4

table(data_w1$ope1)
table(data_w1$ope2)
table(data_w1$ope3)
table(data_w1$ope4)

# External Political Efficacy (recode)

data_w1$extef1 <- data_w1$P58_1
data_w1$extef2 <- data_w1$P58_2
data_w1$extef3 <- data_w1$P58_3

table(data_w1$extef1)
table(data_w1$extef2)
table(data_w1$extef3)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
data_w1 <- data_w1 %>%
  mutate(across(c(extef1, extef2, extef3), ~ 6 - .x))

# Internal Political Efficacy

data_w1$intef1 <- data_w1$P58_4
data_w1$intef2 <- data_w1$P58_5
data_w1$intef3 <- data_w1$P58_6

table(data_w1$intef1)
table(data_w1$intef2)
table(data_w1$intef3)


# Media Exposure

data_w1$tv <- data_w1$P4_1
data_w1$cable <- data_w1$P4_2
data_w1$newspaper <- data_w1$P4_3
data_w1$radio <- data_w1$P4_4
data_w1$tradonline <- data_w1$P4_5
data_w1$online <- data_w1$P4_6
data_w1$podcast <- data_w1$P4_7
data_w1$officialsm <- data_w1$P4_8

table(data_w1$tv)
table(data_w1$cable)
table(data_w1$newspaper)
table(data_w1$radio)
table(data_w1$tradonline)
table(data_w1$online)
table(data_w1$podcast)
table(data_w1$officialsm)


# Social Media Exposure

data_w1$fb <- ifelse(data_w1$P5_1 == 99, NA, data_w1$P5_1)
data_w1$insta <- ifelse(data_w1$P5_2 == 99, NA, data_w1$P5_2)
data_w1$twitter <- ifelse(data_w1$P5_3 == 99, NA, data_w1$P5_3)
data_w1$whatsapp <- ifelse(data_w1$P5_4 == 99, NA, data_w1$P5_4)
data_w1$youtube <- ifelse(data_w1$P5_5 == 99, NA, data_w1$P5_5)
data_w1$tiktok <- ifelse(data_w1$P5_6 == 99, NA, data_w1$P5_6)
data_w1$discord <- ifelse(data_w1$P5_7 == 99, NA, data_w1$P5_7)
data_w1$twitch <- ifelse(data_w1$P5_8 == 99, NA, data_w1$P5_8)

table(data_w1$fb)
table(data_w1$insta)
table(data_w1$twitter)
table(data_w1$whatsapp)
table(data_w1$youtube)
table(data_w1$tiktok)
table(data_w1$discord)
table(data_w1$twitch)


# Franja Exposure

data_w1$franja <- data_w1$P6_1
table(data_w1$franja)

# Social Media Political Use

table(data_w1$P25_5)
table(data_w1$P25_6)
table(data_w1$P25_7)
table(data_w1$P25_8)
table(data_w1$P25_9)
table(data_w1$P25_10)
table(data_w1$P25_11)

data_w1$use1 <- data_w1$P25_5
data_w1$use2 <- data_w1$P25_6
data_w1$use3 <- data_w1$P25_7
data_w1$use4 <- data_w1$P25_8
data_w1$use5 <- data_w1$P25_9
data_w1$use6 <- data_w1$P25_10
data_w1$use7 <- data_w1$P25_11

# Interest

data_w1$polint <- data_w1$P21
table(data_w1$polint)

data_w1$procint <- data_w1$P22
table(data_w1$procint)

data_w1$plebint <- data_w1$P23
table(data_w1$plebint)

############ ANALYSES ############

############ Cronbach ############


cronbach_ope <- alpha(na.omit(data_w1[c("ope1", "ope2", "ope3", "ope4")]))
cronbach_ope

cronbach_intef <- alpha(na.omit(data_w1[c("intef1", "intef2", "intef3")]))
cronbach_intef

cronbach_extef <- alpha(na.omit(data_w1[c("extef1", "extef2", "extef3")]))
cronbach_extef

cronbach_media <- alpha(na.omit(data_w1[c("tv", "cable", "newspaper", "radio", "tradonline", "online", "podcast", "officialsm")]))
cronbach_media

cronbach_social <- alpha(na.omit(data_w1[c("fb", "insta", "twitter", "whatsapp", "youtube", "tiktok", "discord", "twitch")]))
cronbach_social

cronbach_interest <- alpha(na.omit(data_w1[c("polint", "procint", "plebint")]))
cronbach_interest

cronbach_poluse <- alpha(na.omit(data_w1[c("use1", "use2", "use3", "use4", "use5", "use6", "use7")]))
cronbach_poluse

####### CFA #######

data_w1_na <- na.omit(data_w1[c("id", "polint", "procint", "plebint", "ope1", "ope2", "ope3", "ope4", "intef1", "intef2", "intef3", "extef1", "extef2", "extef3", "tv", "cable", "newspaper", "radio", "tradonline", "online", "podcast", "officialsm","fb", "insta", "twitter", "whatsapp", "youtube", "tiktok", "discord", "twitch", "franja", "use1", "use2", "use3", "use4", "use5", "use6", "use7", "age_num", "ses", "sex", "educ")])
sum(is.na(data_w1_na))

data_w1_na <- data_w1_na %>%
  mutate(across(where(is.labelled), as.numeric))

cfa.model <- 'ope =~ ope1 + ope2 + ope3 + ope4
              intef =~ intef1 + intef2 + intef3
              extef =~ extef1 + extef2 + extef3
              media =~ tv + cable + newspaper + radio + tradonline + online + podcast + officialsm
              social =~ fb + insta + twitter + whatsapp + youtube + tiktok + discord + twitch
              interest =~ polint + procint + plebint
              poluse =~ use1 + use2 + use3 + use4 + use5 + use6 + use7'


fit_cfa <- cfa(cfa.model, data = data_w1_na)
latent_scores <- predict(fit_cfa)

data_w1_scores <- cbind(data_w1_na, latent_scores)


####### OLS #######

# Online Political Efficacy

ols_ope1 <- lm(ope ~ age_num + ses + educ + sex + interest + media, data = data_w1_scores)
summary(ols_ope1)

ols_ope2 <- lm(ope ~ age_num + ses + educ + sex + interest + social, data = data_w1_scores)
summary(ols_ope2)

ols_ope3 <- lm(ope ~ age_num + ses + educ + sex + interest + franja, data = data_w1_scores)
summary(ols_ope3)

ols_ope4 <- lm(ope ~ age_num + ses + educ + sex + interest + poluse, data = data_w1_scores)
summary(ols_ope4)

# Internal Efficacy

ols_intef1 <- lm(intef ~ age_num + ses + educ + sex + interest + media, data = data_w1_scores)
summary(ols_intef1)

ols_intef2 <- lm(intef ~ age_num + ses + educ + sex + interest + social, data = data_w1_scores)
summary(ols_intef2)

ols_intef3 <- lm(intef ~ age_num + ses + educ + sex + interest + franja, data = data_w1_scores)
summary(ols_intef3)

ols_intef4 <- lm(intef ~ age_num + ses + educ + sex + interest + poluse, data = data_w1_scores)
summary(ols_intef4)

# External Efficacy

ols_extef1 <- lm(extef ~ age_num + ses + educ + sex + interest + media, data = data_w1_scores)
summary(ols_extef1)

ols_extef2 <- lm(extef ~ age_num + ses + educ + sex + interest + social, data = data_w1_scores)
summary(ols_extef2)

ols_extef3 <- lm(extef ~ age_num + ses + educ + sex + interest + franja, data = data_w1_scores)
summary(ols_extef3)

ols_extef4 <- lm(extef ~ age_num + ses + educ + sex + interest + poluse, data = data_w1_scores)
summary(ols_extef4)



############ WAVE 3 ############

#Import Data

data_w3 <- read_sav("Data_W3.sav")

# ID
data_w3$id <- data_w3$CodPanelista

# Age

data_w3$age_num <- data_w3$age
describe(data_w3$age_num)

# Socioeconomic Status

data_w3$ses <- data_w3$RECO_NSE
table(data_w3$ses)


# Sex (1=women)

data_w3 <- data_w3%>%
  mutate(sex = ifelse(SEX == 2, 1,
                      ifelse(SEX == 1, 0, NA)))
table(data_w3$SEX)
table(data_w3$sex)

# Ideology

table(data_w3$P32)
data_w3$ideology <- ifelse(data_w3$P32 == 99, NA, data_w3$P32)
table(data_w3$ideology)


# Social Media Political Use

table(data_w3$P25_5)
table(data_w3$P25_6)
table(data_w3$P25_7)
table(data_w3$P25_8)
table(data_w3$P25_9)
table(data_w3$P25_10)
table(data_w3$P25_11)

data_w3$use1 <- data_w3$P25_5
data_w3$use2 <- data_w3$P25_6
data_w3$use3 <- data_w3$P25_7
data_w3$use4 <- data_w3$P25_8
data_w3$use5 <- data_w3$P25_9
data_w3$use6 <- data_w3$P25_10
data_w3$use7 <- data_w3$P25_11


# Online Political Efficacy

data_w3$ope1 <- data_w3$P59_1
data_w3$ope2 <- data_w3$P59_2
data_w3$ope3 <- data_w3$P59_3
data_w3$ope4 <- data_w3$P59_4

table(data_w3$ope1)
table(data_w3$ope2)
table(data_w3$ope3)
table(data_w3$ope4)

# External Political Efficacy (recode)

data_w3$extef1 <- data_w3$P58_1
data_w3$extef2 <- data_w3$P58_2
data_w3$extef3 <- data_w3$P58_3

table(data_w3$extef1)
table(data_w3$extef2)
table(data_w3$extef3)

# To recode efficacies (intef1, intef3, extef1, extef3, extef4)
data_w3 <- data_w3 %>%
  mutate(across(c(extef1, extef2, extef3), ~ 6 - .x))

# Internal Political Efficacy

data_w3$intef1 <- data_w3$P58_4
data_w3$intef2 <- data_w3$P58_5
data_w3$intef3 <- data_w3$P58_6

table(data_w3$intef1)
table(data_w3$intef2)
table(data_w3$intef3)

############ TESTING ALTERNATIVE MODEL ############

####### Reliability test #######

#cronbach_ope_3 <- alpha(na.omit(data_w3[c("ope1", "ope2", "ope3", "ope4")]))
#cronbach_ope_3

#cronbach_intef_3 <- alpha(na.omit(data_w3[c("intef1", "intef2", "intef3")]))
#cronbach_intef_3

#cronbach_extef_3 <- alpha(na.omit(data_w3[c("extef1", "extef2", "extef3")]))
#cronbach_extef_3


#data_w3_na <- na.omit(data_w3[c("id", "ope1", "ope2", "ope3", "ope4", "intef1", "intef2", "intef3", "extef1", "extef2", "extef3", "age_num", "ses", "sex")])
#sum(is.na(data_w3_na))

#data_w3_na <- data_w3_na %>%
#  mutate(across(where(is.labelled), as.numeric))

#cfa.model_3 <- 'ope =~ ope1 + ope2 + ope3 + ope4
#              intef =~ intef1 + intef2 + intef3
#              extef =~ extef1 + extef2 + extef3'


#fit_cfa_3 <- cfa(cfa.model_3, data = data_w3_na)
#latent_scores_3 <- predict(fit_cfa_3)

#data_w3_scores <- cbind(data_w3_na, latent_scores_3)

#data_w3_scores$ope_w3 <- data_w3_scores$ope
#data_w3_scores$intef_w3 <- data_w3_scores$intef
#data_w3_scores$extef_w3 <- data_w3_scores$extef


# Assuming 'id' is the common identifier

# Select only the necessary columns from data_w3
#data_w3_subset <- data_w3_scores[, c("id", "ope_w3", "intef_w3", "extef_w3")]

# Merge data_w1 with the subset of data_w3
#data_w1_w3 <- merge(data_w1_scores, data_w3_subset, by = "id")

# Check the first few rows to confirm the merge
#head(data_w1_w3)


#data_w1_w3$ope_dif <- data_w1_w3$ope_w3 - data_w1_w3$ope
#data_w1_w3$intef_dif <- data_w1_w3$intef_w3 - data_w1_w3$intef
#data_w1_w3$extef_dif <- data_w1_w3$extef_w3 - data_w1_w3$extef


####### OLS - Differences #######

# Online Political Efficacy

#ols_ope_dif1 <- lm(ope_dif ~ age_num + ses + educ + sex + media, data = data_w1_w3)
#summary(ols_ope_dif1)

#ols_ope_dif2 <- lm(ope_dif ~ age_num + ses + educ + sex + social, data = data_w1_w3)
#summary(ols_ope_dif2)

#ols_ope_dif3 <- lm(ope_dif ~ age_num + ses + educ + sex + franja, data = data_w1_w3)
#summary(ols_ope_dif3)


# Internal Efficacy

#ols_intef_dif1 <- lm(intef_dif ~ age_num + ses + educ + sex + media, data = data_w1_w3)
#summary(ols_intef_dif1)

#ols_intef_dif2 <- lm(intef_dif ~ age_num + ses + educ + sex + social, data = data_w1_w3)
#summary(ols_intef_dif2)

#ols_intef_dif3 <- lm(intef_dif ~ age_num + ses + educ + sex + franja, data = data_w1_w3)
#summary(ols_intef_dif3)


# External Efficacy

#ols_extef_dif1 <- lm(extef_dif ~ age_num + ses + educ + sex + media, data = data_w1_w3)
#summary(ols_extef_dif1)

#ols_extef_dif2 <- lm(extef_dif ~ age_num + ses + educ + sex + social, data = data_w1_w3)
#summary(ols_extef_dif2)

#ols_extef_dif3 <- lm(extef_dif ~ age_num + ses + educ + sex + franja, data = data_w1_w3)
#summary(ols_extef_dif3)



############ LONGITUDINAL ANALYSIS ############

############ Creating Long Dataset ############

# Subset the necessary variables
data_w1_selec <- data_w1 %>% select(id, ope1, ope2, ope3, ope4, intef1, intef2, intef3, extef1, extef2, extef3, use1, use2, use3, use4, use5, use6, use7, sex, ses, age_num, ideology)

data_w3_selec <- data_w3 %>% select(id, ope1, ope2, ope3, ope4, intef1, intef2, intef3, extef1, extef2, extef3, use1, use2, use3, use4, use5, use6, use7, sex, ses, age_num, ideology)

# Find common ids in both datasets
common_ids <- intersect(data_w1_selec$id, data_w3_selec$id)

# Filter both data frames to include only those ids
data_w1_selec <- data_w1_selec %>% filter(id %in% common_ids)
data_w3_selec <- data_w3_selec %>% filter(id %in% common_ids)

# Add a wave identifier
data_w1_selec$wave <- 1
data_w3_selec$wave <- 2

# Combine the datasets into a long format
data_long <- rbind(data_w1_selec, data_w3_selec)

# Check the first few rows of the combined dataset
head(data_long)

# Check the distribution of data across waves
table(data_long$wave)


############ Creating Latent Variables for Panel ############

cronbach_ope_long <- alpha(na.omit(data_long[c("ope1", "ope2", "ope3", "ope4")]))
cronbach_ope_long

cronbach_intef_long <- alpha(na.omit(data_long[c("intef1", "intef2", "intef3")]))
cronbach_intef_long

cronbach_extef_long <- alpha(na.omit(data_long[c("extef1", "extef2", "extef3")]))
cronbach_extef_long

cronbach_poluse_long <- alpha(na.omit(data_long[c("use1", "use2", "use3", "use4", "use5", "use6", "use7")]))
cronbach_poluse_long




data_long <- data_long %>%
  mutate(across(where(is.labelled), as.numeric))

cfa.model_long <- 'ope =~ ope1 + ope2 + ope3 + ope4
                intef =~ intef1 + intef2 + intef3
                extef =~ extef1 + extef2 + extef3
                poluse =~ use1 + use2 + use3 + use4 + use5 + use6 + use7'

fit_cfa_long <- cfa(cfa.model_long, data = data_long)
latent_scores_long <- predict(fit_cfa_long)

data_long_scores <- cbind(data_long, latent_scores_long)



############ Converting to Panel DF ############

# Convert your data to a panel data frame
pdata <- pdata.frame(data_long_scores, index = c("id", "wave"))


############ Panel Regressions ############

# Fit a random effects model
model_re_ope <- plm(ope ~ poluse + ideology + ses + sex + age_num, data = pdata, model = "random")
# Summary of the random effects model
summary(model_re_ope)

# Fit a fixed effects model
model_fe_intef <- plm(intef ~ poluse + ideology + ses + sex + age_num, data = pdata, model = "random")
# Summary of the fixed effects model
summary(model_fe_intef)

# Fit a fixed effects model
model_fe_extef <- plm(extef ~ poluse + ideology + ses + sex + age_num, data = pdata, model = "random")
# Summary of the fixed effects model
summary(model_fe_extef)
