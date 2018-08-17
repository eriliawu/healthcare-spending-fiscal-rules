# fiscal rules and healthcare spending
# paper 2
# how diff sectors change behaviors in the face of fiscal pressure

setwd("H:/apps/xp/desktop/health-spending")
health <- read.csv("fiscal_rules_data_071618.csv", stringsAsFactors = FALSE)

### install and load packages ----
#install.packages("lfe")
suppressWarnings(library(lfe))

### cleaning ----
# check class and convert to numeric if necessary
names(health)
colnames(health)[82:90] <- c("ct_inpat", "ct_outpat", "mri_inpat", "mri_outpat",
                             "hospital_employment_bed_rat", "tot_hospital_beds",
                             "net_income", "out_pocket_exp", "health_sys")
sapply(health[, 82:92], class) # outpat is a character var, with NA, blank and ..
convert_to_num <- function(x) {
      x[x==".."] <- ""
      x <- as.numeric(x)
}
health[, 91:92] <- sapply(health[, 91:92], convert_to_num)

### analysis ----
# inpatient and outpatient ratio as outcome
# covariates: debt, income, hospital beds, health tech
health$inout_ratio <- health$inpat/health$outpat
health$mri_inout_ratio <- health$mri_inpat/health$mri_outpat
health$ct_inout_ratio <- health$ct_inpat/health$ct_outpat

sapply(health[, 93:95], summary) #too many missing data pts in mri and ct

mod1 <- felm(inout_ratio~fr+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
             data=health)
summary(mod1)

mod2 <- felm(inout_ratio~fr+debt_ratio+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
             data=health)
summary(mod2) # bad predictors
