# fiscal rules and healthcare spending
# paper 2
# how diff sectors change behaviors in the face of fiscal pressure

setwd("H:/apps/xp/desktop/health-spending")
health <- read.csv("fiscal_rules_data_090118.csv", stringsAsFactors = FALSE)

### install and load packages ----
#install.packages("lfe")
suppressWarnings(library(lfe))

### cleaning ----
# check class and convert to numeric if necessary
names(health)
colnames(health)[82:90] <- c("ct_inpat", "ct_outpat", "mri_inpat", "mri_outpat",
                             "hospital_employment_bed_rat", "tot_hospital_beds",
                             "net_income", "out_pocket_exp", "health_sys")
colnames(health)[94:99] <- c("urban", "intermed", "rural", "urb_trend", 
                             "intermed_trened", "rural_trend")
sapply(health[, 82:99], class) # outpat is a character var, with NA, blank and ..

convert_to_num <- function(x) {
      x[x==".."] <- ""
      x <- as.numeric(x)
}
health[, 91:92] <- sapply(health[, 91:92], conver_to_num)

# analyses
### inpatient and outpatient ratio as outcome ----
# covariates: debt, income, hospital beds, health tech
health$inout_ratio <- health$inpat/health$outpat
health$mri_inout_ratio <- health$mri_inpat/health$mri_outpat
health$ct_inout_ratio <- health$ct_inpat/health$ct_outpat

sapply(health[, 93:95], summary) #too many missing data pts in mri and ct

mod1_fr <- felm(inout_ratio~fr+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
             data=health)
summary(mod1_fr) #fr is positive

mod1_er <- felm(inout_ratio~er+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
                data=health)
summary(mod1_er) #er not significant

mod2 <- felm(inout_ratio~fr+debt_ratio+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
             data=health)
summary(mod2)

# add health system var
table(health$health_sys)
sapply(health[, 94:96], summary)

#add urban/rural var
mod3_fr <- felm(inout_ratio~fr+urban+intermed+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
             data=health)
summary(mod3_fr) #fr still positive

mod3_er <- felm(inout_ratio~er+urban+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
                data=health)
summary(mod3_er) #er not significant

#add health system
mod4 <- felm(inout_ratio~fr+as.factor(health_sys)+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
             data=health)
summary(mod4) #health_sys was omitted automatically

# introduce time alg
# 1 year
mod3_fr1 <- felm(inout_ratio~fr1+urban+intermed+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
                data=health)
summary(mod3_fr1) #positive fr and no sig

mod3_fr2 <- felm(inout_ratio~fr2+urban+intermed+log(debt)+log(net_income)+log(tot_hospital_beds)|as.factor(year)+as.factor(country),
                data=health)
summary(mod3_fr2) #negative but no sig











