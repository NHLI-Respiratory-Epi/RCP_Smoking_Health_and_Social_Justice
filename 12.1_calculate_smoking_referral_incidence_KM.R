
library(data.table)
library(dplyr)
library(writexl)
library(parallel)
library(bit64)
library(survival)
library(survminer)

# read in relevant smoking obs
smoke <- readRDS("data/smoking_obs_for_current_smokers.RDS")

# read in (potential) denominator
dat_mega <- readRDS("data/current_smokers_denom_unfinalised.RDS")

# read in the referrals
refer <- readRDS("data/smoking_cessation_referral_or_counselling.RDS")
refer <- refer %>% rename(refer = referral_enrol)
setDT(dat_mega)
setDT(smoke)
setDT(refer)

# then we sort out some code so it runs in parallel for the multiple yearly cohorts

cohorts <- c(2018:2024)
outpath <- "output/incidences/"

split_data <- lapply(cohorts,
                     function(i) dat_mega[year == i])

#process_fun <- function(dat) {

dat <- split_data[[5]]

#----------------------------------------------------------#
# STEP 1: Create the current smoker denominator            #
#----------------------------------------------------------#

# we are actually keeping people until the end now, instead of just the year.

head(dat, 50)

 dat[ , `:=`(
  yearstartdate    = pmax(jan01, regstartdate + 90), # 90 day leeway for SS recording
  yearenddate  = pmin(overallenddate) # their end date is just overall end date
),
by = patid
]

# drop those whose end date is before start date
dat <- dat[yearstartdate < yearenddate]
  #browser()
# for incidence we need to calculate the start and end date separately
# it is no longer simply whether they are in the dataset at the midpoint
# or not

# we do need to separate out the data into the start and end dates

# drop missing statuses
smoke <- smoke[!is.na(smoking_status)]

setkey(smoke, patid)
setkey(dat, patid)

# then link
smoke <- dat[smoke, nomatch = 0]

# now we split into the baseline assessment and the incidence assessment

baseline <- smoke[obsdate < yearstartdate]

# take the most recent smoking status as read.
# if it is never smoker, check for any previous smoker codes.
# if it is non-smoker or ever-smoker, look at other previous codes

setorder(baseline, patid, -obsdate) # order the data table

# slice in data table - we only want the current smokers
baseline <- baseline[ , head(.SD, 1), by = patid ]

baseline <- baseline[smoking_status == "Current smoker"]

# and we then just restrict out population to smokers
baseline <- baseline[, .(patid)]
dat <- baseline[dat, nomatch = 0]

# we also need to remove those who were prescribed

#------------END OF STEP 1-----------------------------------------------#

#----------------------------Step 2 - prescriptions Kaplan Meier----------#

# drug prescription-specific:

# now that we have the baseline, we create the incidence
# for this one, we need the smoking drug prescriptions

# need to link the prescription data with the current smoking data
# and just keep observations that occur within the required time period

refer <- dat[refer, .(patid, yearstartdate, yearenddate, obsdate, refer), nomatch = 0]

refer <- refer[obsdate > yearstartdate]
refer <- refer[obsdate <= yearenddate]

setorder(refer, patid, -obsdate) # order the data table

refer <- refer[ , head(.SD, 1), by = patid ]

incy <- refer[, .(patid, incidence_date = obsdate)]
setkey(incy, patid)

refer %>% group_by(obsdate) %>% summarise(n=n()) %>% arrange(-n) %>% print(n=300)
refer %>% group_by(obsdate) %>% summarise(n=n()) %>% pull(n) %>% hist(., breaks = 1000)
refer %>% group_by(obsdate) %>% summarise(n=n()) %>% filter(n<2000) %>% pull(n) %>% boxplot()

refer %>% pull(obsdate) %>% hist(., breaks = 40)
refer %>% pull(obsdate) %>% hist(., breaks = 40)

#----------------step 2 complete!-----------------------------------#

#----------------Step 3 - generic incidence calculation-------------#

# put these back into the denominator dataset

dat <- incy[dat]

dat[, incidence := ifelse(is.na(incidence_date), 0, 1)]
dat[incidence == 1, yearenddate := incidence_date]

dat[, denom_time := as.numeric(yearenddate - yearstartdate)/365.25]

head(dat)

# 2) helper to compute counts and prevalences by arbitrary group

dat <- readRDS("data/data_for_KM_for_referral.RDS")

# Fit the same model

summary(dat$IMD5)
setwd("output/plots/")

km_fit2 <- survfit(Surv(denom_time, incidence) ~ IMD5, data = dat[IMD5 != "IMD unavailable"])

# Use ggsurvplot for a ggplot2-style output
ggsurvplot(
  km_fit2,
  data = dat[IMD10 != "IMD unavailable"],
 # risk.table = TRUE,        # show number at risk table
#  pval = TRUE,              # show log-rank p-value
#  conf.int = TRUE,          # add confidence intervals
 # palette = c("#2E9FDF", "#E7B800"),
#  legend.labs = c("Male", "Female"),
  xlab = "Time (years)",
  ylab = "Non-receipt of referral",
  title = "Kaplan–Meier Curve: referral by IMD",
ylim = c(0.9, 1),
xlim = c(0, 2.8),
censor = FALSE,
break.time.by = 0.5
)

ggsave("KM - time to referral - IMD5.pdf", width = 4000, height = 2000, units = "px")

IMD15referral <- dat[age > 17 & IMD5 %in% c(1, 5)]

IMD15referral$IMD5 <- as.character(IMD15referral$IMD5)
IMD15referral$IMD5[IMD15referral$IMD5 == 1] <- "1 (least deprived)"
IMD15referral$IMD5[IMD15referral$IMD5 == 5] <- "5 (most deprived)"
IMD15referral$IMD5 <- factor(IMD15referral$IMD5, levels = c("1 (least deprived)", "5 (most deprived)"))

IMD15referral

km_fit2 <- survfit(Surv(denom_time, incidence) ~ IMD5, data = IMD15referral)
summary(km_fit2)
str(km_fit2)

quantile(km_fit2, 0.05)
summary(km_fit2, times = 2)

km_fit2
library(survminer)
library(scales)

# 1) First draw the KM plot with 2% y-breaks between 0.90 and 1.00
surv_out <- ggsurvplot(
  km_fit2,
  data        = IMD15referral,
  xlab        = "Time (years)",
  ylab        = "Non-receipt of referral",
  title       = "Figure 5. Time to referral for smoking cessation for current smokers in 2022",
  ylim        = c(0.90, 1.00),
  xlim        = c(0, 2.8),
  break.time.by = 0.5,
  break.y.by    = 0.02,
  censor      = FALSE,
  linetype    = "solid",
  ggtheme     = theme_minimal()
)

surv_out$plot <- surv_out$plot +
  scale_x_continuous(
    name         = "Time (years)",
    limits       = c(0, 2.8),
    expand       = c(0, 0),
    breaks       = seq(0, 2.8, by = 0.5),
    minor_breaks = NULL
  ) +
  scale_y_continuous(
    name         = "Percentage of current smokers who have not been referred",
    labels       = scales::percent_format(accuracy = 1),
    breaks       = seq(0.90, 1.00, by = 0.02),
    minor_breaks = NULL,
    limits       = c(0.90, 1.00),
    expand       = c(0, 0)
  ) +
  theme(
    panel.grid.major = element_blank(),   # remove major grid lines
    panel.grid.minor = element_blank(),   # remove minor grid lines
    axis.line        = element_line(color = "black"),  # draw solid axes
    axis.ticks.length = unit(4, "pt")     # optional: lengthen tick marks
  )

print(surv_out)

# 6) Save
ggsave(
  "output/plots/",
  plot  = surv_out$plot,
  width = 3000,
  height = 1500,
  units = "px"
)

print(surv_out)

# 2) Convert the y‐axis to percentages and tweak the label
surv_out$plot <- surv_out$plot +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),     # e.g. "90%", "92%", …
    breaks = seq(0.90, 1.00, by = 0.02),        # every 2%
    limits = c(0.90, 1.00)
  ) +
  labs(
    y = "Percentage of current smokers who have not been referred"
  ) + theme_bw()

# 3) Draw it (this will also update the risk table if you’re using it)
print(surv_out)

# Use ggsurvplot for a ggplot2-style output
ggsurvplot(
  km_fit2,
  data = dat[IMD10 != "IMD unavailable"],
  # risk.table = TRUE,        # show number at risk table
  #  pval = TRUE,              # show log-rank p-value
   # conf.int = TRUE,          # add confidence intervals
  # palette = c("#2E9FDF", "#E7B800"),
  #  legend.labs = c("Male", "Female"),
  xlab = "Time (years)",
  ylab = "Non-receipt of referral",
  title = "Kaplan–Meier Curve: referral for smoking cessation",
  ylim = c(0.9, 1),
  xlim = c(0, 2.8),
#  linetype = "dashed",
  censor = FALSE,
linetype = "solid",
break.time.by = 0.5

)

head(dat)

ggsave("KM - time to referral - IMD1vs5.pdf", width = 4000, height = 2000, units = "px")

km_fit2 <- survfit(Surv(denom_time, incidence) ~ region, data = dat[region != "Northern Ireland"  & !is.na(IMD5)])

# Use ggsurvplot for a ggplot2-style output
ggsurvplot(
  km_fit2,
  data = dat[region != "Northern Ireland"],
  # risk.table = TRUE,        # show number at risk table
  #  pval = TRUE,              # show log-rank p-value
  # conf.int = TRUE,          # add confidence intervals
  # palette = c("#2E9FDF", "#E7B800"),
  #  legend.labs = c("Male", "Female"),
  xlab = "Time (years)",
  ylab = "Non-receipt of referral",
  title = "Kaplan–Meier Curve: referral by region",
  ylim = c(0.8, 1),
  xlim = c(0, 2.8),
  #  linetype = "dashed",
  censor = FALSE,
  linetype = "solid",
  break.time.by = 0.5
)

table(dat$region, dat$incidence)

ggsave("KM - time to referral - region.pdf", width = 4000, height = 2000, units = "px")

library(survival)
library(dplyr)

# 1. Fit your KM by region (replace with your actual data & formula)
#    I’ll assume you have something like:
#    mydata$time  # numeric follow‐up in years
#    mydata$event # 0/1, 1=referral (i.e. “failure”)
#    mydata$region

# 2. Pull out the step‐function internals via summary()
sf <- summary(km_fit2)

# 3. Expand that into a data.frame with one row per (stratum × time‐point)
stratum_labels <- rep(names(sf$strata), sf$strata)
km_df <- data.frame(
  region   = sf$strata, #stratum_labels,
  time     = sf$time,      # time of each step
  n.risk   = sf$n.risk,    # number at risk just before that time
  n.event  = sf$n.event,   # number of events at exactly that time
  n.censor = sf$n.censor,  # number of censored at that time
  surv     = sf$surv       # survival probability just after
)

# 4. Calculate the size of each drop (as a proportion)
km_df <- km_df %>%
  group_by(region) %>%
  arrange(region, time) %>%
  mutate(
    prev_surv  = lag(surv, default = 1),       # assume 1 at t=0
    drop       = prev_surv - surv,             # absolute drop in surv
    event_rate = n.event / n.risk               # proportion failing at that time
  ) %>%
  ungroup()

# 5. Inspect the biggest drops across all regions
big_drops <- km_df %>%
  arrange(desc(drop)) %>%
  slice_head(n = 10)

print(big_drops)

str(sf)

# by ethnicity

km_fit2 <- survfit(Surv(denom_time, incidence) ~ eth5, data = dat[region != "Northern Ireland"])

# Use ggsurvplot for a ggplot2-style output
ggsurvplot(
  km_fit2,
  data = dat[region != "Northern Ireland"],
  # risk.table = TRUE,        # show number at risk table
  #  pval = TRUE,              # show log-rank p-value
  # conf.int = TRUE,          # add confidence intervals
  # palette = c("#2E9FDF", "#E7B800"),
  #  legend.labs = c("Male", "Female"),
  xlab = "Time (years)",
  ylab = "Non-receipt of referral",
  title = "Kaplan–Meier Curve: referral by region",
 # ylim = c(0.95, 1),
  xlim = c(0, 2.8),
  #  linetype = "dashed",
  censor = FALSE,
  linetype = "solid",
  break.time.by = 0.5
)

ggsave("KM - time to referral - ethnicity.pdf", width = 4000, height = 2000, units = "px")

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
dat %>% group_by(region) %>% summarise(denom_time = mean(denom_time), denom_mode = Mode(denom_time[denom_time != max(denom_time)]))

summary(dat$denom_time)
dat %>% filter(denom_time > 5)
dat %>% filter(incidence_date > overallenddate) %>% nrow()

# inspect....


smoking_cessation <- readRDS("data/smoking_cessation_PRELIM.RDS")

refer_inspect <- smoking_cessation %>%
  filter(smoking_cessation %in% c("Enrolled in smoking cessation programme",
                                  "Referral for smoking cessation",
                                  "Smoking cessation counselling",
                                  "Smoking cessation behavioural therapy"))

refer_inspect[obsdate == "2022-11-17", .(N = .N), by = .(smoking_cessation)]
refer_inspect[obsdate == "2022-11-17", .(N = .N), by = .(term)]

lookat <- refer_inspect[obsdate == "2022-11-17" & term == "Referral to smoking cessation service"]

setkey(lookat, patid)
setkey(dat, patid)

lookat <- lookat[dat, nomatch = 0]

table(lookat$pracid, lookat$region)

colnames(dat)

colnames(lookat)
summary(lookat) # that was for london. we can see steps but there are so many people it is not that noticeable. Now we try the other one.

refer_inspect[obsdate == "2022-05-27", .(N = .N), by = .(smoking_cessation)]
refer_inspect[obsdate == "2022-05-27", .(N = .N), by = .(term)]

lookat <- refer_inspect[obsdate == "2022-05-27" & term == "Referral to smoking cessation advisor"]

setkey(lookat, patid)
setkey(dat, patid)

lookat <- lookat[dat, nomatch = 0]

table(lookat$pracid, lookat$region)

summary(lookat)

