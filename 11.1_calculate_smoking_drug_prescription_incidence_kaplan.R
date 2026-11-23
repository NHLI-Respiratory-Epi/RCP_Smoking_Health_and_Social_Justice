
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

# read in the prescriptions
presc <- readRDS("data/extracted_smoking_cessation_drugs_data.RDS")

setDT(dat_mega)
setDT(smoke)
setDT(presc)

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

setkey(presc, patid)
setkey(dat, patid)

presc <- dat[presc, .(patid, yearstartdate, yearenddate, issuedate, presc), nomatch = 0]

# presc_baseline <- presc[issuedate <= yearstartdate]
# setorder(presc_baseline, patid, -issuedate) # order the data table
# presc_baseline <- presc_baseline[ , head(.SD, 1), by = patid]
# presc_baseline[, baseline_time_from_presc_to_start := as.numeric(yearstartdate - issuedate)]
#
# # exclude those who have drugs prescribed within 1 year of start date
# presc_baseline <- presc_baseline[baseline_time_from_presc_to_start > 365]

# summary(presc_baseline$baseline_time_from_presc_to_start)

presc <- presc[issuedate > yearstartdate]
presc <- presc[issuedate <= yearenddate]

setorder(presc, patid, -issuedate) # order the data table

presc <- presc[ , head(.SD, 1), by = patid ]

incy <- presc[, .(patid, incidence_date = issuedate)]

setkey(incy, patid)

#----------------step 2 complete!-----------------------------------#

#----------------Step 3 - generic incidence calculation-------------#

# put these back into the denominator dataset

dat <- incy[dat]

dat[, incidence := ifelse(is.na(incidence_date), 0, 1)]
dat[incidence == 1, yearenddate := incidence_date]

dat[, denom_time := as.numeric(yearenddate - yearstartdate)/365.25]

head(dat)

# 2) helper to compute counts and prevalences by arbitrary group

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
  ylab = "Non-receipt of prescription",
  title = "Kaplan–Meier Curve: prescription by IMD",
ylim = c(0.95, 1),
xlim = c(0, 2.8),
censor = FALSE,
break.time.by = 0.5
)

ggsave("KM - time to prescription - IMD5.pdf", width = 4000, height = 2000, units = "px")

km_fit2 <- survfit(Surv(denom_time, incidence) ~ IMD5, data = dat[IMD5 %in% c(1, 5)])

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
  ylab = "Non-receipt of prescription",
  title = "Kaplan–Meier Curve: prescription by IMD",
  ylim = c(0.95, 1),
  xlim = c(0, 2.8),
#  linetype = "dashed",
  censor = FALSE,
linetype = "solid",
break.time.by = 0.5
)

ggsave("KM - time to prescription - IMD1vs5.pdf", width = 4000, height = 2000, units = "px")

km_fit2 <- survfit(Surv(denom_time, incidence) ~ region, data = dat[region != "Northern Ireland"])

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
  ylab = "Non-receipt of prescription",
  title = "Kaplan–Meier Curve: prescription by region",
  ylim = c(0.95, 1),
  xlim = c(0, 2.8),
  #  linetype = "dashed",
  censor = FALSE,
  linetype = "solid",
  break.time.by = 0.5
)

ggsave("KM - time to prescription - region.pdf", width = 4000, height = 2000, units = "px")

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
  title = "Kaplan–Meier Curve: prescription by region",
  ylim = c(0.95, 1),
  xlim = c(0, 2.8),
  #  linetype = "dashed",
  censor = FALSE,
  linetype = "solid",
  break.time.by = 0.5
)

ggsave("KM - time to prescription - ethnicity.pdf", width = 4000, height = 2000, units = "px")

