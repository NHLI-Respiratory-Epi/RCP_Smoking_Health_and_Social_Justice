
library(data.table)
library(dplyr)
library(writexl)
library(parallel)
library(bit64)

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

process_fun <- function(dat) {

#----------------------------------------------------------#
# STEP 1: Create the current smoker denominator            #
#----------------------------------------------------------#

 dat[ , `:=`(
  yearstartdate    = pmax(jan01, regstartdate + 90), # 90 day leeway for SS recording
  yearenddate  = pmin(dec31, overallenddate)
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

#------------END OF STEP 1-----------------------------------------------#

#----------------------------Step 2 - apply the incidence numerator----------#

# drug prescription-specific:

# now that we have the baseline, we create the incidence
# for this one, we need the smoking drug prescriptions

# need to link the prescription data with the current smoking data
# and just keep observations that occur within the required time period

presc <- dat[presc, .(patid, yearstartdate, yearenddate, issuedate, presc), nomatch = 0]

presc <- presc[issuedate > yearstartdate & issuedate <= yearenddate]

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

calc_incidence <- function(DT, by_cols, conf.level = 0.95) {
  alpha <- 1 - conf.level

  DT[
    , {
      # - denominators -
      ppl_N_total    <- .N                      # number of persons at risk
      py_total  <- sum(denom_time, na.rm=TRUE)  # total person-years

      # - numerators -
      n_total    <- sum(incidence, na.rm=TRUE)

      # - crude rates  -
      rate_total    <- (n_total   / py_total)

      # - Poisson CIs for the rates -
      total_rate_lo  <- if (n_total == 0) 0 else qchisq(alpha/2,      2*n_total) / (2 * py_total)
      total_rate_hi  <-               qchisq(1-alpha/2, 2*(n_total+1)) / (2 * py_total)

      # - assemble and round to 4 d.p. -
      out <- list(
        ppl_N_total = ppl_N_total,
        person_years_total  = round(py_total,  -1),

        n_total_incidence  = n_total,
        incidence_rate_total  = round(rate_total*1000,   2),
        ir_total_ci_lower     = round(total_rate_lo*1000,2),
        ir_total_ci_upper     = round(total_rate_hi*1000,2)

      )
      out
    },
    by = by_cols
  ]
}

all_combinations <- function(x) {
  n <- length(x)
  # for each size m = 1???n, get combn(x, m) as a list
  combo_list <- lapply(seq_len(n), function(m) {
    combn(x, m, simplify = FALSE)
  })
  # flatten the list-of-lists into one list
  unlist(combo_list, recursive = FALSE)
}

combos <- all_combinations(c("age", "gender", "region", "IMD10", "eth5"))

results_list <- lapply(combos, function(combos) {
  incidence <- calc_incidence(DT = dat, by_cols = combos) %>%
    arrange(across(all_of(combos)))
})

names(results_list) <- sapply(combos, function(x) paste(x, collapse = "_"))

# 6) write them all to one Excel workbook
write_xlsx(results_list,  path = paste0(outpath, dat$year[1], "/smoking_drug_prescription_incidence_results_all_ages_", dat$year[1], ".xlsx"))

}

# 2. Filter `dat` to year 2018

# Number of workers

# I think we probably shouldn't use the entirety of the PCPH computing power
# also, we can only use the number of nodes that reflects the number of elements
# in the list

detectCores()
ncores <- 7

# Create a cluster
cl <- makeCluster(ncores)

# these need to be read into each cluster
clusterEvalQ(cl, {
  library(data.table)
  library(dplyr)
  library(writexl)
  library(bit64)
})

# objects that aren't referenced in apply need to be read into the clusters too
clusterExport(cl, varlist = c("smoke", "cohorts", "process_fun", "outpath", "presc"), envir = environment())

parLapply(cl, split_data, function(dat) {
  process_fun(dat)
})

stopCluster(cl)
