
library(data.table)
library(dplyr)
library(writexl)
library(parallel)
library(bit64)

# read in relevant vaping obs
vape <- readRDS("data/vape_obs.RDS")

# read in (potential) denominator
dat_mega <- readRDS("data/current_vapers_denom_unfinalised.RDS")

# unlike for smoking, this just uses the vaping odds as no vaping cessation codes are used.

all <- vape[vape == "Current vaper" | vape == "Ex-vaper"]

all$vape <- factor(all$vape, levels = c("Current vaper", "Ex-vaper"))

# on closer inspection, lots of times people are coded as 'electronic cigarette user' and on the same day as 'ex-user'.
# for this reason, ex-user takes precedence.

setorder(all, patid, obsdate, -vape)

# if there are multiple of the same codes on the same day, we use this order.

all <- all[, .SD[1], by = .(patid, obsdate)]

table(all$vape)

# now we will only have one code per day so we don't need to worry about
# ordering by smoking status any more.

# quit success > quit attempt > ex-smoker > current smoker

# difficult to calculate the number of quit attempts

# I think we can only calculate the quit attempt once,
# because we don't know how long the patient was at risk
# for and when/if they relapsed

# so we have the following options: attempt, success, fail
# let's map out every single combination

# attempt-X-X: quit attempt - unknown result
# success-X-X: quit attempt - success
# fail-X-X:    no attempt
# attempt-success-X: quit attempt - success
# attempt-fail-X: quit attempt - fail
# success-attempt-X: quit attempt - success
# success-fail-X: quit attempt - fail
# attempt-success-fail: quit attempt - fail

# we reduce to only the options above

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
vape <- vape[!is.na(vape)]

setkey(vape, patid)
setkey(dat, patid)

# then link
vape <- dat[vape, nomatch = 0]

# now we split into the baseline assessment and the incidence assessment

baseline <- vape[obsdate < yearstartdate]

# take the most recent smoking status as read.
# if it is never smoker, check for any previous smoker codes.
# if it is non-smoker or ever-smoker, look at other previous codes

setorder(baseline, patid, -obsdate) # order the data table

# slice in data table - we only want the current smokers
baseline <- baseline[ , head(.SD, 1), by = patid ]

baseline <- baseline[vape == "Current vaper"]

# and we then just restrict out population to smokers
baseline <- baseline[, .(patid)]
dat <- baseline[dat, nomatch = 0]

#------------END OF STEP 1-----------------------------------------------#

#----------------------------Step 2 - apply the incidence numerator----------#

# quit attempts

# now that we have the baseline, we create the incidence

# we can combine this with the incidence data table we have created
incy <- dat[, .(patid, yearstartdate, yearenddate)]

setkey(incy, patid)
setkey(all, patid)

incy <- incy[all, nomatch = 0]
incy <- incy[obsdate >= yearstartdate]
incy <- incy[obsdate <= (yearenddate + 365)]

# if the only observations consist of current smoking, we can get rid of them
# likewise, we can get rid of any current smoker observations before the
# quit attempt

# from chat gpt:

library(data.table)

setorder(incy, patid, obsdate)

# 1) only keep the “quit markers” (Ex-vaper)
markers_vape <- incy[
  vape == "Ex-vaper",
  .(patid, marker_date = obsdate)
]
markers_vape[, marker_id := .I]    # unique ID for each quit‐marker

# 2) key on patid for fast sub‐setting
setkey(incy, patid)

# 3) for each marker, look 1 year ahead and classify
outcomes_vape <- markers_vape[, {
  window_end <- marker_date + 365L
  # all events in (marker_date, window_end]
  ev <- incy[patid == .BY$patid &
               obsdate > marker_date &
               obsdate <= window_end]

  # if ANY Current-vaper appears → unsuccessful, else → successful
  oc <- if (any(ev$vape == "Current vaper"))
    "unsuccessful"
  else
    "successful"

  .(marker_date, outcome = oc)
}, by = .(patid, marker_id)]

# 4) if you only want each patient’s first quit‐marker:
first_vape <- outcomes_vape[
  order(marker_date),
  .SD[1],
  by = patid
][, .(patid, quit_date = marker_date, quit_outcome = outcome)]

result <- first_vape

# I have extensively checked this and it seems to work well.

# we join it to incy

setkey(result, patid)

incy <- result[incy, nomatch = 0]
incy <- incy[quit_date < yearenddate]

incy <- incy[, .(patid, quit_date, quit_outcome)]
incy <- incy[, by = patid, .SD[1]]

incy <- incy %>% rename(incidence_date = quit_date)

#----------------step 2 complete!-----------------------------------#

#----------------Step 3 - generic incidence calculation-------------#

# put these back into the denominator dataset

dat <- incy[dat]

dat[, incidence := ifelse(is.na(incidence_date), 0, 1)]
dat[incidence == 1, yearenddate := incidence_date]

dat[, denom_time := as.numeric(yearenddate - yearstartdate)/365.25]

dat[, successful_attempt := ifelse(quit_outcome != "successful" | is.na(quit_outcome), 0, 1)]
dat[, failed_attempt := ifelse(quit_outcome != "unsuccessful" | is.na(quit_outcome), 0, 1)]

# 2) helper to compute counts and prevalences by arbitrary group

calc_incidence <- function(DT, by_cols, conf.level = 0.95) {
  alpha <- 1 - conf.level

  DT[
    , {
      # ── denominators ───────────────────────────────────────────────────────
      ppl_N_total   <- .N                       # number of persons at risk
      py_total      <- sum(denom_time, na.rm=TRUE)  # total person‐years

      # ── numerators ─────────────────────────────────────────────────────────
      n_total       <- sum(incidence,         na.rm=TRUE)
      n_success     <- sum(successful_attempt, na.rm=TRUE)
      n_failed      <- sum(failed_attempt,     na.rm=TRUE)

      # ── crude rates (per person‐year) ──────────────────────────────────────
      rate_total    <- n_total   / py_total
      rate_success  <- n_success / py_total
      rate_failed   <- n_failed  / py_total

      # ── Poisson CIs for each rate ──────────────────────────────────────────
      # helper: lower = 0 if count==0
      get_ci <- function(n) {
        lo <- if (n == 0) 0 else qchisq(alpha/2,      2*n)     / (2 * py_total)
        hi <-                qchisq(1-alpha/2, 2*(n+1)) / (2 * py_total)
        c(lo, hi)
      }

      ci_total   <- get_ci(n_total)
      ci_success <- get_ci(n_success)
      ci_failed  <- get_ci(n_failed)

      # ── assemble & round to sensible precision ──────────────────────────────
      out <- list(
        # denominators
        ppl_N_total            = ppl_N_total,
        person_years_total     = py_total,

        # overall incidence
        n_total_incidence      = n_total,
        incidence_rate_total   = round(rate_total   * 1000, 2),
        ir_total_ci_lower      = round(ci_total[1] * 1000, 2),
        ir_total_ci_upper      = round(ci_total[2] * 1000, 2),

        # successful quits
        n_successful_attempts      = n_success,
        incidence_rate_successful  = round(rate_success   * 1000, 2),
        ir_successful_ci_lower     = round(ci_success[1] * 1000, 2),
        ir_successful_ci_upper     = round(ci_success[2] * 1000, 2),

        # failed quits
        n_failed_attempts      = n_failed,
        incidence_rate_failed  = round(rate_failed    * 1000, 2),
        ir_failed_ci_lower     = round(ci_failed[1]  * 1000, 2),
        ir_failed_ci_upper     = round(ci_failed[2]  * 1000, 2)

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
write_xlsx(results_list,  path = paste0(outpath, dat$year[1], "/vaping_cessation_attempt_results_all_ages_", dat$year[1], ".xlsx"))

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
clusterExport(cl, varlist = c("vape", "cohorts", "process_fun", "outpath", "all"), envir = environment())

parLapply(cl, split_data, function(dat) {
  process_fun(dat)
})

stopCluster(cl)
