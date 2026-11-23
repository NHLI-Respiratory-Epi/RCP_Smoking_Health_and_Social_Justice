
library(data.table)
library(dplyr)
library(writexl)
library(parallel)
library(bit64)

# read in relevant smoking obs
smoke <- readRDS("data/smoking_obs_for_current_smokers.RDS")

# read in (potential) denominator
dat_mega <- readRDS("data/current_smokers_denom_unfinalised.RDS")

# read in the quit attempts and successes, and smoking cessation drugs indicate quit attempt
attempt <- readRDS("data/smoking_cessation_quit_attempt.RDS")
success <- readRDS("data/smoking_cessation_success.RDS")
presc <- readRDS("data/extracted_smoking_cessation_drugs_data.RDS")

attempt <- attempt %>% rename(attempt = quit_attempt)
presc <- presc %>% rename(obsdate = issuedate)

table(attempt$attempt)
table(success$success)

# success is a subset of attempt.

# if a quit attempt occurs within one month, then it constitutes the same attempt.
# for quit success it doesn't really matter as we are only taking the first one -
# if they then become a current smoker again afterwards then they are unsuccessful

# therefore any recording of current smoking means that they are a fail
# if they have an ex-smoker code then unless it is followed by a current smoker
# code then it is a success

fail <- smoke[smoking_status == "Current smoker" | smoking_status == "Ex-smoker"]

# for quit attempts, because we are allowing multiple attempts, we need to
# remove the codes that occur within a month

setDT(attempt)
setorder(attempt, patid, obsdate)
attempt[, time_diff := as.numeric(obsdate - shift(obsdate)), by = patid]
attempt <- attempt[is.na(time_diff) | time_diff > 30]

# we will make the cut-off 31 days, so that it captures the 28 week check.

# maybe we need to do it sequentially...

# we have our start date and we have our end date

# and we incorporate our fails

presc$smoking_status <- "Attempt"
presc$presc <- NULL
# then we append

attempt$smoking_status <- "Attempt"
attempt$attempt <- NULL
attempt$time_diff <- NULL

head(success)

success$smoking_status <- "Success"
success$success <- NULL

all <- rbind(attempt, fail, presc, success)

table(all$smoking_status)

# we should also ensure we have our ties correct.

all$smoking_status <- factor(all$smoking_status,
                             levels = c("Current smoker", "Ex-smoker",
                                        "Attempt", "Success"))

setorder(all, patid, obsdate, -smoking_status)

# if there are multiple of the same codes on the same day, we use this order.

all <- all[, .SD[1], by = .(patid, obsdate)]

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

setorder(incy, patid, obsdate)           # sort by patient then date

# 1) Mark only the “quit‐related” rows
markers <- incy[ smoking_status %in% c("Attempt","Ex-smoker","Success") ]

# 2) Tag each marker with an ID + pull out its date & status
markers[, `:=`(
  marker_id     = .I,                  # unique ID for this row
  marker_date   = obsdate,             # the date of the quit‐event
  marker_status = smoking_status       # store the status ("Attempt", etc.)
)]

# 3) Key the full table on patid for fast lookups
setkey(incy, patid)

# 4) For each marker, look forward 1 year (365 days) and decide outcome
outcomes <- markers[, {
  window_end <- marker_date + 365L
  # all events for this patient in (marker_date, marker_date + 365]
  ev <- incy[ patid == .BY$patid &
              obsdate > marker_date &
              obsdate <= window_end ]

  # classify according to your rules:
  oc <- if (marker_status == "Attempt") {
    if      (nrow(ev) == 0L)                                 "unknown"
    else if (any(ev$smoking_status == "Current smoker"))     "unsuccessful"
    else if (any(ev$smoking_status %in% c("Ex-smoker","Success")))
      "successful"
    else                                                      "unknown"
  } else {
    # marker_status is "Ex-smoker" or "Success"
    if (any(ev$smoking_status == "Current smoker"))           "unsuccessful"
    else                                                      "successful"
  }

  .(marker_date, outcome = oc)
}, by = .(patid, marker_id) ]

# 5) Keep only the *first* quit attempt per patient
first_quit <- outcomes[ order(marker_date), .SD[1], by = patid ]

# 6) Build your final table: one row per patient
result <- first_quit[, .(
  patid,
  quit_date    = marker_date,
  quit_outcome = outcome
)]

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
dat[, unknown_result := ifelse(quit_outcome != "unknown" | is.na(quit_outcome), 0, 1)]

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
      n_unknown     <- sum(unknown_result,     na.rm=TRUE)

      # ── crude rates (per person‐year) ──────────────────────────────────────
      rate_total    <- n_total   / py_total
      rate_success  <- n_success / py_total
      rate_failed   <- n_failed  / py_total
      rate_unknown  <- n_unknown / py_total

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
      ci_unknown <- get_ci(n_unknown)

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
        ir_failed_ci_upper     = round(ci_failed[2]  * 1000, 2),

        # unknown outcome
        n_unknown_attempts     = n_unknown,
        incidence_rate_unknown = round(rate_unknown   * 1000, 2),
        ir_unknown_ci_lower    = round(ci_unknown[1] * 1000, 2),
        ir_unknown_ci_upper    = round(ci_unknown[2] * 1000, 2)
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
write_xlsx(results_list,  path = paste0(outpath, dat$year[1], "/smoking_cessation_attempt_results_all_ages_", dat$year[1], ".xlsx"))

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
clusterExport(cl, varlist = c("smoke", "cohorts", "process_fun", "outpath", "all"), envir = environment())

parLapply(cl, split_data, function(dat) {
  process_fun(dat)
})

stopCluster(cl)
