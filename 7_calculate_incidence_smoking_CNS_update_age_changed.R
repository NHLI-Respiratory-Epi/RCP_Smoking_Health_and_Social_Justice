
library(data.table)
library(dplyr)
library(writexl)
library(parallel)
library(bit64)

# update this when we have all the data:
smoke <- readRDS("data/extracted_smoking_data_CNS_update.RDS")

# we are going to trim the denominator down for ease in developing the script
# I only ran this once to create the testing denominator, then hashed it out.
# when we have all the data we will just read in the full denominator.

# # unique(dat$pracid) %>% nchar() %>% table() # all pracids 5 characters
#
# restrict_to_pracid <- unique(str_sub(unique(smoke$patid), -5, -1))
# dat <- dat %>% filter(pracid %in% restrict_to_pracid)
#
# dat %>% select(patid) %>% unique() %>% nrow()
# smoke %>% select(patid) %>% unique() %>% nrow()
#

# update this when we have all the data:
dat_mega <- readRDS("data/cleaned_denominator_final.RDS")

# done!

# we start off with the most simple process - calculating prevalence.
# we need smoking status at the middle timepoint

# easiest to reduce the denominator file to the yearly segments.

# 1. Convert to data.tables (if not already)
setDT(dat_mega)
setDT(smoke)

# then we sort out some code so it runs in parallel for the multiple yearly cohorts

cohorts <- c(2018:2024)
outpath <- "output/incidences/"

split_data <- lapply(cohorts,
                     function(i) dat_mega[year == i])

process_fun <- function(dat) {
# dat <- split_data[[3]] # HASH OUT WHEN FUNCTION IS READY

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

# 4b. ...drop missing statuses
smoke <- smoke[!is.na(smoking_status)]

# 4c. ...keep only the three columns you need
smoke <- smoke[, .(patid, obsdate, smoking_status)]

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

# 1) for each patient, order so the first row is the latest obs, and flag if ex- or -ever

baseline[ , `:=`(
  had_ex    = any(smoking_status == "Ex-smoker"),
  had_ever  = any(smoking_status == "Ever-smoker")
),
by = patid
]

# then we drop the ever smoker and non-smoker codes

# smoke %>% filter(!(smoking_status %in% c("Ever-smoker", "Non-smoker"))) %>%
#   group_by(patid) %>% slice(1) %>% nrow() # 11 people missed... no problem. will be classed as 'never'

baseline <- baseline[!(smoking_status %in% c("Ever-smoker", "Non-smoker"))]

# slice in data table
baseline <- baseline[ , head(.SD, 1), by = patid ]

# OGs <- smoke %>% filter(smoking_status == "Ex-smoker")
# table(smoke$smoking_status)

# 4) recode only those whose latest is Never but had_ex == TRUE
baseline[
  smoking_status == "Never smoker" & (had_ex | had_ever),
  smoking_status := "Ex-smoker"
]

# NEWGs <- smoke %>% filter(smoking_status == "Ex-smoker") %>% filter(!(patid %in% OGs$patid))
# table(smoke$smoking_status)

# this is just looking at who is recoded - current non-smoker is 'ex-smoker' according to QOF

#
# check <- check %>% filter(obsdate <= cutoff_date) %>% filter(patid %in% NEWGs$patid) %>% arrange(patid, obsdate)
#

# table(smoke$smoking_status)
#
# # For the purposes of QOF, 137L (current non-smoker) is an ex-smoker code.

# therefore, we have smoking status

# we only need to keep people with a current or ex status, everyone else is missing.
baseline <- baseline[smoking_status %in% c("Current smoker", "Ex-smoker")]

# and we just need these columns
baseline <- baseline[, .(patid, smoking_status)]

# and then we save it.

# then apply this back to dat

dat <- baseline[dat]

dat[is.na(smoking_status),
    smoking_status := "Never smoker"
]

# because we are doing incidence, we need to remove the current smokers

dat <- dat[smoking_status != "Current smoker"]

# now that we have the baseline, we create the incidence

incy <- smoke[smoking_status == "Current smoker"]

incy <- incy[obsdate > yearstartdate & obsdate <= yearenddate]

setorder(incy, patid, -obsdate) # order the data table

incy <- incy[ , head(.SD, 1), by = patid ]

summary(incy$obsdate)

# most people have genuinely been registered for longer than a year
# incy %>% filter(obsdate - regstartdate > 365) %>% nrow()

incy <- incy[, .(patid, incidence_date = obsdate)]

head(incy)

# put these back into the denominator dataset

dat <- incy[dat]

dat[, incidence := ifelse(is.na(incidence_date), 0, 1)]
dat[incidence == 1, yearenddate := incidence_date]

dat[, denom_time := as.numeric(yearenddate - yearstartdate)/365.25]

head(dat)

# 2) helper to compute counts and prevalences by arbitrary group

calc_prev <- function(DT, by_cols, conf.level = 0.95) {
  alpha <- 1 - conf.level
  z     <- qnorm(1 - alpha/2)

  DT[
    , {
      # 1) counts
      n_ex    <- sum(smoking_status == "Ex-smoker",     na.rm=TRUE)
      n_curr  <- sum(smoking_status == "Current smoker", na.rm=TRUE)
      n_never <- sum(smoking_status == "Never smoker",   na.rm=TRUE)
      n_tot   <- .N

      # # 2) proportions
      p_ex    <- n_ex   / n_tot
      p_curr  <- n_curr / n_tot
      p_never <- n_never / n_tot

      # 3) standard errors
      se_ex    <- sqrt(p_ex    * (1 - p_ex)    / n_tot)
      se_curr  <- sqrt(p_curr  * (1 - p_curr)  / n_tot)
      se_never <- sqrt(p_never * (1 - p_never) / n_tot)

      # 4) Wald CIs, clamped to [0,1]
      ex_lo    <- pmax(0, p_ex    - z * se_ex)
      ex_hi    <- pmin(1, p_ex    + z * se_ex)
      curr_lo  <- pmax(0, p_curr  - z * se_curr)
      curr_hi  <- pmin(1, p_curr  + z * se_curr)
      never_lo <- pmax(0, p_never - z * se_never)
      never_hi <- pmin(1, p_never + z * se_never)

      # 5) assemble output: count, prop + CI, prop + CI, ...
      out <- list(
        n_total      = n_tot,

        n_ex         = n_ex,
        prev_ex      = p_ex,
        ex_ci_lower  = ex_lo,
        ex_ci_upper  = ex_hi,

        n_current    = n_curr,
        prev_current = p_curr,
        curr_ci_lower= curr_lo,
        curr_ci_upper= curr_hi,

        n_never      = n_never,
        prev_never   = p_never,
        never_ci_lower = never_lo,
        never_ci_upper = never_hi
      )

      # 6) round every numeric element to 4 d.p.
      out <- lapply(out, function(x) if (is.numeric(x)) round(x, 4) else x)
      out

    },
    by = by_cols
  ]
}

calc_incidence <- function(DT, by_cols, conf.level = 0.95) {
  alpha <- 1 - conf.level

  DT[
    , {
      # - denominators -
      ppl_N_total    <- .N                      # number of persons at risk
      ppl_N_ex    <- sum(smoking_status == "Ex-smoker", na.rm = TRUE)                      # number of persons at risk
      ppl_N_never    <- sum(smoking_status == "Never smoker", na.rm = TRUE)                      # number of persons at risk
      py_total  <- sum(denom_time, na.rm=TRUE)  # total person-years
      py_ex  <- sum(denom_time[smoking_status == "Ex-smoker"], na.rm=TRUE)
      py_never  <- sum(denom_time[smoking_status == "Never smoker"], na.rm=TRUE)

      # - numerators -
      n_total    <- sum(incidence, na.rm=TRUE)
      n_ex    <- sum(incidence[smoking_status == "Ex-smoker"], na.rm=TRUE)
      n_never    <- sum(incidence[smoking_status == "Never smoker"], na.rm=TRUE)

      # - crude rates  -
      rate_total    <- (n_total   / py_total)
      rate_ex  <- (n_ex / py_ex)
      rate_never <- (n_never/ py_never)

      # - Poisson CIs for the rates -
      total_rate_lo  <- if (n_total == 0) 0 else qchisq(alpha/2,      2*n_total) / (2 * py_total)
      total_rate_hi  <-               qchisq(1-alpha/2, 2*(n_total+1)) / (2 * py_total)
      ex_rate_lo    <- if (n_ex   == 0) 0 else qchisq(alpha/2,      2*n_ex)   / (2 * py_ex)
      ex_rate_hi    <-               qchisq(1-alpha/2, 2*(n_ex+1)) / (2 * py_ex)
      never_rate_lo <- if (n_never== 0) 0 else qchisq(alpha/2,      2*n_never)/ (2 * py_never)
      never_rate_hi <-               qchisq(1-alpha/2, 2*(n_never+1))/(2 * py_never)

      # - assemble and round to 4 d.p. -
      out <- list(
        ppl_N_total = ppl_N_total,
        person_years_total  = py_total,

        n_total_incidence  = n_total,
        incidence_rate_total  = round(rate_total*1000,   2),
        ir_total_ci_lower     = round(total_rate_lo*1000,2),
        ir_total_ci_upper     = round(total_rate_hi*1000,2),

        ppl_N_ex = ppl_N_ex,
        person_years_ex  = py_ex,

        n_ex_incidence       = n_ex,
        incidence_rate_ex    = round(rate_ex*1000,   2),
        ir_ex_ci_lower       = round(ex_rate_lo*1000, 2),
        ir_ex_ci_upper       = round(ex_rate_hi*1000, 2),

        ppl_N_never = ppl_N_never,
        person_years_never  = py_never,

        n_never_incidence    = n_never,
        incidence_rate_never = round(rate_never*1000,   2),
        ir_never_ci_lower    = round(never_rate_lo*1000,2),
        ir_never_ci_upper    = round(never_rate_hi*1000,2)

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

combos <- all_combinations(c("age", "gender", "region", "IMD10"))

results_list <- lapply(combos, function(combos) {
  incidence <- calc_incidence(DT = dat, by_cols = combos) %>%
    arrange(across(all_of(combos)))
})

names(results_list) <- sapply(combos, function(x) paste(x, collapse = "_"))

# 6) write them all to one Excel workbook
write_xlsx(results_list,  path = paste0(outpath, dat$year[1], "/smoking_incidence_results_all_ages_", dat$year[1], ".xlsx"))

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
clusterExport(cl, varlist = c("smoke", "cohorts", "process_fun", "outpath"), envir = environment())

parLapply(cl, split_data, function(dat) {
  process_fun(dat)
})

stopCluster(cl)
