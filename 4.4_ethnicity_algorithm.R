library(tidyverse)
library(data.table)

# Ethnicity algorithm

# use the most commonly recorded ethnicity.
# if multiple ethnicities are the most common, use the most recent valid ethnicity

dat <- read_rds("data/extracted_ethnicity_data.RDS")

head(dat)

nrow(dat)

length(unique(dat$patid))

# I don't see a point in restricting the observation dates for ethnicity
# might as well use all the data - the only difference will be the accuracy of the data.
# There shouldn't be any issues with immortal time bias.

table(dat$eth19)
table(dat$eth19_grouped)

# 1) Remove any rows where eth19 == "not stated" (case???insensitive)
dt <- dat[eth19 != "Not Stated"]

# 2) For each (patid, eth19), compute:
#      - count     = number of times that eth19 appears for that patid
#      - last_date = the most recent obsdate when that eth19 was coded
dt_summary19 <- dt[ , .(
  count     = .N,
  last_date = max(obsdate)
), by = .(patid, eth19)]

# 3) Within each patid, figure out:
#      - max_count = maximum of count across all eth19 for that patid
#      - n_ties    = how many ethnicities share that max_count
dt_summary19[ , `:=`(
  max_count = max(count),
  n_ties    = sum(count == max(count))
), by = patid]

dt_final19 <- dt_summary19[
  count == max_count,
  {
    i <- if (.N > 1L) which.max(last_date) else 1L
    .(ethnicity = eth19[i],
      method    = ifelse(.N > 1L, "recent", "mode"))
  },
  by = patid
]

dt_final19 <- dt_final19 %>% rename(eth19 = ethnicity, eth19_meth = method)

# 2) For each (patid, eth19), compute:
#      - count     = number of times that eth19 appears for that patid
#      - last_date = the most recent obsdate when that eth19 was coded
dt_summary5 <- dt[ , .(
  count     = .N,
  last_date = max(obsdate)
), by = .(patid, eth19_grouped)]

# 3) Within each patid, figure out:
#      - max_count = maximum of count across all eth19 for that patid
#      - n_ties    = how many ethnicities share that max_count
dt_summary5[ , `:=`(
  max_count = max(count),
  n_ties    = sum(count == max(count))
), by = patid]

dt_final5 <- dt_summary5[
  count == max_count,
  {
    i <- if (.N > 1L) which.max(last_date) else 1L
    .(ethnicity = eth19_grouped[i],
      method    = ifelse(.N > 1L, "recent", "mode"))
  },
  by = patid
]

dt_final5 <- dt_final5 %>% rename(eth5 = ethnicity, eth5_meth = method)

# add into one data table

final <- dt_final5[dt_final19, on = "patid"]

table(final$eth19)
table(final$eth19_meth)
table(final$eth5)
table(final$eth5_meth)

table(final$eth19, final$eth5)

# almost everyone seems pretty consistent. Almost everyone is identified by the mode,
# not the most recent date.

# difficult to tell what is missing because the code is missing and what is missing because we
# didn't request data for it as it is in the denominator.
# for this, we can use our original list of ids that we wanted extract data for to tell us.

output_dir <- "output/chunks/"

legit_patids <- lapply(1:24,
               function(x) {
                 fread(sprintf("%s/patid_chunk_%02d.txt", output_dir, x))
                 }
               )

legit_patids <- rbindlist(legit_patids)

colnames(legit_patids) <- "patid"

eth <- final[legit_patids, on = "patid"]

eth$eth5[is.na(eth$eth5)] <- "Unknown / Not Stated"
eth$eth19[is.na(eth$eth19)] <- "Unknown / Not Stated"

table(eth$eth5, eth$eth19)

saveRDS(eth, "data/baseline_ethnicity_data.RDS")

# # we can see how this compares to our denominator
#

#
# dat_mega <- dat_mega[year == 2018]
#
# test <- eth[dat_mega, on = "patid"]
#
# table(test$eth19, useNA = "ifany") %>% prop.table()
#
# colnames(test)
