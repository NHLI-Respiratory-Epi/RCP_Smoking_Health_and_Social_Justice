library(tidyverse)
library(data.table)

# read in full denominator
dat_mega <- readRDS("data/cleaned_denominator_final.RDS")

# read in the smoking observations
smoke <- readRDS("data/extracted_smoking_data_CNS_update.RDS")

# read in the vaping observations
vape <- readRDS("data/vape_obs.RDS")

# read in the ethnicity data
eth <- readRDS("data/baseline_ethnicity_data.RDS")

eth %>% nrow()

# restrict 'smoke' and 'vape' to those with a current smoking code at any point
# this is a preliminary step that reduces the size of the denominator - the
# denominator will still need to be determined at each step to identify the current
# smokers

smoke <- smoke[smoking_status == "Current smoker"] # already reduces to 5% of size

smoke <- smoke[, .(patid)]
smoke <- unique(smoke)

# the potential denominator is then calculated using these patids who have a
# current smoking status at any point

setkey(smoke, patid)
setkey(dat_mega, patid)

dat_smoke <- smoke[dat_mega, nomatch = 0]

# now we add in the ethnicity data

dat_smoke <- eth[dat_smoke, on = "patid"]

saveRDS(dat_smoke, "data/current_smokers_denom_unfinalised.RDS")

# now we follow the same process but with current vapers

vape <- vape[vape == "Current vaper"]
vape <- vape[, .(patid)]
vape <- unique(vape)

# the potential denominator is then calculated using these patids who have a
# current smoking status at any point

setkey(vape, patid)
setkey(dat_mega, patid)

dat_vape <- vape[dat_mega, nomatch = 0]

# now we add in the ethnicity data

dat_vape <- eth[dat_vape, on = "patid"]

saveRDS(dat_vape, "data/current_vapers_denom_unfinalised.RDS")

head(dat_smoke)

# it's also worth reducing the smoking status observations to just those
# present in the new current smoking denominator to save time

# read in the smoking observations
smoke_reduced <- readRDS("data/extracted_smoking_data_CNS_update.RDS")

setkey(smoke_reduced, patid)
smoke_reduced <- smoke[smoke_reduced, on = "patid"]
smoke_reduced <- smoke_reduced[, .(patid, obsdate, smoking_status)]
smoke_reduced <- unique(smoke_reduced)

saveRDS(smoke_reduced, "data/smoking_obs_for_current_smokers.RDS")

