Sys.time()

library(data.table)
library(tidyverse)
library(bit64)
library(parallel)

codelist_path <- "data/codelists/"

extract_files <- dir("data/raw/")
extract_files <- extract_files[grepl("Observation", extract_files) == TRUE]
extract_files <- extract_files[grepl("Define", extract_files) == FALSE]

# this gives us all the extract files we need to run the codelist on.
# for our purposes now though, because we don't have all of them, we might as well
# just run it on 5 of them

#extract_files <- extract_files[1:5]

ethnicity_codelist <- readRDS(paste0(codelist_path, "ethnicity.RDS"))

# clean up the codelist
colnames(ethnicity_codelist)
colnames(ethnicity_codelist) <- tolower(colnames(ethnicity_codelist))

ethnicity_codelist <- ethnicity_codelist %>%
  select(medcodeid, eth19, eth19_grouped)

# I want to make the medcodeid an integer 64

ethnicity_codelist$medcodeid <- as.integer64(ethnicity_codelist$medcodeid)
ethnicity_codelist <- as.data.table(ethnicity_codelist)
setkey(ethnicity_codelist, medcodeid)

dat <- fread(paste0("data/raw/", extract_files[1]))

dat <- dat[ethnicity_codelist, on = "medcodeid"]
colnames(dat)

head(dat)

apply_codelist_pipe <- function(raw_extract.txt, codelist, path = NULL,
                                cleaning_for_obs = TRUE, cleaning_for_drug_issue = FALSE) {
  # check that medcodeid is called medcodeid in both files

  if (!is.null(path)) {
    raw_extract.txt <- paste0(path, raw_extract.txt)
  }

  dat <- fread(raw_extract.txt)
  setkey(dat, medcodeid)
  setkey(codelist, medcodeid) # should be done already
  dat <- dat[codelist, on = "medcodeid", nomatch = 0]

  # further cleaning here (as we are reading in raw text files)

  if (cleaning_for_obs == TRUE) {
    dat <- dat %>% select(patid, -consid, -pracid, -obsid, obsdate, -enterdate, -staffid, -parentobsid,
                          medcodeid, -value, -numunitid, -obstypeid, -numrangelow, -numrangehigh, -probobsid) %>%
      mutate(obsdate = as.Date(obsdate, format = "%d/%m/%Y")) #,
#             enterdate = as.Date(enterdate, format = "%d/%m/%Y"))
    dat <- dat %>% filter(!is.na(obsdate))
    dat <- dat %>% filter(obsdate > "01/01/1902")

  }

  print(paste0(raw_extract.txt, " completed."))
  return(dat)

}

detectCores()
ncores <- 75

# Create a cluster
cl <- makeCluster(ncores)

# # may need to do this step multiple times if the numbers get too large
# dat <- map(extract_files, apply_codelist_pipe, codelist = ethnicity_codelist,

# we should parallelise this process

clusterEvalQ(cl, { # these need to be read into each cluster
  library(data.table)
  library(dplyr)
  library(bit64)
})

# these R objects are necessary in each cluster
clusterExport(cl, varlist = c("ethnicity_codelist", "apply_codelist_pipe"), envir = environment())

big <- parLapply(cl, extract_files, function(dat) {
  apply_codelist_pipe(dat, codelist = ethnicity_codelist, path = "data/raw/")
})

stopCluster(cl)

Sys.time()
full <- rbindlist(big)
Sys.time()

# we can make this a lot smaller by dropping unnecessary columns

# full <- full[, .(patid, obsdate, smoking_status)]

head(full)

# I made a mistake here - accidentally dropped the ethnicities! We need to get them back in...

getback <- full[ethnicity_codelist, on = "medcodeid", nomatch = 0]

getback <- getback %>% select(-medcodeid)
head(getback)

saveRDS(getback, "data/extracted_ethnicity_data.RDS")

Sys.time()
