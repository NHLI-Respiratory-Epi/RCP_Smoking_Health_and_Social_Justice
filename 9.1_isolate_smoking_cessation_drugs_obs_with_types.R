Sys.time()

library(data.table)
library(tidyverse)
library(bit64)
library(parallel)

codelist_path <- "data/codelists/"

extract_files <- dir("data/raw/")
extract_files <- extract_files[grepl("DrugIssue", extract_files) == TRUE]
extract_files <- extract_files[grepl("Define", extract_files) == FALSE]

# this gives us all the extract files we need to run the codelist on.
# for our purposes now though, because we don't have all of them, we might as well
# just run it on 5 of them

#extract_files <- extract_files[1:5]

codelist <- readRDS(paste0(codelist_path, "smoking_cessation_drugs.RDS"))

# clean up the codelist
colnames(codelist)
colnames(codelist) <- tolower(colnames(codelist))

# we need to add in the missing drug substances here.

codelist <- codelist %>% arrange(drugsubstancename)

codelist$drugsubstancename[codelist$drugsubstancename == "Nicotine bitartrate"] <- "Nicotine"
codelist$drugsubstancename[codelist$drugsubstancename == "Nicotine cyclodextrin complex"] <- "Nicotine"
codelist$drugsubstancename[codelist$drugsubstancename == "Varenicline tartrate"] <- "Varenicline"
codelist$drugsubstancename[codelist$drugsubstancename == "Bupropion hydrochloride"] <- "Bupropion"

# and now the ones that are just coded in EMIS (and the ones in DMD that are missing a drug substance name for some reason)
codelist %>% filter(drugsubstancename == "") %>% select(prodcodeid, term.from.emis)

codelist$drugsubstancename[codelist$prodcodeid ==  "970441000033115"] <- "Nicotine" #  Inhalation cartridge with mouthpiece (refill)  10 mg/cartridge
codelist$drugsubstancename[codelist$prodcodeid ==  "1768341000033114"] <- "Nicotine" #     Nicotine  Inhalation cartridge with mouthpiece (starter pack)  10 mg/cartridge
codelist$drugsubstancename[codelist$prodcodeid ==  "2280441000033111"] <- "Nicotine" #    Nicotinell  Chewing gum  2 mg
codelist$drugsubstancename[codelist$prodcodeid ==  "2280541000033112"] <- "Nicotine" #    Nicotinell  Chewing gum  4 mg
codelist$drugsubstancename[codelist$prodcodeid ==  "2795441000033114"] <- "Nicotine" #  Nicotine  Chewing gum (mint)  Sugar Free 4 mg
codelist$drugsubstancename[codelist$prodcodeid ==  "2795641000033111"] <- "Nicotine" #    Nicotine  Chewing gum (mint)  Sugar Free 2 mg
codelist$drugsubstancename[codelist$prodcodeid ==  "2975241000033114"] <- "Nicotine" # Nicotine  Lozenges (Mint)  Sugar Free 2 mg
codelist$drugsubstancename[codelist$prodcodeid ==  "2975341000033116"] <- "Nicotine" #     Nicotine  Lozenges (Mint)  Sugar Free 4 mg
codelist$drugsubstancename[codelist$prodcodeid ==  "4011741000033112"] <- "Varenicline" #                       Varenicline 1mg tablets and Varenicline 500microgram tablets
codelist$drugsubstancename[codelist$prodcodeid ==  "5401441000033115"] <- "Nicotine" # Nicorette combi 15mg transdermal patches + 2mg medicated gum (McNeil Products Ltd)
codelist$drugsubstancename[codelist$prodcodeid ==  "11483241000033116"] <- "Varenicline" #                    Champix 0.5mg/1mg 2 week treatment initiation pack (Pfizer Ltd)
codelist$drugsubstancename[codelist$prodcodeid ==  "11483341000033114"] <- "Varenicline" # Champix 0.5mg/1mg 4 week treatment initiation pack (Pfizer Ltd)
codelist$drugsubstancename[codelist$prodcodeid ==  "14240941000033117"] <- "Varenicline" #   Varenicline 1mg tablets and Varenicline 500microgram tablets (Imported (Canada))

table(codelist$drugsubstancename)
codelist <- codelist %>%
  select(prodcodeid, drugsubstancename) %>% mutate(presc = 1)

# I want to make the medcodeid an integer 64

codelist$prodcodeid <- as.integer64(codelist$prodcodeid)
codelist <- as.data.table(codelist)
setkey(codelist, prodcodeid)

dat <- fread(paste0("data/raw/", extract_files[1]))

dat <- dat[codelist, on = "prodcodeid"]
colnames(dat)

head(dat)

apply_codelist_pipe <- function(raw_extract.txt, codelist, path = NULL,
                                cleaning_for_obs = TRUE, cleaning_for_drug_issue = FALSE, type = "medcodeid") {
  # check that medcodeid is called medcodeid in both files

  if (!is.null(path)) {
    raw_extract.txt <- paste0(path, raw_extract.txt)
  }

  codelist <- data.table(codelist)
  dat <- fread(raw_extract.txt)
  setkeyv(dat, type)
  setkeyv(codelist, type) # should be done already
  dat <- dat[codelist, nomatch = 0]

  # further cleaning here (as we are reading in raw text files)

  if (cleaning_for_obs == TRUE) {
    dat <- dat %>% select( # patid, obsdate, medcodeid, these are being selected, but if you explicitly
                           # select them you will loose the codelist data
                          -consid, -pracid, -obsid,  -enterdate, -staffid, -parentobsid,
                           -value, -numunitid, -obstypeid, -numrangelow, -numrangehigh,
                          -probobsid) %>%
      mutate(obsdate = as.Date(obsdate, format = "%d/%m/%Y")) #,
#             enterdate = as.Date(enterdate, format = "%d/%m/%Y"))
    dat <- dat %>% filter(!is.na(obsdate))
    dat <- dat %>% filter(obsdate > "01/01/1902")

  }

  if (cleaning_for_drug_issue == TRUE) {
   dat <- dat %>% select(-issueid, -pracid, -probobsid, -drugrecid, -enterdate,
                         -staffid, -dosageid, -quantity, -quantunitid, -duration,
                         -estnhscost, -prodcodeid) %>%
     mutate(issuedate = as.Date(issuedate, format = "%d/%m/%Y")) #,
   #             enterdate = as.Date(enterdate, format = "%d/%m/%Y"))
   dat <- dat %>% filter(!is.na(issuedate))
   dat <- dat %>% filter(issuedate > "1902-01-01")

  }

  print(paste0(raw_extract.txt, " completed."))
  return(dat)

}

# # codelist test for medcodes

# colnames(codelisttest) <- tolower(colnames(codelisttest))
# codelisttest$medcodeid <- as.integer64(codelisttest$medcodeid)

# does it work for prodcodes? yes.

apply_codelist_pipe(extract_files[1], codelist, path = "data/raw/",
                    cleaning_for_obs = FALSE, cleaning_for_drug_issue = TRUE,
                    type = "prodcodeid")

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
clusterExport(cl, varlist = c("codelist", "apply_codelist_pipe"), envir = environment())

big <- parLapply(cl, extract_files, function(dat) {
  apply_codelist_pipe(dat, codelist = codelist, path = "data/raw/",
                      cleaning_for_obs = FALSE, cleaning_for_drug_issue = TRUE,
                      type = "prodcodeid")
})

stopCluster(cl)

Sys.time()
full <- rbindlist(big)
Sys.time()

saveRDS(full, "data/extracted_smoking_cessation_drugs_data_specific_drug.RDS")

Sys.time()

table(full$drugsubstancename)