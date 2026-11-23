# create codelist for smoking ERAP

codelist_filepath <- "data/codelists/"
filename <- "smoking_status"

dat <- read.csv(paste0(codelist_filepath, filename, ".csv"), colClasses = "character")

table(dat$vape)
table(dat$smoking_status)

dat <- dat %>% filter(smoking_status %in% c("Current smoker", "Ex-smoker") | vape %in% c("Current vaper", "Ex-vaper"))

dat <- dat %>% select(medcodeid, term, smoking_status, vape)

# write the text file

dat %>% pull(medcodeid) %>% paste0(., collapse = ",") %>%
  cat(., file = "data/Smoking_prevalence_extraction_codelist.txt")

# also, assess the denominator files

denom <- read.delim("data/202412_CPRDAurum_AcceptablePats.txt",
           nrows = 1000)

# the denominator file includes the region