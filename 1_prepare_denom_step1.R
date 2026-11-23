
library(tidyverse)
library(data.table)

# let's create a function for reading in lookups

addLookup <- function(dat, column_name, lookup_filename,
                      lookup_filepath = "data/lookups/") {
  lkup <- read.csv(paste0(lookup_filepath, lookup_filename), sep = "\t")
  colnames(lkup)[1] <- column_name

  dat <- left_join(dat, lkup, by = {{column_name}})

  #find the index for where the column is
  col_locate <- match(column_name, colnames(dt))
  col_locate2 <- colnames(dt)[col_locate - 1]

  dat <- dat %>% dplyr::select(!matches(column_name)) %>%
    dplyr::relocate(Description, .after = {{col_locate2}}) %>%
    dplyr::rename({{column_name}} := Description)

  return(dat)
  }

# lookup directory
lookups <- "data/lookups/"

list.files(lookups)

dt <- fread("data/202503_CPRDAurum_AcceptablePats.txt") # naming dt as data table

nrow(dt)
# first, clean the denominator a little

head(dt)

# sorting out the dates take time, so we can do it bit by bit and kick out patients as we go
dt$regenddate <- as.Date(dt$regenddate, format = "%d/%m/%Y")
dt <- dt[regenddate > "2017-12-31" | is.na(regenddate)]

nrow(dt)

# this already gets it down to 27 mil

dt$cprd_ddate <- as.Date(dt$cprd_ddate, format = "%d/%m/%Y")

dt <- dt[cprd_ddate > "2017-12-31" | is.na(cprd_ddate)]
nrow(dt)

dt$regstartdate <- as.Date(dt$regstartdate, format = "%d/%m/%Y")

dt$lcd <- as.Date(dt$lcd, format = "%d/%m/%Y")

# add in the lookups
dt <- addLookup(dt, "gender", "Gender.txt")
dt <- addLookup(dt, "region", "Region.txt")

table(dt$region)

# convert yob to dob
dt$dob <- paste0(dt$yob, "-07-01")
dt$dob <- as.Date(dt$dob)

# remove unnecessary columns
dt <- dt %>% dplyr::select(-mob, -emis_ddate, -patienttypeid, -acceptable, -uts, -yob)

# we have further useful things to do
summary(dt$lcd) # noone is missing a lst collection date so everyone will have an end date

dt <- dt %>% mutate(overallenddate = pmin(regenddate, cprd_ddate, lcd, na.rm = TRUE))
summary(dt$overallenddate)

dt <- dt %>% select(-regenddate, -cprd_ddate, -lcd)

dt$startdate <- as.Date("2018-01-01")

dt <- dt %>% mutate(studystartdate = pmax(regstartdate, startdate))

head(dt)
dt <- dt %>% select(-startdate)

# remove all the people from the duplicate practices:

nrow(dt)

dt <- dt[!(pracid %in% c(
  20024, 20469, 20803, 21112, 21444,
  20036, 20487, 20804, 21118, 21451,
  20091, 20552, 20822, 21172, 21529,
  20171, 20554, 20868, 21173, 21553,
  20178, 20640, 20908, 21277, 21558,
  20202, 20717, 20912, 21281, 21585,
  20254, 20734, 20996, 21331,
  20389, 20737, 21001, 21334,
  20430, 20740, 21015, 21390,
  20452, 20790, 21078, 21430))]

nrow(dt)

# save this

# get rid of anyone who is too young for the entire time period.

head(dt)

dtbackup <- dt

nrow(dt)

# everyone needs to be at least 11

dt <- dt[dob <= "2013-12-14"]

nrow(dt)

dt %>% head() %>% mutate(age_pass = time_length(interval("2023-01-01", "2024-01-01"), "years")) # this method accounts for irregular sized years
dt %>% head() %>% mutate(age_pass = time_length(difftime("2024-01-01", "2023-01-01"), "years")) # this method does not

dt$age_pass <- time_length(interval(dt$dob, dt$overallenddate), "years")

head(dt)

nrow(dt)

# drop people who don't turn 11 at any point during their time period in the study
dt <- dt[age_pass >= 11]

nrow(dt)

saveRDS(dt, "data/cleaned_denominator_step1.RDS")

