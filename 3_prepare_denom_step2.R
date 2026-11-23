
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

# we now have access to the linked data so we can link it in

dat <- readRDS("data/cleaned_denominator_step1.RDS")

# read in the linked data

imd <- fread("data/raw/")

imd <- imd[, c(1, 3)]
colnames(dat)
head(imd)

setkey(imd, patid)
setkey(dat, patid)

dat <- imd[dat, on = "patid"]

dat <- dat %>% rename(IMD10 = e2019_imd_10)
dat <- dat %>% mutate(IMD5 = IMD10)

dat$IMD5[dat$IMD10 == 1] <- 1
dat$IMD5[dat$IMD10 == 2] <- 1
dat$IMD5[dat$IMD10 == 3] <- 2
dat$IMD5[dat$IMD10 == 4] <- 2
dat$IMD5[dat$IMD10 == 5] <- 3
dat$IMD5[dat$IMD10 == 6] <- 3
dat$IMD5[dat$IMD10 == 7] <- 4
dat$IMD5[dat$IMD10 == 8] <- 4
dat$IMD5[dat$IMD10 == 9] <- 5
dat$IMD5[dat$IMD10 == 10] <- 5

dat$IMD5[is.na(dat$IMD5)] <- "IMD unavailable"
dat$IMD10[is.na(dat$IMD10)] <- "IMD unavailable"

# add the levels

dat$IMD10 <- factor(dat$IMD10, levels = c(1:10, "IMD unavailable"))
dat$IMD5 <- factor(dat$IMD5, levels = c(1:5, "IMD unavailable"))

table(dat$IMD10)
table(dat$IMD5)

#add the levels for gender (sex) as well

str(dat)

dat$gender[dat$gender == "F"] <- "Female"
dat$gender[dat$gender == "M"] <- "Male"
dat$gender[dat$gender == "I"] <- "Indeterminate"

dat$gender <- factor(dat$gender, levels = c("Male", "Female", "Indeterminate"))

summary(dat$region)
table(dat$region)

dat$region <- factor(dat$region, levels = c("East Midlands",
                                            "East of England",
                                            "North East",
                                            "North West",
                                            "South East",
                                            "South West",
                                            "West Midlands",
                                            "Yorkshire and The Humber",
                                            "London",
                                            "Northern Ireland"))
# maybe at this point we do need to split it into prevalence and incidence cohorts because they will not be the same

summary(dat$region)

dat[, `:=`(
  start_year = year(studystartdate),
  end_year = year(overallenddate)
)]

head(dat)

dat <- dat[, .(
  year = seq(start_year, end_year)  # generate sequence of years
), by = .(patid, pracid, region, gender, regstartdate, dob, overallenddate, studystartdate,
          age_pass, IMD5, IMD10)]

dat$midpoint <- paste0(dat$year, "-07-01")
dat$midpoint <- as.Date(dat$midpoint)

dat$jan01 <- paste0(dat$year, "-01-01")
dat$jan01 <- as.Date(dat$jan01)

dat$dec31 <- paste0(dat$year, "-12-31")
dat$dec31 <- as.Date(dat$dec31)

# dat <- dat[studystartdate <= midpoint & overallenddate >= midpoint]

dat[, age := year(midpoint) - year(dob)]

# agepass only needed for linkage eligibility - can now be removed
dat <- dat %>% select(-age_pass)

# remove everyone under 11 years old in their particular years

dat %>% filter(age < 11) %>% nrow()

dat <- dat %>% filter(age >= 11)

# now we create our age groups

dat <- dat %>%  mutate(agecat = cut(age, breaks = c(11, 16, 19, 22, 25, 30, 35,
                                            40, 45, 50, 55, 60, 65,
                                            70, 75, 80, 85, 90, 200),
                            right = FALSE,
                            labels = c("11-15", "16-18", "19-21",
                                       "22-24", "25-29", "30-34",
                                       "35-39", "40-44", "45-49",
                                       "50-54", "55-59", "60-64",
                                       "65-69", "70-74", "75-79",
                                       "80-84", "85-89", "90+")))

summary(dat$agecat)

# I think that it is at this point that we have the clean denominator ready to go so we can save it!

saveRDS(dat, "data/cleaned_denominator_final.RDS")

