library(tidyverse)
library(data.table)

dat <- readRDS("data/extracted_smoking_data_full_details.RDS")

# vaping
vape <- dat[!is.na(vape)]
vape <- vape[, .(patid, obsdate, vape)]
saveRDS(vape, "data/vape_obs.RDS")

# vaping cessation
vaping_cessation <- dat[!is.na(vaping_cessation)]
vaping_cessation <- vaping_cessation[, .(patid, obsdate, vaping_cessation)]
# only 7 codes and none are relevant - no point

# smoking cessation
smoking_cessation <- dat[!is.na(smoking_cessation)]
saveRDS(smoking_cessation, "data/smoking_cessation_PRELIM.RDS")

# we require: successful and unsuccessful smoking cessation.

table(smoking_cessation$smoking_cessation)

smoking_cessation %>% select(smoking_cessation, smoking_status) %>% table(useNA = "ifany")

smoking_cessation %>% filter(smoking_cessation == "Offered referral for smoking cessation") %>%
  select(term) %>% table()

smoking_cessation %>% head(1000000) %>%  group_by(patid, obsdate) %>%
  mutate(present = sum(smoking_cessation == "Offered referral for smoking cessation")) %>%
  filter(present > 0) %>% arrange(patid, obsdate) %>% select(patid, obsdate, term, smoking_cessation, smoking_status, present) %>%
 print(n=400)

  #mutate(here = if_any(present > 0, 1, 0)) %>% filter(here == 1)

# a lot of the time there is no indication that if someone has been offered a referral, that they have actually been referred.
# therefore, we do not include those ones.

smoking_cessation <- smoking_cessation[, .(patid, obsdate, smoking_cessation)]

smoking_cessation_success <- smoking_cessation %>%
  filter(smoking_cessation == "Has undergone quit attempt - successful")

head(smoking_cessation_success)

smoking_cessation_success$success <- 1
smoking_cessation_success$smoking_cessation <- NULL

smoking_cessation_quit_attempt <- smoking_cessation %>%
  filter(smoking_cessation %in% c("Has undergone quit attempt - successful",
                                  "Undergoing quit attempt",
                                  "Has undergone quit attempt - status unknown"))

smoking_cessation_quit_attempt$quit_attempt <- 1
smoking_cessation_quit_attempt$smoking_cessation <- NULL

smoking_cessation_referral_or_counselling <- smoking_cessation %>%
  filter(smoking_cessation %in% c("Enrolled in smoking cessation programme",
                                  "Referral for smoking cessation",
                                  "Smoking cessation counselling",
                                  "Smoking cessation behavioural therapy"))

smoking_cessation_referral_or_counselling$referral_enrol <- 1
smoking_cessation_referral_or_counselling$smoking_cessation <- NULL

saveRDS(smoking_cessation_success, "data/smoking_cessation_success.RDS")
saveRDS(smoking_cessation_quit_attempt, "data/smoking_cessation_quit_attempt.RDS")
saveRDS(smoking_cessation_referral_or_counselling, "data/smoking_cessation_referral_or_counselling.RDS")

