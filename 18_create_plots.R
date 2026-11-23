
library(readxl)
library(data.table)
library(tidyverse)
library(data.table)
library(ggplot2)
library(scales)  # for percent_format()

# 1) Binomial CI functions for prevalence
get_prev_lo <- function(x, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  # exact (Clopper-Pearson) lower bound; if x = 0 we force 0
  lo <- qbeta(alpha/2,       x,   n - x + 1)
  lo[x == 0] <- 0
  lo
}

get_prev_hi <- function(x, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  # exact upper bound; if x = n we force 1
  hi <- qbeta(1 - alpha/2, x + 1, n - x)
  hi[x == n] <- 1
  hi
}

# we can create a standard population if we decide to standardise our results.

setwd(dir = "~")

census2 <- fread("data/Census_detailed_age_sex_region_ethnicity.csv")

census2 <- census2 %>% select(region = Regions,
                              age_category = `Age (91 categories)`,
                              # ethnic_code = `Ethnic group (6 categories) Code`,
                              # ethnic_category = `Ethnic group (6 categories)`,
                              sex = `Sex (2 categories)`,
                              census_N = Observation)

sum(census2$census_N[census2$region != "Wales"])
head(census2)

census2[ , ageextract:= str_extract(age_category, "(\\d)+") ]

census2$agecat[census2$ageextract %in%  11:15] <- "11-15"
census2$agecat[census2$ageextract %in%  16:18] <- "16-18"
census2$agecat[census2$ageextract %in%  19:21] <- "19-21"
census2$agecat[census2$ageextract %in%  22:24] <- "22-24"
census2$agecat[census2$ageextract %in%  25:29] <- "25-29"
census2$agecat[census2$ageextract %in%  30:34] <- "30-34"
census2$agecat[census2$ageextract %in%  35:39] <- "35-39"
census2$agecat[census2$ageextract %in%  40:44] <- "40-44"
census2$agecat[census2$ageextract %in%  45:49] <- "45-49"
census2$agecat[census2$ageextract %in%  50:54] <- "50-54"
census2$agecat[census2$ageextract %in%  55:59] <- "55-59"
census2$agecat[census2$ageextract %in%  60:64] <- "60-64"
census2$agecat[census2$ageextract %in%  65:69] <- "65-69"
census2$agecat[census2$ageextract %in%  70:74] <- "70-74"
census2$agecat[census2$ageextract %in%  75:79] <- "75-79"
census2$agecat[census2$ageextract %in%  80:84] <- "80-84"
census2$agecat[census2$ageextract %in%  85:89] <- "85-89"
census2$agecat[census2$ageextract %in%  90] <- "90+"

census2 <- census2[!is.na(agecat)]
census2 <- census2[agecat != "11-15"]

census2 <- census2[region != "Wales"]

census2 <- census2 %>% select(-age_category, -ageextract)

table(census2$agecat)
# sex

table(census2$sex)

census3 <- census2[, .(census_N = sum(census_N)), by = c("region", "sex", "agecat")]

pop_N <- sum(census3$census_N)

census_weights <- census3[, .(weight = census_N/pop_N, census_N, pop_N), by = c("region", "sex", "agecat")]

# # # # # # smoking_prevalence

cohort <- 2018:2024

smoking_prevalence <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/prevalences/", x,
                            "/prevalence_results_", x, ".xlsx"), sheet = "agecat_gender_region_IMD10")
  steps$year <- x
  return(steps)
}
)

smoking_prevalence <- rbindlist(smoking_prevalence)

smoking_prevalence <- smoking_prevalence[IMD10 != "IMD unavailable"]

smoking_prevalence$IMD5 <- NA
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 1] <- 1
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 2] <- 1
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 3] <- 2
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 4] <- 2
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 5] <- 3
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 6] <- 3
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 7] <- 4
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 8] <- 4
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 9] <- 5
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 10] <- 5

smoking_prevalence$IMD5 <- factor(smoking_prevalence$IMD5, levels = c("1", "2", "3", "4", "5"))

colnames(smoking_prevalence)

# calculate current weights

smoking_prevalence <- smoking_prevalence %>% rename(sex = gender)

colnames(result)

library(data.table)
weighted <- smoking_prevalence[, cprd_yearly_n := sum(n_total), .(year)]

weighted <- weighted[, .(n_total = sum(n_total), n_current = sum(n_current), n_ex = sum(n_ex), n_never = sum(n_never)),
         .(year, region, agecat, sex)]

weighted$prev_current <- weighted$n_current/weighted$n_total

weighted <- weighted[census_weights, on = .(region, agecat, sex)]

weighted$standardised_current_smoking_numerator <- weighted$prev_current*weighted$census_N

weighted
census_weights

# standardised
result_weighted <- weighted[,
  .(
    prevalence = sum(standardised_current_smoking_numerator) / sum(census_N)
#    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
 #   prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year)
]

# 2) Summarise prevalence by year and IMD5
result_unweighted <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year)
]

# 3) Plot
ggplot(result, aes(x = year,
                   y = prevalence)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.3)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking Prevalence",
    # color = "IMD Quintile",
    # fill  = "IMD Quintile",
    title = "Smoking Prevalence Over Time by IMD"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

# 2) Summarise prevalence by year and IMD5
result <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year, IMD5)
]

# national average (across all IMD quintiles)
national <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = year
]

# 3) Plot
ggplot(result, aes(x = year,
                   y = prevalence,
                   color = IMD5,
                   group = IMD5)) +
  # geom_ribbon(aes(ymin = prev_lo, ymax = prev_hi, fill = IMD5),
  #             alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.3)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking Prevalence",
    color = "IMD Quintile",
    fill  = "IMD Quintile",
    title = "Smoking Prevalence Over Time by IMD"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

# national average (across all IMD quintiles)
national <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = year
]

# plot IMD5 lines + national average
library(ggplot2)
library(scales)

ggplot(result, aes(x = year, y = prevalence, color = IMD5, group = IMD5)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  # add national average as dashed black
  geom_line(data = national,
            aes(x = year, y = prevalence),
            color = "black", size = 1.2, linetype = "dashed") +
  geom_point(data = national,
             aes(x = year, y = prevalence),
             color = "black", shape = 18, size = 3) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.3)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking Prevalence",
    color = "IMD Quintile",
    title = "Smoking Prevalence Over Time by IMD (with National Average)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

library(ggplot2)
library(scales)

# 1) IMD-specific
p <- ggplot(result, aes(x = year, y = prevalence, color = IMD5, group = IMD5)) +
  geom_line(size = 1) +
  geom_point(size = 2)

# 2) National average
p <- p +
  geom_line(
    data        = national,
    mapping     = aes(x = year, y = prevalence),
    inherit.aes = FALSE,
    color       = "black",
    size        = 1.2,
    linetype    = "dashed"
  ) +
  geom_point(
    data        = national,
    mapping     = aes(x = year, y = prevalence),
    inherit.aes = FALSE,
    color       = "black",
    shape       = 18,
    size        = 3
  )

# 3) Scales & labels
p +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.3)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking Prevalence",
    color = "IMD Quintile",
    title = "Smoking Prevalence Over Time by IMD (with National Average)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

# by sex

# 2) Summarise prevalence by year and IMD5
result_sex <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable" & sex != "Indeterminate",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year, sex)
]

# 2) standardised sex results
sex_result_weighted <- weighted[,
                            .(
                              prevalence = sum(standardised_current_smoking_numerator) / sum(census_N)
                              #    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
                              #   prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
                            ),
                            by = .(year, sex)
]

# national average (across all IMD quintiles)
national <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = year
]

# 1) IMD-specific
p <- ggplot(sex_result_weighted, aes(x = year, y = prevalence, color = sex, group = sex)) +
  geom_line(size = 1) +
  geom_point(size = 2)

# 2) National average
p <- p +
  geom_line(
    data        = national,
    mapping     = aes(x = year, y = prevalence),
    inherit.aes = FALSE,
    color       = "black",
    size        = 1.2,
    linetype    = "dashed"
  ) +
  geom_point(
    data        = national,
    mapping     = aes(x = year, y = prevalence),
    inherit.aes = FALSE,
    color       = "black",
    shape       = 18,
    size        = 3
  )

# 3) Scales & labels
p +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.3)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking Prevalence",
    color = "IMD Quintile",
    title = "Smoking Prevalence Over Time by IMD (with National Average)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

################ vaping

# # # # # # smoking_prevalence

cohort <- 2018:2024

vaping_prevalence <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/prevalences/", x,
                            "/vaping_prevalence_results_", x, ".xlsx"), sheet = "agecat_gender_region_IMD10")
  steps$year <- x
  return(steps)
}
)

vaping_prevalence <- rbindlist(vaping_prevalence)

vaping_prevalence <- vaping_prevalence[IMD10 != "IMD unavailable"]

vaping_prevalence$IMD5 <- NA
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 1] <- 1
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 2] <- 1
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 3] <- 2
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 4] <- 2
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 5] <- 3
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 6] <- 3
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 7] <- 4
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 8] <- 4
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 9] <- 5
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 10] <- 5

vaping_prevalence$IMD5 <- factor(vaping_prevalence$IMD5, levels = c("1", "2", "3", "4", "5"))

colnames(vaping_prevalence)

library(data.table)
library(ggplot2)
library(scales)  # for percent_format()

# 1) Binomial CI functions for prevalence
get_prev_lo <- function(x, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  # exact (Clopper-Pearson) lower bound; if x = 0 we force 0
  lo <- qbeta(alpha/2,       x,   n - x + 1)
  lo[x == 0] <- 0
  lo
}

get_prev_hi <- function(x, n, conf.level = 0.95) {
  alpha <- 1 - conf.level
  # exact upper bound; if x = n we force 1
  hi <- qbeta(1 - alpha/2, x + 1, n - x)
  hi[x == n] <- 1
  hi
}

# 2) Summarise prevalence by year & IMD5
result <- vaping_prevalence[
  IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year, agecat)
]

# 3) Plot
ggplot(result[agecat %in% c("16-18", "19-21", "22-24", "25-29", "30-34")], aes(x = year,
                   y = prevalence,
                   color = agecat,
                   group = agecat)) +
  # geom_ribbon(aes(ymin = prev_lo, ymax = prev_hi, fill = IMD5),
  #             alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.05)
  ) +
  labs(
    x     = "Year",
    y     = "vaping Prevalence",
    color = "IMD Quintile",
    fill  = "IMD Quintile",
    title = "vaping Prevalence Over Time by IMD"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

# let's create some longitudinal graphs.

cohort <- 2018:2024

cessattempt <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_cessation_attempt_results_", x, ".xlsx"), sheet = "agecat_gender_region_IMD10_eth5")
  steps$year <- x
  return(steps)
}
)

get_ci_lo <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  lo <- ifelse(
    n == 0,
    0,
    qchisq(alpha/2, df = 2 * n) / (2 * py_total)
  )
  return(lo)
}

get_ci_hi <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  hi <- qchisq(1 - alpha/2, df = 2 * (n + 1)) / (2 * py_total)
  return(hi)
}

cessattempt <- rbindlist(cessattempt)

cessattempt <- cessattempt[IMD10 != "IMD unavailable"]

cessattempt$IMD5 <- NA
cessattempt$IMD5[cessattempt$IMD10 == 1] <- 1
cessattempt$IMD5[cessattempt$IMD10 == 2] <- 1
cessattempt$IMD5[cessattempt$IMD10 == 3] <- 2
cessattempt$IMD5[cessattempt$IMD10 == 4] <- 2
cessattempt$IMD5[cessattempt$IMD10 == 5] <- 3
cessattempt$IMD5[cessattempt$IMD10 == 6] <- 3
cessattempt$IMD5[cessattempt$IMD10 == 7] <- 4
cessattempt$IMD5[cessattempt$IMD10 == 8] <- 4
cessattempt$IMD5[cessattempt$IMD10 == 9] <- 5
cessattempt$IMD5[cessattempt$IMD10 == 10] <- 5

cessattempt$IMD5 <- factor(cessattempt$IMD5, levels = c("1", "2", "3", "4", "5"))

colnames(cessattempt)

result <- cessattempt[
  !agecat %in% c("11-15", "16-18") & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_successful_attempts) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_successful_attempts), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_successful_attempts), sum(person_years_total))
  ),
  .(year, IMD5)
]

summary(result)
plot(result$year, result$IRRsuccess)

ggplot(result, aes(x = year,
                   y = IRRsuccess,
                   color = IMD5,
                   group = IMD5)) +
  # CI ribbon
  geom_ribbon(aes(ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5),
              alpha = 0.2, color = NA) +
  # lines and points
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(0, 0.11)) +
  labs(
    x = "Year",
    y = "Incidence Rate (per person-year)",
    color = "IMD Quintile",
    fill  = "IMD Quintile",
    title = "Successful smoking cessation Attempt Incidence Over Time by IMD"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1),

  )

# # # # # # drug prescription

cohort <- 2018:2024

prescription <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_drug_prescription_incidence_results_", x, ".xlsx"), sheet = "agecat_gender_region_IMD10_eth5")
  steps$year <- x
  return(steps)
}
)

get_ci_lo <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  lo <- ifelse(
    n == 0,
    0,
    qchisq(alpha/2, df = 2 * n) / (2 * py_total)
  )
  return(lo)
}

get_ci_hi <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  hi <- qchisq(1 - alpha/2, df = 2 * (n + 1)) / (2 * py_total)
  return(hi)
}

prescription <- rbindlist(prescription)

prescription <- prescription[IMD10 != "IMD unavailable"]

prescription$IMD5 <- NA
prescription$IMD5[prescription$IMD10 == 1] <- 1
prescription$IMD5[prescription$IMD10 == 2] <- 1
prescription$IMD5[prescription$IMD10 == 3] <- 2
prescription$IMD5[prescription$IMD10 == 4] <- 2
prescription$IMD5[prescription$IMD10 == 5] <- 3
prescription$IMD5[prescription$IMD10 == 6] <- 3
prescription$IMD5[prescription$IMD10 == 7] <- 4
prescription$IMD5[prescription$IMD10 == 8] <- 4
prescription$IMD5[prescription$IMD10 == 9] <- 5
prescription$IMD5[prescription$IMD10 == 10] <- 5

prescription$IMD5 <- factor(prescription$IMD5, levels = c("1", "2", "3", "4", "5"))

colnames(prescription)

result <- prescription[
  !agecat %in% c("11-15", "16-18") & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(ppl_N_total) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(ppl_N_total), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(ppl_N_total), sum(person_years_total))
  ),
  .(year, IMD5)
]

summary(result)
plot(result$year, result$IRRsuccess)

ggplot(result, aes(x = year,
                   y = IRRsuccess,
                   color = IMD5,
                   group = IMD5)) +
  # CI ribbon
  geom_ribbon(aes(ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5),
              alpha = 0.2, color = NA) +
  # lines and points
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_y_continuous(limits = c(1, 1.2)) +
  labs(
    x = "Year",
    y = "Incidence Rate (per person-year)",
    color = "IMD Quintile",
    fill  = "IMD Quintile",
    title = "Smoking cessation drug prescription Incidence Over Time by IMD"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1),

  )

# # # # # # smoking_incidence

cohort <- 2018:2024

smoking_incidence <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_incidence_results_", x, ".xlsx"), sheet = "agecat_gender_region_IMD10")
  steps$year <- x
  return(steps)
}
)

get_ci_lo <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  lo <- ifelse(
    n == 0,
    0,
    qchisq(alpha/2, df = 2 * n) / (2 * py_total)
  )
  return(lo)
}

get_ci_hi <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  hi <- qchisq(1 - alpha/2, df = 2 * (n + 1)) / (2 * py_total)
  return(hi)
}

smoking_incidence <- rbindlist(smoking_incidence)

smoking_incidence <- smoking_incidence[IMD10 != "IMD unavailable"]

smoking_incidence$IMD5 <- NA
smoking_incidence$IMD5[smoking_incidence$IMD10 == 1] <- 1
smoking_incidence$IMD5[smoking_incidence$IMD10 == 2] <- 1
smoking_incidence$IMD5[smoking_incidence$IMD10 == 3] <- 2
smoking_incidence$IMD5[smoking_incidence$IMD10 == 4] <- 2
smoking_incidence$IMD5[smoking_incidence$IMD10 == 5] <- 3
smoking_incidence$IMD5[smoking_incidence$IMD10 == 6] <- 3
smoking_incidence$IMD5[smoking_incidence$IMD10 == 7] <- 4
smoking_incidence$IMD5[smoking_incidence$IMD10 == 8] <- 4
smoking_incidence$IMD5[smoking_incidence$IMD10 == 9] <- 5
smoking_incidence$IMD5[smoking_incidence$IMD10 == 10] <- 5

smoking_incidence$IMD5 <- factor(smoking_incidence$IMD5, levels = c("1", "2", "3", "4", "5"))

colnames(smoking_incidence)

result <- smoking_incidence[
  IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  .(agecat, IMD5)
]

ggplot(result, aes(x = agecat,
                   y = IRRsuccess,
                   color = IMD5,
                   group = IMD5)) +
  # CI ribbon
  geom_ribbon(aes(ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5),
              alpha = 0.2, color = NA) +
  # lines and points
  geom_line(size = 1) +
  geom_point(size = 2) +
  #  scale_y_continuous(limits = c(1, 1.2)) +
  labs(
    x = "Year",
    y = "Incidence Rate (per person-year)",
    color = "IMD Quintile",
    fill  = "IMD Quintile",
    title = "Smoking cessation drug prescription Incidence Over Time by IMD"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x     = element_text(angle = 45, hjust = 1),

  )

library(data.table)
library(ggplot2)

# 1) Summarise across all IMD strata
result <- smoking_incidence[
  IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_never_incidence) / sum(person_years_never),
    IRRsuccess_lo = get_ci_lo(sum(n_never_incidence), sum(person_years_never)),
    IRRsuccess_hi = get_ci_hi(sum(n_never_incidence), sum(person_years_never))
  ),
  by = agecat       # only grouping by agecat now
]

# 2) Plot a single curve (and ribbon) across agecat
ggplot(result, aes(x = agecat, y = IRRsuccess, group = 1)) +
  # CI ribbon (single fill color)
  geom_ribbon(aes(ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
              fill = "lightblue", alpha = 0.3) +
  # line + points
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  # adjust y if needed
  # scale_y_continuous(limits = c(0, 0.1)) +
  labs(
    x     = "Age Category",
    y     = "Incidence Rate (per person-year)",
    title = "Smoking Incidence by Age"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1)
  )

# relapse

result <- smoking_incidence[
  IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_ex_incidence) / sum(person_years_ex),
    IRRsuccess_lo = get_ci_lo(sum(n_ex_incidence), sum(person_years_ex)),
    IRRsuccess_hi = get_ci_hi(sum(n_ex_incidence), sum(person_years_ex))
  ),
  by = agecat       # only grouping by agecat now
]

# 2) Plot a single curve (and ribbon) across agecat
ggplot(result, aes(x = agecat, y = IRRsuccess, group = 1)) +
  # CI ribbon (single fill color)
  geom_ribbon(aes(ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
              fill = "lightblue", alpha = 0.3) +
  # line + points
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  # adjust y if needed
  scale_y_continuous(limits = c(0, 0.1)) +
  labs(
    x     = "Age Category",
    y     = "Incidence Rate (per person-year)",
    title = "Smoking Incidence by Age"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1)
  )

colnames(smoking_incidence)

result <- smoking_incidence[
  IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = agecat       # only grouping by agecat now
]

# 2) Plot a single curve (and ribbon) across agecat
ggplot(result, aes(x = agecat, y = IRRsuccess, group = 1)) +
  # CI ribbon (single fill color)
  geom_ribbon(aes(ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
              fill = "lightblue", alpha = 0.3) +
  # line + points
  geom_line(size = 1, color = "steelblue") +
  geom_point(size = 2, color = "steelblue") +
  # adjust y if needed
  scale_y_continuous(limits = c(0, 0.1)) +
  labs(
    x     = "Age Category",
    y     = "Incidence Rate (per person-year)",
    title = "Smoking Incidence by Age"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1)
  )

