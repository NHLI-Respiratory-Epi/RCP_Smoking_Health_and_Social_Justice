
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
census2$agecat[census2$ageextract %in%  16:17] <- "16-17"
census2$agecat[census2$ageextract %in%  18:21] <- "18-21"
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
                            "/prevalence_results_all_ages_", x, ".xlsx"), sheet = "age_gender_region_IMD10")
  steps$year <- x
  return(steps)
}
)

smoking_prevalence <- rbindlist(smoking_prevalence)

smoking_prevalence$agecat <- NA
smoking_prevalence$agecat[smoking_prevalence$age %in%  11:15] <- "11-15"
smoking_prevalence$agecat[smoking_prevalence$age %in%  16:17] <- "16-17"
smoking_prevalence$agecat[smoking_prevalence$age %in%  18:21] <- "18-21"
smoking_prevalence$agecat[smoking_prevalence$age %in%  22:24] <- "22-24"
smoking_prevalence$agecat[smoking_prevalence$age %in%  25:29] <- "25-29"
smoking_prevalence$agecat[smoking_prevalence$age %in%  30:34] <- "30-34"
smoking_prevalence$agecat[smoking_prevalence$age %in%  35:39] <- "35-39"
smoking_prevalence$agecat[smoking_prevalence$age %in%  40:44] <- "40-44"
smoking_prevalence$agecat[smoking_prevalence$age %in%  45:49] <- "45-49"
smoking_prevalence$agecat[smoking_prevalence$age %in%  50:54] <- "50-54"
smoking_prevalence$agecat[smoking_prevalence$age %in%  55:59] <- "55-59"
smoking_prevalence$agecat[smoking_prevalence$age %in%  60:64] <- "60-64"
smoking_prevalence$agecat[smoking_prevalence$age %in%  65:69] <- "65-69"
smoking_prevalence$agecat[smoking_prevalence$age %in%  70:74] <- "70-74"
smoking_prevalence$agecat[smoking_prevalence$age %in%  75:79] <- "75-79"
smoking_prevalence$agecat[smoking_prevalence$age %in%  80:84] <- "80-84"
smoking_prevalence$agecat[smoking_prevalence$age %in%  85:89] <- "85-89"
smoking_prevalence$agecat[smoking_prevalence$age >=  90] <- "90+"

smoking_prevalence <- smoking_prevalence[IMD10 != "IMD unavailable"]

smoking_prevalence$IMD5 <- NA
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 1] <- "1 (least deprived)"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 2] <- "1 (least deprived)"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 3] <- "2"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 4] <- "2"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 5] <- "3"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 6] <- "3"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 7] <- "4"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 8] <- "4"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 9] <- "5 (most deprived)"
smoking_prevalence$IMD5[smoking_prevalence$IMD10 == 10] <- "5 (most deprived)"

smoking_prevalence$IMD5 <- factor(smoking_prevalence$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(smoking_prevalence)
table(smoking_prevalence$IMD5)

# calculate current weights

smoking_prevalence <- smoking_prevalence %>% rename(sex = gender)

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
result_weighted <- weighted[!agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(standardised_current_smoking_numerator) / sum(census_N)
#    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
 #   prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year)
]

# 2) Summarise prevalence by year and IMD5
result_unweighted <- smoking_prevalence[
  !agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year)
]

result_weighted
result_unweighted

# any tobacco smoking 17.5%

result_unweighted_no_imd_restrict <- smoking_prevalence[
  !agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year)
]

# IMD5

# 2) Summarise prevalence by year and IMD5
result_unweighted <- smoking_prevalence[
  !agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year, IMD5)
]

# national average (across all IMD quintiles)
national <- smoking_prevalence[
  !agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = year
]

library(ggplot2)
library(scales)

library(ggplot2)
library(scales)
library(ggplot2)
library(scales)

library(ggplot2)
library(scales)

# define a vector of colours for IMD1–5 (you can tweak these)
imd_cols <- c("1 (least deprived)" = "#F8766D",  # red
              "2" = "#7CAE00",  # green
              "3" = "#00BFC4",  # cyan
              "4" = "#C77CFF",  # purple
              "5 (most deprived)" = "#FF61CC",  # pink
              "National average" = "black")

# all solid for IMD, dashed for national
line_types <- c("1 (least deprived)" = "solid",
                "2" = "solid",
                "3" = "solid",
                "4" = "solid",
                "5 (most deprived)" = "solid",
                "National average" = "dashed")

# solid circle for IMD, diamond for national
pt_shapes  <- c("1 (least deprived)" = 16,
                "2" = 16,
                "3" = 16,
                "4" = 16,
                "5 (most deprived)" = 16,
                "National average" = 18)

# 2) Plot
ggplot() +
  # faint horizontal grid lines at 0%, 5%, 10%, …, 30%
  geom_hline(
    yintercept  = seq(0, 0.30, by = 0.05),
    color       = "grey80",
    size        = 0.3,
    linetype    = "dashed",
    inherit.aes = FALSE
  ) +
  # IMD5 lines
  geom_line(
    data = result_unweighted,
    aes(x = year,
        y = prevalence,
        color    = IMD5,
        linetype = IMD5,
        group    = IMD5),
    size = 1
  ) +
  # IMD5 points
  geom_point(
    data = result_unweighted,
    aes(x = year,
        y = prevalence,
        color    = IMD5,
        shape    = IMD5,
        group    = IMD5),
    size = 2
  ) +
  # national dashed line
  geom_line(
    data = national,
    aes(x = year,
        y = prevalence,
        color    = "National average",
        linetype = "National average"),
    size        = 1.2,
    inherit.aes = FALSE
  ) +
  # national diamond points
  geom_point(
    data = national,
    aes(x = year,
        y = prevalence,
        color = "National average",
        shape = "National average"),
    size        = 3,
    inherit.aes = FALSE
  ) +
  # show every year on x-axis
  scale_x_continuous(
    breaks = seq(min(result_unweighted$year),
                 max(result_unweighted$year),
                 by = 1)
  ) +
  # y-axis as percentage with fixed limits
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    limits = c(0, 0.30)
  ) +
  # combined legend for color, linetype, shape
  scale_color_manual(
    name   = "IMD Quintile",
    values = imd_cols,
    breaks = names(imd_cols)
  ) +
  scale_linetype_manual(
    name   = "IMD Quintile",
    values = line_types,
    breaks = names(line_types)
  ) +
  scale_shape_manual(
    name   = "IMD Quintile",
    values = pt_shapes,
    breaks = names(pt_shapes)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking prevalence (%)",
    title = "Figure 1. Smoking prevalence over time by IMD"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

# by region:

# Replace IMD5 with region
result_unweighted <- smoking_prevalence[
  !agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year, region)
]

# National average remains the same
national <- smoking_prevalence[
  !agecat %in% c("11-15", "16-17"),
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = year
]
library(ggplot2)
library(RColorBrewer)
library(scales)

# 1) Prep your data as before…

# 2) Define legend levels & palettes
region_levels <- unique(result_unweighted$region)
legend_levels <- c(region_levels, "National average")

# a) colours
region_cols <- setNames(
  brewer.pal(length(region_levels), "Set3"),
  region_levels
)
region_cols["National average"] <- "black"

# b) linetypes
line_types <- setNames(
  rep("solid", length(region_levels)),
  region_levels
)
line_types["National average"] <- "dashed"

pt_shapes <- setNames(
  c(21:25, 0:4),
  c(region_levels, "National average")
)
pt_shapes["National average"] <- 18  # also filled

# 3) Plot
ggplot() +
  # background grid
  geom_hline(
    yintercept = seq(0, .30, by = .05),
    color      = "grey80",
    size       = 0.3,
    linetype   = "dashed"
  ) +

  # region ribbons, lines, points
  geom_ribbon(
    data = result_unweighted,
    aes(year, ymin = prev_lo, ymax = prev_hi, fill = region, group = region),
    alpha = 0.2, color = NA
  ) +
  geom_line(
    data = result_unweighted,
    aes(year, prevalence, color = region, group = region),
    size = 1
  ) +
  geom_point(
    data = result_unweighted,
    aes(year, prevalence, color = region, shape = region, group = region,
        fill = region),
    size = 2
  ) +

  # national ribbon, line & point
  geom_ribbon(
    data        = national,
    aes(year, ymin = prev_lo, ymax = prev_hi),
    fill        = "black",
    alpha       = 0.1,
    inherit.aes = FALSE
  ) +
  geom_line(
    data        = national,
    aes(year, prevalence, color = "National average", group = "National average"),
    linetype    = line_types["National average"],
    size        = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data        = national,
    aes(year, prevalence, color = "National average", shape = "National average",
        fill = "National average"),
    size        = 3,
    inherit.aes = FALSE
  ) +

  # axes
  scale_x_continuous(breaks = seq(min(result_unweighted$year),
                                  max(result_unweighted$year), by = 1)) +
  scale_y_continuous(
    labels = percent_format(1),
    limits = c(0.0, 0.20)
  ) +

  # combined Region guide: colour + shape + linetype
  # scales

  scale_color_manual(
    name   = "Region",
    values = region_cols[legend_levels],
    breaks = legend_levels,
    guide  = guide_legend(
      override.aes = list(
        shape    = pt_shapes[legend_levels],
        linetype = line_types[legend_levels],
        fill     = region_cols[legend_levels]  # <— legend fill keys
      )
    )
  ) +
  scale_shape_manual(
    name   = "Region",
    values = pt_shapes,
    guide  = FALSE
  ) +
  scale_linetype_manual(
    name   = "Region",
    values = line_types,
    guide  = FALSE
  ) +
  scale_fill_manual(
    name   = "Region",
    values = region_cols,
    guide  = FALSE
  ) +

  # labels & theme
  labs(
    x     = "Year",
    y     = "Smoking prevalence (%)",
    title = "Figure 2. Smoking prevalence over time by region"
  ) +
  theme_bw() +
  theme(
    panel.grid.major  = element_blank(),
    panel.grid.minor  = element_blank(),
    axis.text.x       = element_text(angle = 45, hjust = 1),
    legend.position   = "right"
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

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

# # 2) standardised sex results
# sex_result_weighted <- weighted[,
#                             .(
#                               prevalence = sum(standardised_current_smoking_numerator) / sum(census_N)
#                               #    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
#                               #   prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
#                             ),
#                             by = .(year, sex)
# ]

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

sex_cols <- c("Male" = "#1F78B4",      # blue
              "Female" = "#E31A1C",    # red
              "National average" = "black")

line_types <- c("Male" = "solid",
                "Female" = "solid",
                "National average" = "dashed")

pt_shapes  <- c("Male" = 16,
                "Female" = 16,
                "National average" = 18)
ggplot() +
  geom_hline(
    yintercept  = seq(0, 0.30, by = 0.05),
    color       = "grey80",
    size        = 0.3,
    linetype    = "dashed",
    inherit.aes = FALSE
  ) +
  geom_line(
    data = result_sex,
    aes(x = year,
        y = prevalence,
        color    = sex,
        linetype = sex,
        group    = sex),
    size = 1
  ) +
  geom_point(
    data = result_sex,
    aes(x = year,
        y = prevalence,
        color    = sex,
        shape    = sex,
        group    = sex),
    size = 2
  ) +
  geom_line(
    data = national,
    aes(x = year,
        y = prevalence,
        color    = "National average",
        linetype = "National average"),
    size        = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = national,
    aes(x = year,
        y = prevalence,
        color = "National average",
        shape = "National average"),
    size        = 3,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(min(result_sex$year),
                 max(result_sex$year),
                 by = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.30)
  ) +
  scale_color_manual(
    name   = "Sex",
    values = sex_cols,
    breaks = names(sex_cols)
  ) +
  scale_linetype_manual(
    name   = "Sex",
    values = line_types,
    breaks = names(line_types)
  ) +
  scale_shape_manual(
    name   = "Sex",
    values = pt_shapes,
    breaks = names(pt_shapes)
  ) +
  labs(
    x     = "Year",
    y     = "Smoking prevalence (%)",
    title = "Figure 3. Smoking prevalence over time by sex"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

############ by age

# 2) Summarise prevalence by year and IMD5
# Restrict and re-factor before summarising
age_result_step1 <- smoking_prevalence[
  agecat %in% c("16-17", "18-21", "22-24", "25-29", "30-34") &
    IMD5 != "IMD unavailable" & sex != "Indeterminate"
]
age_result_step1[, agecat := factor(agecat, levels = c("16-17", "18-21", "22-24", "25-29", "30-34"))]

result_age <- age_result_step1[
  , .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year, agecat)
]

# # 2) standardised sex results
# sex_result_weighted <- weighted[,
#                             .(
#                               prevalence = sum(standardised_current_smoking_numerator) / sum(census_N)
#                               #    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
#                               #   prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
#                             ),
#                             by = .(year, sex)
# ]

# national average (across all IMD quintiles)
national <- smoking_prevalence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total)),
    n_current = sum(n_current),
    n_total = sum(n_total)
  ),
  by = year
]

age_categories <- c("16-17", "18-21", "22-24", "25-29", "30-34")

age_colors <- c(
  "16-17" = "#1b9e77",
  "18-21" = "#d95f02",
  "22-24" = "#7570b3",
  "25-29" = "#e7298a",
  "30-34" = "#66a61e",
  "National average" = "black"
)

line_types <- c(
  setNames(rep("solid", length(age_categories)), age_categories),
  "National average" = "dashed"
)

pt_shapes <- c(
  setNames(rep(16, length(age_categories)), age_categories),
  "National average" = 18
)

ggplot() +
  # Horizontal reference lines
  geom_hline(
    yintercept  = seq(0, 0.30, by = 0.05),
    color       = "grey80",
    size        = 0.3,
    linetype    = "dashed"
  ) +

  # Lines for age categories
  geom_line(
    data = result_age,
    aes(x = year, y = prevalence, color = agecat, linetype = agecat, group = agecat),
    size = 1
  ) +

  # Points for age categories
  geom_point(
    data = result_age,
    aes(x = year, y = prevalence, color = agecat, shape = agecat, group = agecat),
    size = 2
  ) +

  # National average line
  geom_line(
    data = national,
    aes(x = year, y = prevalence, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +

  # National average points
  geom_point(
    data = national,
    aes(x = year, y = prevalence, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # Axis setup
  scale_x_continuous(
    breaks = seq(min(result_age$year), max(result_age$year), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    limits = c(0, 0.30)
  ) +

  # Manual color, linetype, and shape scales
  scale_color_manual(
    name = "Age category",
    values = age_colors,
    breaks = c(age_categories, "National average")
  ) +
  scale_linetype_manual(
    name = "Age category",
    values = line_types,
    breaks = c(age_categories, "National average")
  ) +
  scale_shape_manual(
    name = "Age category",
    values = pt_shapes,
    breaks = c(age_categories, "National average")
  ) +

  # Labels and theme
  labs(
    x = "Year",
    y = "Smoking prevalence (%)",
    title = "Figure 4. Smoking prevalence over time by age category"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

####### incidence

# # # # # # smoking_incidence

cohort <- 2018:2024

smoking_incidence <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_incidence_results_all_ages_", x, ".xlsx"), sheet = "age_gender_region_IMD10")
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

smoking_incidence$agecat <- NA
smoking_incidence$agecat[smoking_incidence$age %in%  11:15] <- "11-15"
smoking_incidence$agecat[smoking_incidence$age %in%  16:17] <- "16-17"
smoking_incidence$agecat[smoking_incidence$age %in%  18:21] <- "18-21"
smoking_incidence$agecat[smoking_incidence$age %in%  22:24] <- "22-24"
smoking_incidence$agecat[smoking_incidence$age %in%  25:29] <- "25-29"
smoking_incidence$agecat[smoking_incidence$age %in%  30:34] <- "30-34"
smoking_incidence$agecat[smoking_incidence$age %in%  35:39] <- "35-39"
smoking_incidence$agecat[smoking_incidence$age %in%  40:44] <- "40-44"
smoking_incidence$agecat[smoking_incidence$age %in%  45:49] <- "45-49"
smoking_incidence$agecat[smoking_incidence$age %in%  50:54] <- "50-54"
smoking_incidence$agecat[smoking_incidence$age %in%  55:59] <- "55-59"
smoking_incidence$agecat[smoking_incidence$age %in%  60:64] <- "60-64"
smoking_incidence$agecat[smoking_incidence$age %in%  65:69] <- "65-69"
smoking_incidence$agecat[smoking_incidence$age %in%  70:74] <- "70-74"
smoking_incidence$agecat[smoking_incidence$age %in%  75:79] <- "75-79"
smoking_incidence$agecat[smoking_incidence$age %in%  80:84] <- "80-84"
smoking_incidence$agecat[smoking_incidence$age %in%  85:89] <- "85-89"
smoking_incidence$agecat[smoking_incidence$age >=  90] <- "90+"

smoking_incidence <- smoking_incidence[IMD10 != "IMD unavailable"]

smoking_incidence$IMD5 <- NA
smoking_incidence$IMD5[smoking_incidence$IMD10 == 1] <- "1 (least deprived)"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 2] <- "1 (least deprived)"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 3] <- "2"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 4] <- "2"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 5] <- "3"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 6] <- "3"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 7] <- "4"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 8] <- "4"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 9] <- "5 (most deprived)"
smoking_incidence$IMD5[smoking_incidence$IMD10 == 10] <- "5 (most deprived)"

smoking_incidence$IMD5 <- factor(smoking_incidence$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(smoking_incidence)
table(smoking_incidence$IMD5)

# Regions look too strange and messy. ##################

# Filter by region instead of age category
region_result_step1_inc <- smoking_incidence[
  IMD5 != "IMD unavailable" & gender != "Indeterminate" & agecat != "11-15"
]

# Summarise by year and region
result_region_inc <- region_result_step1_inc[
  , .(
    IRRsuccess    = sum(n_never_incidence) / sum(person_years_never),
    IRRsuccess_lo = get_ci_lo(sum(n_never_incidence), sum(person_years_never)),
    IRRsuccess_hi = get_ci_hi(sum(n_never_incidence), sum(person_years_never))
  ),
  by = .(year, region)
]

# # 2) standardised sex results
# sex_result_weighted <- weighted[,
#                             .(
#                               prevalence = sum(standardised_current_smoking_numerator) / sum(census_N)
#                               #    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
#                               #   prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
#                             ),
#                             by = .(year, sex)
# ]

# national average (across all IMD quintiles)
national_inc <- smoking_incidence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable" & gender != "Indeterminate",
  .(
    IRRsuccess    = sum(n_never_incidence) / sum(person_years_never),
    IRRsuccess_lo = get_ci_lo(sum(n_never_incidence), sum(person_years_never)),
    IRRsuccess_hi = get_ci_hi(sum(n_never_incidence), sum(person_years_never))
  ),
  by = year
]

# Multiply by 1000 for rates per 1000 person-years
result_region_inc[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

national_inc[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

region_levels_inc <- sort(unique(result_region_inc$region))

region_colors_inc <- setNames(
  RColorBrewer::brewer.pal(length(region_levels_inc), "Set3")[1:length(region_levels_inc)],
  region_levels_inc
)

region_colors_inc["National average"] <- "black"

line_types_inc <- setNames(rep("solid", length(region_levels_inc)), region_levels_inc)
pt_shapes_inc  <- setNames(rep(16, length(region_levels_inc)), region_levels_inc)

line_types_inc["National average"] <- "dashed"
pt_shapes_inc["National average"]  <- 18

ggplot() +
  geom_hline(
    yintercept = seq(0, max(result_region_inc$IRRsuccess, na.rm = TRUE), by = 2.5),
    color = "grey80",
    size = 0.3,
    linetype = "dashed"
  ) +

  # Region lines
  geom_line(
    data = result_region_inc,
    aes(x = year, y = IRRsuccess, color = region, linetype = region, group = region),
    size = 1
  ) +

  # Region points
  geom_point(
    data = result_region_inc,
    aes(x = year, y = IRRsuccess, color = region, shape = region, group = region),
    size = 2
  ) +

  # National average line
  geom_line(
    data = national_inc,
    aes(x = year, y = IRRsuccess, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +

  # National average points
  geom_point(
    data = national_inc,
    aes(x = year, y = IRRsuccess, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # # National average confidence intervals
  # # Region ribbons
  # geom_ribbon(
  #   data = result_region_inc,
  #   aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = region, group = region),
  #   alpha = 0.2,
  #   color = NA
  # ) +

  # Scales
  scale_x_continuous(
    breaks = seq(min(result_region_inc$year), max(result_region_inc$year), by = 1)
  ) +
  # Change this part of your ggplot call
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1),
    limits = c(0, 10)  # 0 to 0.01 (original scale) * 1000
  ) +
  guides(fill = "none") +
  # Aesthetic mappings
  scale_color_manual(
    name = "Region",
    values = region_colors_inc,
    breaks = c(region_levels_inc, "National average")
  ) +
  scale_linetype_manual(
    name = "Region",
    values = line_types_inc,
    breaks = c(region_levels_inc, "National average")
  ) +
  scale_shape_manual(
    name = "Region",
    values = pt_shapes_inc,
    breaks = c(region_levels_inc, "National average")
  ) +

  labs(
    x = "Year",
    y = "Incidence rate (per 1,000 person-years)",  # updated label
    title = "Smoking initiation incidence over time by region (with 95% confidence intervals)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),  # removes vertical stripes
    panel.grid.major.y = element_line(color = "grey80", size = 0.3, linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

# INCIDENCE VS imd

# Filter and summarise by IMD5 instead of region
imd_result_step1_inc <- smoking_incidence[
  IMD5 != "IMD unavailable" & gender != "Indeterminate" & agecat != "11-15"
]

# Summarise by year and IMD5
result_imd_inc <- imd_result_step1_inc[
  , .(
    IRRsuccess    = sum(n_never_incidence) / sum(person_years_never),
    IRRsuccess_lo = get_ci_lo(sum(n_never_incidence), sum(person_years_never)),
    IRRsuccess_hi = get_ci_hi(sum(n_never_incidence), sum(person_years_never))
  ),
  by = .(year, IMD5)
]

# national average (across all IMD quintiles)
national_inc <- smoking_incidence[
  !agecat %in% c("11-15") & IMD5 != "IMD unavailable" & gender != "Indeterminate",
  .(
    IRRsuccess    = sum(n_never_incidence) / sum(person_years_never),
    IRRsuccess_lo = get_ci_lo(sum(n_never_incidence), sum(person_years_never)),
    IRRsuccess_hi = get_ci_hi(sum(n_never_incidence), sum(person_years_never))
  ),
  by = year
]

result_imd_inc[, `:=`(
  IRRsuccess    = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

national_inc[, `:=`(
  IRRsuccess    = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

imd_levels_inc <- c("1 (least deprived)", "2", "3", "4", "5 (most deprived)")

imd_colors_inc <-  imd_cols

line_types_inc <- c(
  setNames(rep("solid", length(imd_levels_inc)), imd_levels_inc),
  "National average" = "dashed"
)

pt_shapes_inc <- c(
  setNames(rep(16, length(imd_levels_inc)), imd_levels_inc),
  "National average" = 18
)

ggplot() +
  geom_hline(
    yintercept = seq(0, 15, by = 2.5),
    color = "grey80",
    size = 0.3,
    linetype = "dashed"
  ) +

  # IMD ribbons
  geom_ribbon(
    data = result_imd_inc,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5, group = IMD5),
    alpha = 0.2,
    color = NA
  ) +

  # IMD lines
  geom_line(
    data = result_imd_inc,
    aes(x = year, y = IRRsuccess, color = IMD5, linetype = IMD5, group = IMD5),
    size = 1
  ) +

  # IMD points
  geom_point(
    data = result_imd_inc,
    aes(x = year, y = IRRsuccess, color = IMD5, shape = IMD5, group = IMD5),
    size = 2
  ) +

  # National ribbon
  geom_ribbon(
    data = national_inc,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
    fill = "black",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +

  # National line and point
  geom_line(
    data = national_inc,
    aes(x = year, y = IRRsuccess, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = national_inc,
    aes(x = year, y = IRRsuccess, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # Axis scales
  scale_x_continuous(
    breaks = seq(min(result_imd_inc$year), max(result_imd_inc$year), by = 1)
  ) +
  scale_y_continuous(
    labels = scales::number_format(accuracy = 0.1),
    limits = c(0, 15)
  ) +

  # Aesthetic mappings
  scale_color_manual(
    name = "IMD Quintile",
    values = imd_colors_inc,
    breaks = c(imd_levels_inc, "National average")
  ) +
  scale_fill_manual(
    name = "IMD Quintile",
    values = imd_colors_inc,
    breaks = imd_levels_inc
  ) +
  scale_linetype_manual(
    name = "IMD Quintile",
    values = line_types_inc,
    breaks = c(imd_levels_inc, "National average")
  ) +
  scale_shape_manual(
    name = "IMD Quintile",
    values = pt_shapes_inc,
    breaks = c(imd_levels_inc, "National average")
  ) +
  guides(fill = "none") +

  # Labels and theme
  labs(
    x = "Year",
    y = "Incidence rate (per 1,000 person-years)",
    title = "Figure 5. Smoking initiation incidence over time by IMD quintile"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey80", size = 0.3, linetype = "dashed"),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

# incidence over time

################ vaping

# # # # # # vaping_prevalence

cohort <- 2018:2024

vaping_prevalence <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/prevalences/", x,
                            "/vaping_prevalence_results_", x, ".xlsx"), sheet = "age_gender_region_IMD10")
  steps$year <- x
  return(steps)
}
)

vaping_prevalence <- rbindlist(vaping_prevalence)

vaping_prevalence <- vaping_prevalence[IMD10 != "IMD unavailable"]

vaping_prevalence$IMD5 <- NA
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 1] <- "1 (least deprived)"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 2] <- "1 (least deprived)"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 3] <- "2"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 4] <- "2"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 5] <- "3"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 6] <- "3"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 7] <- "4"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 8] <- "4"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 9] <- "5 (most deprived)"
vaping_prevalence$IMD5[vaping_prevalence$IMD10 == 10] <- "5 (most deprived)"

vaping_prevalence$IMD5 <- factor(vaping_prevalence$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(vaping_prevalence)
table(vaping_prevalence$IMD5)

vaping_prevalence$agecat <- NA
vaping_prevalence$agecat[vaping_prevalence$age %in%  11:15] <- "11-15"
vaping_prevalence$agecat[vaping_prevalence$age %in%  16:17] <- "16-17"
vaping_prevalence$agecat[vaping_prevalence$age %in%  18:21] <- "18-21"
vaping_prevalence$agecat[vaping_prevalence$age %in%  22:24] <- "22-24"
vaping_prevalence$agecat[vaping_prevalence$age %in%  25:29] <- "25-29"
vaping_prevalence$agecat[vaping_prevalence$age %in%  30:34] <- "30-34"
vaping_prevalence$agecat[vaping_prevalence$age %in%  35:39] <- "35-39"
vaping_prevalence$agecat[vaping_prevalence$age %in%  40:44] <- "40-44"
vaping_prevalence$agecat[vaping_prevalence$age %in%  45:49] <- "45-49"
vaping_prevalence$agecat[vaping_prevalence$age %in%  50:54] <- "50-54"
vaping_prevalence$agecat[vaping_prevalence$age %in%  55:59] <- "55-59"
vaping_prevalence$agecat[vaping_prevalence$age %in%  60:64] <- "60-64"
vaping_prevalence$agecat[vaping_prevalence$age %in%  65:69] <- "65-69"
vaping_prevalence$agecat[vaping_prevalence$age %in%  70:74] <- "70-74"
vaping_prevalence$agecat[vaping_prevalence$age %in%  75:79] <- "75-79"
vaping_prevalence$agecat[vaping_prevalence$age %in%  80:84] <- "80-84"
vaping_prevalence$agecat[vaping_prevalence$age %in%  85:89] <- "85-89"
vaping_prevalence$agecat[vaping_prevalence$age >=  90] <- "90+"

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
result_vaping_prevalence <- vaping_prevalence[
  !(age %in% 11:15) &
    IMD5 != "IMD unavailable" & gender != "Indeterminate",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total)),
    n_current = sum(n_current),
    n_total = sum(n_total)
  ),
  by = .(year, IMD5)
]

result_vaping_prevalence

national_result_vaping_prevalence <- vaping_prevalence[!(age %in% 11:15) &
  IMD5 != "IMD unavailable" & gender != "Indeterminate",
  .(
    prevalence = sum(n_current) / sum(n_total),
    prev_lo    = get_prev_lo(sum(n_current), sum(n_total)),
    prev_hi    = get_prev_hi(sum(n_current), sum(n_total))
  ),
  by = .(year)
]

# define a vector of colours for IMD1–5 (you can tweak these)
imd_cols <- c("1 (least deprived)" = "#F8766D",  # red
              "2" = "#7CAE00",  # green
              "3" = "#00BFC4",  # cyan
              "4" = "#C77CFF",  # purple
              "5 (most deprived)" = "#FF61CC",  # pink
              "National average" = "black")

# all solid for IMD, dashed for national
line_types <- c("1 (least deprived)" = "solid",
                "2" = "solid",
                "3" = "solid",
                "4" = "solid",
                "5 (most deprived)" = "solid",
                "National average" = "dashed")

# solid circle for IMD, diamond for national
pt_shapes  <- c("1 (least deprived)" = 16,
                "2" = 16,
                "3" = 16,
                "4" = 16,
                "5 (most deprived)" = 16,
                "National average" = 18)

ggplot() +
  # Horizontal grid lines
  geom_hline(
    yintercept  = seq(0, 0.10, by = 0.01),
    color       = "grey80",
    size        = 0.3,
    linetype    = "dashed",
    inherit.aes = FALSE
  ) +

  # Vaping prevalence lines by IMD5
  geom_line(
    data = result_vaping_prevalence,
    aes(x = year,
        y = prevalence,
        color    = IMD5,
        linetype = IMD5,
        group    = IMD5),
    size = 1
  ) +

  # Vaping prevalence points by IMD5
  geom_point(
    data = result_vaping_prevalence,
    aes(x = year,
        y = prevalence,
        color    = IMD5,
        shape    = IMD5,
        group    = IMD5),
    size = 2
  ) +
  # Confidence interval ribbons for each IMD5 group
  geom_ribbon(
    data = result_vaping_prevalence,
    aes(x = year, ymin = prev_lo, ymax = prev_hi, fill = IMD5, group = IMD5),
    alpha = 0.2,
    color = NA
  ) +

  # National average line
  geom_line(
    data = national_result_vaping_prevalence,
    aes(x = year,
        y = prevalence,
        color    = "National average",
        linetype = "National average"),
    size        = 1.2,
    inherit.aes = FALSE
  ) +

  # National average points
  geom_point(
    data = national_result_vaping_prevalence,
    aes(x = year,
        y = prevalence,
        color = "National average",
        shape = "National average"),
    size        = 3,
    inherit.aes = FALSE
  ) +

  # X-axis by year
  scale_x_continuous(
    breaks = seq(min(result_vaping_prevalence$year),
                 max(result_vaping_prevalence$year),
                 by = 1)
  ) +

  # Y-axis as percentage
  scale_y_continuous(
    labels = percent_format(accuracy = 0.1),
    limits = c(0, 0.05)
  ) +

  # Legend settings
  scale_color_manual(
    name   = "IMD Quintile",
    values = imd_cols,
    breaks = names(imd_cols)
  ) +

  scale_linetype_manual(
    name   = "IMD Quintile",
    values = line_types,
    breaks = names(line_types)
  ) +
  scale_shape_manual(
    name   = "IMD Quintile",
    values = pt_shapes,
    breaks = names(pt_shapes)
  ) +
  # Hide ribbon legend
  guides(fill = "none") +
  scale_fill_manual(
    values = imd_cols,
    breaks = names(imd_cols)
  ) +

  # Labels and theme
  labs(
    x     = "Year",
    y     = "Vaping prevalence (%)",
    title = "Figure 7. Vaping prevalence over time by IMD"
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

# let's create some longitudinal graphs.

cohort <- 2018:2024

cessattempt <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_cessation_attempt_results_all_ages_", x, ".xlsx"), sheet = "age_gender_region_IMD10_eth5")
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

cessattempt$agecat <- NA
cessattempt$agecat[cessattempt$age %in%  11:15] <- "11-15"
cessattempt$agecat[cessattempt$age %in%  16:17] <- "16-17"
cessattempt$agecat[cessattempt$age %in%  18:21] <- "18-21"
cessattempt$agecat[cessattempt$age %in%  22:24] <- "22-24"
cessattempt$agecat[cessattempt$age %in%  25:29] <- "25-29"
cessattempt$agecat[cessattempt$age %in%  30:34] <- "30-34"
cessattempt$agecat[cessattempt$age %in%  35:39] <- "35-39"
cessattempt$agecat[cessattempt$age %in%  40:44] <- "40-44"
cessattempt$agecat[cessattempt$age %in%  45:49] <- "45-49"
cessattempt$agecat[cessattempt$age %in%  50:54] <- "50-54"
cessattempt$agecat[cessattempt$age %in%  55:59] <- "55-59"
cessattempt$agecat[cessattempt$age %in%  60:64] <- "60-64"
cessattempt$agecat[cessattempt$age %in%  65:69] <- "65-69"
cessattempt$agecat[cessattempt$age %in%  70:74] <- "70-74"
cessattempt$agecat[cessattempt$age %in%  75:79] <- "75-79"
cessattempt$agecat[cessattempt$age %in%  80:84] <- "80-84"
cessattempt$agecat[cessattempt$age %in%  85:89] <- "85-89"
cessattempt$agecat[cessattempt$age >=  90] <- "90+"

cessattempt <- cessattempt[IMD10 != "IMD unavailable"]

cessattempt$IMD5 <- NA
cessattempt$IMD5[cessattempt$IMD10 == 1] <- "1 (least deprived)"
cessattempt$IMD5[cessattempt$IMD10 == 2] <- "1 (least deprived)"
cessattempt$IMD5[cessattempt$IMD10 == 3] <- "2"
cessattempt$IMD5[cessattempt$IMD10 == 4] <- "2"
cessattempt$IMD5[cessattempt$IMD10 == 5] <- "3"
cessattempt$IMD5[cessattempt$IMD10 == 6] <- "3"
cessattempt$IMD5[cessattempt$IMD10 == 7] <- "4"
cessattempt$IMD5[cessattempt$IMD10 == 8] <- "4"
cessattempt$IMD5[cessattempt$IMD10 == 9] <- "5 (most deprived)"
cessattempt$IMD5[cessattempt$IMD10 == 10] <- "5 (most deprived)"

cessattempt$IMD5 <- factor(cessattempt$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(cessattempt)

cessattempt <- cessattempt[year != 2024]

result <- cessattempt[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_successful_attempts) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_successful_attempts), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_successful_attempts), sum(person_years_total))
  ),
  .(year, IMD5)
]

table(cessattempt$year)
summary(result)
plot(result$year, result$IRRsuccess)

cessattempt[, .(total_ppl = sum(ppl_N_total),
                total_py = sum(person_years_total),
                ppl_to_py_ratio = sum(ppl_N_total)/sum(person_years_total)),
            .(year)]
head(cessattempt)

# Add national average
national_cessation <- cessattempt[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_successful_attempts) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_successful_attempts), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_successful_attempts), sum(person_years_total))
  ),
  by = year
]

result[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

national_cessation[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

# Plot
ggplot() +
  # Horizontal reference lines
  geom_hline(
    yintercept = seq(0, 110, by = 10),
    color = "grey90",
    size = 0.3,
    linetype = "dashed"
  ) +

  # CI ribbons for IMD5
  geom_ribbon(
    data = result,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5, group = IMD5),
    alpha = 0.2,
    color = NA
  ) +

  # IMD lines and points
  geom_line(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, linetype = IMD5, group = IMD5),
    size = 1
  ) +
  geom_point(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, shape = IMD5, group = IMD5),
    size = 2
  ) +

  # National average ribbon
  geom_ribbon(
    data = national_cessation,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
    fill = "black",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +

  # National average line and point
  geom_line(
    data = national_cessation,
    aes(x = year, y = IRRsuccess, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = national_cessation,
    aes(x = year, y = IRRsuccess, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # Scales
  scale_x_continuous(
    breaks = seq(min(result$year), max(result$year), by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 110),
    labels = scales::number_format(accuracy = 1)
  ) +

  # Aesthetic legends
  scale_color_manual(name = "IMD Quintile", values = imd_cols) +
  scale_fill_manual(name = "IMD Quintile", values = imd_cols) +
  scale_linetype_manual(name = "IMD Quintile", values = line_types) +
  scale_shape_manual(name = "IMD Quintile", values = pt_shapes) +
  guides(fill = "none") +

  # Labels and theme
  labs(
    x = "Year",
    y = "Cessation incidence (per 1,000 person-years)",
    title = "Figure 6. Successful smoking cessation incidence over time by IMD quintile"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

# # # # # # drug prescription

cohort <- 2018:2024

prescription <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_drug_prescription_incidence_results_all_ages_", x, ".xlsx"), sheet = "age_gender_region_IMD10_eth5")
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

prescription$agecat <- NA
prescription$agecat[prescription$age %in%  11:15] <- "11-15"
prescription$agecat[prescription$age %in%  16:17] <- "16-17"
prescription$agecat[prescription$age %in%  18:21] <- "18-21"
prescription$agecat[prescription$age %in%  22:24] <- "22-24"
prescription$agecat[prescription$age %in%  25:29] <- "25-29"
prescription$agecat[prescription$age %in%  30:34] <- "30-34"
prescription$agecat[prescription$age %in%  35:39] <- "35-39"
prescription$agecat[prescription$age %in%  40:44] <- "40-44"
prescription$agecat[prescription$age %in%  45:49] <- "45-49"
prescription$agecat[prescription$age %in%  50:54] <- "50-54"
prescription$agecat[prescription$age %in%  55:59] <- "55-59"
prescription$agecat[prescription$age %in%  60:64] <- "60-64"
prescription$agecat[prescription$age %in%  65:69] <- "65-69"
prescription$agecat[prescription$age %in%  70:74] <- "70-74"
prescription$agecat[prescription$age %in%  75:79] <- "75-79"
prescription$agecat[prescription$age %in%  80:84] <- "80-84"
prescription$agecat[prescription$age %in%  85:89] <- "85-89"
prescription$agecat[prescription$age >=  90] <- "90+"

prescription <- prescription[IMD10 != "IMD unavailable"]

prescription$IMD5 <- NA
prescription$IMD5[prescription$IMD10 == 1] <- "1 (least deprived)"
prescription$IMD5[prescription$IMD10 == 2] <- "1 (least deprived)"
prescription$IMD5[prescription$IMD10 == 3] <- "2"
prescription$IMD5[prescription$IMD10 == 4] <- "2"
prescription$IMD5[prescription$IMD10 == 5] <- "3"
prescription$IMD5[prescription$IMD10 == 6] <- "3"
prescription$IMD5[prescription$IMD10 == 7] <- "4"
prescription$IMD5[prescription$IMD10 == 8] <- "4"
prescription$IMD5[prescription$IMD10 == 9] <- "5 (most deprived)"
prescription$IMD5[prescription$IMD10 == 10] <- "5 (most deprived)"

prescription$IMD5 <- factor(prescription$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(prescription)

table(prescription$IMD5)

result <- prescription[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  .(year, IMD5)
]

table(prescription$year)
summary(result)
plot(result$year, result$IRRsuccess)

prescription[, .(total_ppl = sum(ppl_N_total),
                total_py = sum(person_years_total),
                ppl_to_py_ratio = sum(ppl_N_total)/sum(person_years_total)),
            .(year)]
head(prescription)

# Add national average
national_prescription <- prescription[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = year
]

result[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

national_prescription[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

# Plot
ggplot() +
  # Horizontal reference lines
  geom_hline(
    yintercept = seq(0, 110, by = 10),
    color = "grey90",
    size = 0.3,
    linetype = "dashed"
  ) +

  # CI ribbons for IMD5
  geom_ribbon(
    data = result,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5, group = IMD5),
    alpha = 0.2,
    color = NA
  ) +

  # IMD lines and points
  geom_line(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, linetype = IMD5, group = IMD5),
    size = 1
  ) +
  geom_point(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, shape = IMD5, group = IMD5),
    size = 2
  ) +

  # National average ribbon
  geom_ribbon(
    data = national_prescription,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
    fill = "black",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +

  # National average line and point
  geom_line(
    data = national_prescription,
    aes(x = year, y = IRRsuccess, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = national_prescription,
    aes(x = year, y = IRRsuccess, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # Scales
  scale_x_continuous(
    breaks = seq(min(result$year), max(result$year), by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 30),
    labels = scales::number_format(accuracy = 1)
  ) +

  # Aesthetic legends
  scale_color_manual(name = "IMD Quintile", values = imd_cols) +
  scale_fill_manual(name = "IMD Quintile", values = imd_cols) +
  scale_linetype_manual(name = "IMD Quintile", values = line_types) +
  scale_shape_manual(name = "IMD Quintile", values = pt_shapes) +
  guides(fill = "none") +

  # Labels and theme
  labs(
    x = "Year",
    y = "Prescriptions (per 1,000 person-years)",
    title = "Smoking cessation drug prescription over time by IMD quintile"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

# # # # # # drug prescription - sensitivity

cohort <- 2018:2024

prescription_sens <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_drug_prescription_sensitivity_incidence_results_all_ages_", x, ".xlsx"), sheet = "age_gender_region_IMD10_eth5")
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

prescription_sens <- rbindlist(prescription_sens)

prescription_sens$agecat <- NA
prescription_sens$agecat[prescription_sens$age %in%  11:15] <- "11-15"
prescription_sens$agecat[prescription_sens$age %in%  16:17] <- "16-17"
prescription_sens$agecat[prescription_sens$age %in%  18:21] <- "18-21"
prescription_sens$agecat[prescription_sens$age %in%  22:24] <- "22-24"
prescription_sens$agecat[prescription_sens$age %in%  25:29] <- "25-29"
prescription_sens$agecat[prescription_sens$age %in%  30:34] <- "30-34"
prescription_sens$agecat[prescription_sens$age %in%  35:39] <- "35-39"
prescription_sens$agecat[prescription_sens$age %in%  40:44] <- "40-44"
prescription_sens$agecat[prescription_sens$age %in%  45:49] <- "45-49"
prescription_sens$agecat[prescription_sens$age %in%  50:54] <- "50-54"
prescription_sens$agecat[prescription_sens$age %in%  55:59] <- "55-59"
prescription_sens$agecat[prescription_sens$age %in%  60:64] <- "60-64"
prescription_sens$agecat[prescription_sens$age %in%  65:69] <- "65-69"
prescription_sens$agecat[prescription_sens$age %in%  70:74] <- "70-74"
prescription_sens$agecat[prescription_sens$age %in%  75:79] <- "75-79"
prescription_sens$agecat[prescription_sens$age %in%  80:84] <- "80-84"
prescription_sens$agecat[prescription_sens$age %in%  85:89] <- "85-89"
prescription_sens$agecat[prescription_sens$age >=  90] <- "90+"

prescription_sens <- prescription_sens[IMD10 != "IMD unavailable"]

prescription_sens$IMD5 <- NA
prescription_sens$IMD5[prescription_sens$IMD10 == 1] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 2] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 3] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 4] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 5] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 6] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 7] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 8] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 9] <- "5 (most deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 10] <- "5 (most deprived)"

prescription_sens$IMD5 <- factor(prescription_sens$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(prescription_sens)

table(prescription_sens$IMD5)

result <- prescription_sens[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  .(year, IMD5)
]

table(prescription_sens$year)
summary(result)
plot(result$year, result$IRRsuccess)

prescription_sens[, .(total_ppl = sum(ppl_N_total),
                 total_py = sum(person_years_total),
                 ppl_to_py_ratio = sum(ppl_N_total)/sum(person_years_total)),
             .(year)]
head(prescription_sens)

# Add national average
national_prescription_sens <- prescription_sens[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = year
]

result[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

national_prescription_sens[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

# Plot
ggplot() +
  # Horizontal reference lines
  geom_hline(
    yintercept = seq(0, 110, by = 10),
    color = "grey90",
    size = 0.3,
    linetype = "dashed"
  ) +

  # CI ribbons for IMD5
  geom_ribbon(
    data = result,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5, group = IMD5),
    alpha = 0.2,
    color = NA
  ) +

  # IMD lines and points
  geom_line(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, linetype = IMD5, group = IMD5),
    size = 1
  ) +
  geom_point(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, shape = IMD5, group = IMD5),
    size = 2
  ) +

  # National average ribbon
  geom_ribbon(
    data = national_prescription_sens,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
    fill = "black",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +

  # National average line and point
  geom_line(
    data = national_prescription_sens,
    aes(x = year, y = IRRsuccess, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = national_prescription_sens,
    aes(x = year, y = IRRsuccess, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # Scales
  scale_x_continuous(
    breaks = seq(min(result$year), max(result$year), by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 70),
    labels = scales::number_format(accuracy = 1)
  ) +

  # Aesthetic legends
  scale_color_manual(name = "IMD Quintile", values = imd_cols) +
  scale_fill_manual(name = "IMD Quintile", values = imd_cols) +
  scale_linetype_manual(name = "IMD Quintile", values = line_types) +
  scale_shape_manual(name = "IMD Quintile", values = pt_shapes) +
  guides(fill = "none") +

  # Labels and theme
  labs(
    x = "Year",
    y = "Prescriptions (per 1,000 person-years)",
    title = "Figure 2. Smoking cessation drug prescriptions over time by IMD quintile"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

national_prescription_sens

# # # # # # drug prescription (sensitivity) - ethnicity - all

# cohorts
cohort <- 2018:2024

# read & stack all years
prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist()

prescription_sens$agecat <- NA
prescription_sens$agecat[prescription_sens$age %in%  11:15] <- "11-15"
prescription_sens$agecat[prescription_sens$age %in%  16:17] <- "16-17"
prescription_sens$agecat[prescription_sens$age %in%  18:21] <- "18-21"
prescription_sens$agecat[prescription_sens$age %in%  22:24] <- "22-24"
prescription_sens$agecat[prescription_sens$age %in%  25:29] <- "25-29"
prescription_sens$agecat[prescription_sens$age %in%  30:34] <- "30-34"
prescription_sens$agecat[prescription_sens$age %in%  35:39] <- "35-39"
prescription_sens$agecat[prescription_sens$age %in%  40:44] <- "40-44"
prescription_sens$agecat[prescription_sens$age %in%  45:49] <- "45-49"
prescription_sens$agecat[prescription_sens$age %in%  50:54] <- "50-54"
prescription_sens$agecat[prescription_sens$age %in%  55:59] <- "55-59"
prescription_sens$agecat[prescription_sens$age %in%  60:64] <- "60-64"
prescription_sens$agecat[prescription_sens$age %in%  65:69] <- "65-69"
prescription_sens$agecat[prescription_sens$age %in%  70:74] <- "70-74"
prescription_sens$agecat[prescription_sens$age %in%  75:79] <- "75-79"
prescription_sens$agecat[prescription_sens$age %in%  80:84] <- "80-84"
prescription_sens$agecat[prescription_sens$age %in%  85:89] <- "85-89"
prescription_sens$agecat[prescription_sens$age >=  90] <- "90+"

prescription_sens <- prescription_sens[IMD10 != "IMD unavailable"]

prescription_sens$IMD5 <- NA
prescription_sens$IMD5[prescription_sens$IMD10 == 1] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 2] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 3] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 4] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 5] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 6] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 7] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 8] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 9] <- "5 (most deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 10] <- "5 (most deprived)"

prescription_sens$IMD5 <- factor(prescription_sens$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

# recode eth5 as factor with your desired levels, and drop missing
# 2) Factor with your six levels
prescription_sens[, eth5 := factor(
  eth5,
  levels = c("White", "Black", "Asian", "Mixed", "Other", "Unknown / Not Stated")
)]
# helper CI functions (unchanged)
get_ci_lo <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  if (n == 0) return(0)
  qchisq(alpha/2, df = 2 * n) / (2 * py_total)
}
get_ci_hi <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  qchisq(1 - alpha/2, df = 2 * (n + 1)) / (2 * py_total)
}

# aggregate by year & ethnicity
result_eth <- prescription_sens[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRR     = sum(n_total_incidence) / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = .(year, eth5)
]

# national average (all ethnicities pooled)
national_eth <- prescription_sens[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRR     = sum(n_total_incidence) / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = year
]

for (dt in list(result_eth, national_eth)) {
  dt[, `:=`(
    IRR    = IRR * 1000,
    IRR_lo = IRR_lo * 1000,
    IRR_hi = IRR_hi * 1000
  )]
}

# define a palette & aesthetics for eth5 + national
eth_cols <- c(
  "White"                = "#1b9e77",
  "Black"                = "#d95f02",
  "Asian"                = "#7570b3",
  "Mixed"                = "#e7298a",
  "Other"                = "#66a61e",
  "Unknown / Not Stated" = "#999999"
)
# six solid lines for each ethnicity, plus one longdash for national
line_types <- c(
  setNames(rep("solid", 6), levels(prescription_sens$eth5)),
  "National average" = "longdash"
)

# six point shapes, plus one for national
pt_shapes <- c(
  setNames(c(16, 17, 15, 18, 19, 4), levels(prescription_sens$eth5)),
  "National average" = 3
)

# plot
p <- ggplot() +
  # light grid
  geom_hline(yintercept = seq(0, 110, by = 10), color = "grey90", linetype = "dashed") +

  # eth5 ribbons & lines
  geom_ribbon(
    data = result_eth,
    aes(x = year, ymin = IRR_lo, ymax = IRR_hi, fill = eth5, group = eth5),
    alpha = 0.2
  ) +
  geom_line(
    data = result_eth,
    aes(x = year, y = IRR, color = eth5, linetype = eth5, group = eth5),
    size = 1
  ) +
  geom_point(
    data = result_eth,
    aes(x = year, y = IRR, color = eth5, shape = eth5, group = eth5),
    size = 2
  ) +

  # national ribbon, line, point
  geom_ribbon(
    data = national_eth,
    aes(x = year, ymin = IRR_lo, ymax = IRR_hi),
    fill = "black", alpha = 0.1
  ) +
  geom_line(
    data = national_eth,
    aes(x = year, y = IRR, color = "National average", linetype = "National average"),
    size = 1.2
  ) +
  geom_point(
    data = national_eth,
    aes(x = year, y = IRR, color = "National average", shape = "National average"),
    size = 3
  ) +

  # scales
  scale_x_continuous(breaks = seq(min(result_eth$year), max(result_eth$year), 1)) +
  scale_y_continuous(limits = c(0, 75), labels = number_format(1)) +
  scale_color_manual(
    name = "Ethnicity",
    values = c(eth_cols, "National average" = "black")
  ) +
  scale_fill_manual( name = "Ethnicity", values = eth_cols) +
  scale_linetype_manual(
    name = "Ethnicity",
    values = c(setNames(rep("solid", 6), names(eth_cols)), "National average" = "longdash")
  ) +
  scale_shape_manual(name = "Ethnicity", values = pt_shapes) +
  guides(fill = "none") +

  # labels + theme
  labs(
    x     = "Year",
    y     = "Prescriptions per 1,000 person-years",
    title = "Smoking cessation drug prescriptions over time by Ethnicity"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)

# ethnicity - restricted

# # # # # # drug prescription (sensitivity) - ethnicity - all

# cohorts
cohort <- 2018:2024

# read & stack all years
prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/nicotine_smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist()

prescription_sens$agecat <- NA
prescription_sens$agecat[prescription_sens$age %in%  11:15] <- "11-15"
prescription_sens$agecat[prescription_sens$age %in%  16:17] <- "16-17"
prescription_sens$agecat[prescription_sens$age %in%  18:21] <- "18-21"
prescription_sens$agecat[prescription_sens$age %in%  22:24] <- "22-24"
prescription_sens$agecat[prescription_sens$age %in%  25:29] <- "25-29"
prescription_sens$agecat[prescription_sens$age %in%  30:34] <- "30-34"
prescription_sens$agecat[prescription_sens$age %in%  35:39] <- "35-39"
prescription_sens$agecat[prescription_sens$age %in%  40:44] <- "40-44"
prescription_sens$agecat[prescription_sens$age %in%  45:49] <- "45-49"
prescription_sens$agecat[prescription_sens$age %in%  50:54] <- "50-54"
prescription_sens$agecat[prescription_sens$age %in%  55:59] <- "55-59"
prescription_sens$agecat[prescription_sens$age %in%  60:64] <- "60-64"
prescription_sens$agecat[prescription_sens$age %in%  65:69] <- "65-69"
prescription_sens$agecat[prescription_sens$age %in%  70:74] <- "70-74"
prescription_sens$agecat[prescription_sens$age %in%  75:79] <- "75-79"
prescription_sens$agecat[prescription_sens$age %in%  80:84] <- "80-84"
prescription_sens$agecat[prescription_sens$age %in%  85:89] <- "85-89"
prescription_sens$agecat[prescription_sens$age >=  90] <- "90+"

prescription_sens <- prescription_sens[IMD10 != "IMD unavailable"]

prescription_sens$IMD5 <- NA
prescription_sens$IMD5[prescription_sens$IMD10 == 1] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 2] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 3] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 4] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 5] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 6] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 7] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 8] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 9] <- "5 (most deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 10] <- "5 (most deprived)"

prescription_sens$IMD5 <- factor(prescription_sens$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

# prescription_sens <- prescription_sens[year %in% 2018:2019]
prescription_sens <- prescription_sens[age < 65]

# prescription_sens <- prescription_sens[age %in% 65:74]

# recode eth5 as factor with your desired levels, and drop missing
# 2) Factor with your six levels
prescription_sens[, eth5 := factor(
  eth5,
  levels = c("White", "Black", "Asian", "Mixed", "Other", "Unknown / Not Stated")
)]

prescription_sens <- prescription_sens[eth5 != "Unknown / Not Stated"]

# helper CI functions (unchanged)
get_ci_lo <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  if (n == 0) return(0)
  qchisq(alpha/2, df = 2 * n) / (2 * py_total)
}
get_ci_hi <- function(n, py_total, conf.level = 0.95) {
  alpha <- 1 - conf.level
  qchisq(1 - alpha/2, df = 2 * (n + 1)) / (2 * py_total)
}

# aggregate by year & ethnicity
result_eth <- prescription_sens[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRR     = sum(n_total_incidence) / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total)),
    total_people = sum(ppl_N_total)
  ),
  by = .(agecat, eth5)
]

# national average (all ethnicities pooled)
national_eth <- prescription_sens[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRR     = sum(n_total_incidence) / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = agecat
]

for (dt in list(result_eth, national_eth)) {
  dt[, `:=`(
    IRR    = IRR * 1000,
    IRR_lo = IRR_lo * 1000,
    IRR_hi = IRR_hi * 1000
  )]
}

# define a palette & aesthetics for eth5 + national
eth_cols <- c(
  "White"                = "#1b9e77",
  "Black"                = "#d95f02",
  "Asian"                = "#7570b3",
  "Mixed"                = "#e7298a",
  "Other"                = "#66a61e",
  "Unknown / Not Stated" = "#999999"
)
# six solid lines for each ethnicity, plus one longdash for national
line_types <- c(
  setNames(rep("solid", 6), levels(prescription_sens$eth5)),
  "National average" = "longdash"
)

# six point shapes, plus one for national
pt_shapes <- c(
  setNames(c(16, 17, 15, 18, 19, 4), levels(prescription_sens$eth5)),
  "National average" = 3
)

# plot
p <- ggplot() +
  # light grid
  geom_hline(yintercept = seq(0, 110, by = 10), color = "grey90", linetype = "dashed") +

#  eth5 ribbons & lines
  geom_ribbon(
    data = result_eth,
    aes(x = agecat, ymin = IRR_lo, ymax = IRR_hi, fill = eth5, group = eth5),
    alpha = 0.2
  ) +
  geom_line(
    data = result_eth,
    aes(x = agecat, y = IRR, color = eth5, linetype = eth5, group = eth5),
    size = 1
  ) +
  geom_point(
    data = result_eth,
    aes(x = agecat, y = IRR, color = eth5, shape = eth5, group = eth5),
    size = 2
  ) +

  # # national ribbon, line, point
  # geom_ribbon(
  #   data = national_eth,
  #   aes(x = agecat, ymin = IRR_lo, ymax = IRR_hi),
  #   fill = "black", alpha = 0.1
  # ) +
  # geom_line(
  #   data = national_eth,
  #   aes(x = agecat, y = IRR, color = "National average", linetype = "National average"),
  #   size = 1.2
  # ) +
  # geom_point(
  #   data = national_eth,
  #   aes(x = agecat, y = IRR, color = "National average", shape = "National average"),
  #   size = 3
  # ) +

  # scales
#  scale_x_continuous(breaks = seq(min(result_eth$agecat), max(result_eth$agecat), 1)) +
  scale_y_continuous(limits = c(0, 60), labels = number_format(1)) +
  scale_color_manual(
    name = "Ethnicity",
#    values = c(eth_cols, "National average" = "black")
   values = c(eth_cols)
  ) +
  scale_fill_manual( name = "Ethnicity", values = eth_cols) +
  scale_linetype_manual(
    name = "Ethnicity",
    values = c(setNames(rep("solid", 6), names(eth_cols))) #, "National average" = "longdash")
  ) +
  scale_shape_manual(name = "Ethnicity", values = pt_shapes) +
  guides(fill = "none") +

  # labels + theme
  labs(
    x     = "Age category",
    y     = "Prescriptions per 1,000 person-years",
    title = "Figure 4. Nicotine replacement therapy prescription by age group and ethnicity"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0)
  )

print(p)

# # save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)

# drugs prescription vs region

# cohorts
cohort <- 2018:2024

# read & stack all years
prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist()

prescription_sens[, .(totals = sum(ppl_N_total)), year]
prescription_sens[, .(totals = sum(ppl_N_total)), .(region, year)]

prescription_sens$agecat <- NA
prescription_sens$agecat[prescription_sens$age %in%  11:15] <- "11-15"
prescription_sens$agecat[prescription_sens$age %in%  16:17] <- "16-17"
prescription_sens$agecat[prescription_sens$age %in%  18:21] <- "18-21"
prescription_sens$agecat[prescription_sens$age %in%  22:24] <- "22-24"
prescription_sens$agecat[prescription_sens$age %in%  25:29] <- "25-29"
prescription_sens$agecat[prescription_sens$age %in%  30:34] <- "30-34"
prescription_sens$agecat[prescription_sens$age %in%  35:39] <- "35-39"
prescription_sens$agecat[prescription_sens$age %in%  40:44] <- "40-44"
prescription_sens$agecat[prescription_sens$age %in%  45:49] <- "45-49"
prescription_sens$agecat[prescription_sens$age %in%  50:54] <- "50-54"
prescription_sens$agecat[prescription_sens$age %in%  55:59] <- "55-59"
prescription_sens$agecat[prescription_sens$age %in%  60:64] <- "60-64"
prescription_sens$agecat[prescription_sens$age %in%  65:69] <- "65-69"
prescription_sens$agecat[prescription_sens$age %in%  70:74] <- "70-74"
prescription_sens$agecat[prescription_sens$age %in%  75:79] <- "75-79"
prescription_sens$agecat[prescription_sens$age %in%  80:84] <- "80-84"
prescription_sens$agecat[prescription_sens$age %in%  85:89] <- "85-89"
prescription_sens$agecat[prescription_sens$age >=  90] <- "90+"

prescription_sens <- prescription_sens[IMD10 != "IMD unavailable"]

prescription_sens$IMD5 <- NA
prescription_sens$IMD5[prescription_sens$IMD10 == 1] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 2] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 3] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 4] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 5] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 6] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 7] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 8] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 9] <- "5 (most deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 10] <- "5 (most deprived)"

prescription_sens$IMD5 <- factor(prescription_sens$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

# drug prescription - region
prescription_sens[, region := factor(
  region,
  levels = c(
    "North East",
    "North West",
    "Yorkshire and The Humber",
    "East Midlands",
    "West Midlands",
    "East of England",
    "London",
    "South East",
    "South West"
  )
)]

# 5) Aggregate by year & region, excluding ages 11–17
result_reg <- prescription_sens[
 # !age %in% 11:17 & !is.na(IMD5),
   !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR     = sum(n_total_incidence)    / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total)),
    ppl_denom = sum(ppl_N_total)
  ),
  by = .(year, region)
]

# 6) National average
national_reg <- prescription_sens[
  !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR     = sum(n_total_incidence)    / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = year
]

for (dt in list(result_reg, national_reg)) {
  dt[, `:=`(
    IRR    = IRR * 1000,
    IRR_lo = IRR_lo * 1000,
    IRR_hi = IRR_hi * 1000
  )]
}

# 8) Define a colour palette and aesthetics
region_cols <- RColorBrewer::brewer.pal(9, "Set1")
names(region_cols) <- levels(prescription_sens$region)

line_types <- c(setNames(rep("solid", 9), levels(prescription_sens$region)),
                "National average" = "longdash")

pt_shapes <- c(setNames(c(21:25, 0:3), levels(prescription_sens$region)),
               "National average" = 8)

# 9) Plot
p <- ggplot() +
  geom_hline(yintercept = seq(0, 80, by = 10),
             color = "grey90", linetype = "dashed") +

  # ribbons & lines for each region
  geom_ribbon(data = result_reg,
              aes(x = year, ymin = IRR_lo, ymax = IRR_hi,
                  fill = region, group = region),
              alpha = 0.2) +
  geom_line(data = result_reg,
            aes(x = year, y = IRR,
                color = region, linetype = region, group = region),
            size = 1) +
  geom_point(data = result_reg,
             aes(x = year, y = IRR,
                 color = region, shape = region, group = region),
             size = 2) +

  # national average
  geom_ribbon(data = national_reg,
              aes(x = year, ymin = IRR_lo, ymax = IRR_hi),
              fill = "black", alpha = 0.1) +
  geom_line(data = national_reg,
            aes(x = year, y = IRR,
                color = "National average",
                linetype = "National average"),
            size = 1.2) +
  geom_point(data = national_reg,
             aes(x = year, y = IRR,
                 color = "National average",
                 shape = "National average"),
             size = 3) +

  scale_x_continuous(breaks = seq(min(result_reg$year), max(result_reg$year), 1)) +
  scale_y_continuous(limits = c(0, 80), breaks = c(0, 20, 40, 60, 80), labels = number_format(1)) +

  # scale_color_manual(
  #   name   = "Region",
  #   values = c(region_cols, "National average" = "black")
  # ) +
  # scale_fill_manual(name = "Region", values = region_cols) +
  # scale_linetype_manual(
  #   name   = "Region",
  #   values = line_types
  # ) +
  # scale_shape_manual(
  #   name   = "Region",
  #   values = pt_shapes,
  #   guide  = FALSE           # remove extra shapes‐only legend
  # ) +

   scale_color_manual(
    name   = "Region",
    values = c(region_cols, "National average" = "black"),
    guide  = guide_legend(
      override.aes = list(
        shape = c(pt_shapes[levels(prescription_sens$region)],
                  pt_shapes["National average"]),
        linetype = c(rep("solid", length(region_cols)), "longdash")
      )
    )
  ) +
  scale_shape_manual(
    name   = "Region",
    values = pt_shapes,
    guide  = FALSE     # turn off the separate shape legend
  ) +
  scale_linetype_manual(
    name   = "Region",
    values = line_types,
    guide  = FALSE     # turn off the separate linetype legend
  ) +
  scale_fill_manual(name = "Region", values = region_cols, guide = FALSE) +
  guides(fill = "none") +

  labs(
    x     = "Year",
    y     = "Prescriptions per 1,000 person-years",
    title = "Smoking cessation drug prescriptions over time by Region"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# 10) Save
ggsave(
  "output/plots/",
  plot = p, width = 3000, height = 1500, units = "px"
)

# prescriptions by each age #############################

# cohorts
cohort <- 2018:2024

# read & stack all years
prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist()

prescription_sens$agecat <- NA
prescription_sens$agecat[prescription_sens$age %in%  11:15] <- "11-15"
prescription_sens$agecat[prescription_sens$age %in%  16:17] <- "16-17"
prescription_sens$agecat[prescription_sens$age %in%  18:21] <- "18-21"
prescription_sens$agecat[prescription_sens$age %in%  22:24] <- "22-24"
prescription_sens$agecat[prescription_sens$age %in%  25:29] <- "25-29"
prescription_sens$agecat[prescription_sens$age %in%  30:34] <- "30-34"
prescription_sens$agecat[prescription_sens$age %in%  35:39] <- "35-39"
prescription_sens$agecat[prescription_sens$age %in%  40:44] <- "40-44"
prescription_sens$agecat[prescription_sens$age %in%  45:49] <- "45-49"
prescription_sens$agecat[prescription_sens$age %in%  50:54] <- "50-54"
prescription_sens$agecat[prescription_sens$age %in%  55:59] <- "55-59"
prescription_sens$agecat[prescription_sens$age %in%  60:64] <- "60-64"
prescription_sens$agecat[prescription_sens$age %in%  65:69] <- "65-69"
prescription_sens$agecat[prescription_sens$age %in%  70:74] <- "70-74"
prescription_sens$agecat[prescription_sens$age %in%  75:79] <- "75-79"
prescription_sens$agecat[prescription_sens$age %in%  80:84] <- "80-84"
prescription_sens$agecat[prescription_sens$age %in%  85:89] <- "85-89"
prescription_sens$agecat[prescription_sens$age >=  90] <- "90+"

prescription_sens <- prescription_sens[IMD10 != "IMD unavailable"]

prescription_sens$IMD5 <- NA
prescription_sens$IMD5[prescription_sens$IMD10 == 1] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 2] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 3] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 4] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 5] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 6] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 7] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 8] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 9] <- "5 (most deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 10] <- "5 (most deprived)"

prescription_sens$IMD5 <- factor(prescription_sens$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

# 5) Aggregate by year & region, excluding ages 11–17
presc_result_age <- prescription_sens[
  # !age %in% 11:17 & !is.na(IMD5),
  !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR     = sum(n_total_incidence)    / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total)),
    ppl_denom = sum(ppl_N_total),
    denom_time = sum(person_years_total)
  ),
  by = .(year, agecat)
]

# 6) National average
national_presc_age <- prescription_sens[
  !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR     = sum(n_total_incidence)    / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = year
]

library(data.table)
library(ggplot2)
library(scales)
library(RColorBrewer)

# --- assume prescription_sens already loaded and agecat created ---
# 0) ensure agecat is a factor in your desired order:

age_levels <- c(
  "18-21","22-24","25-29","30-34","35-39","40-44",
  "45-49","50-54","55-59","60-64","65-69","70-74","75-79","80-84",
  "85-89","90+"
)

presc_result_age[, agecat := factor(agecat, levels = age_levels)]

# 3.2) Capture only the active agecat levels
active_ages <- levels(presc_result_age$agecat)
# (this should be length 16: "18-21", "22-24", …, "90+")

# 3.3) Rebuild palettes & aesthetics just for those
library(RColorBrewer)
age_cols   <- colorRampPalette(brewer.pal(9, "Set1"))(length(active_ages))
names(age_cols) <- active_ages

line_types <- c(
  setNames(rep("solid", length(active_ages)), active_ages),
  "National average" = "longdash"
)

shape_seq  <- c(0:25)[1:length(active_ages)]  # pick up to 25 valid pch’s
pt_shapes  <- c(
  setNames(shape_seq, active_ages),
  "National average" = 20
)

# 4) Now re‐plot using those:
p_age <- ggplot() +
  geom_hline(yintercept = seq(0, 80, by = 20),
             color = "grey90", linetype = "dashed") +
  # ribbons & lines
  geom_ribbon(data = presc_result_age,
              aes(year, ymin = IRR_lo, ymax = IRR_hi,
                  fill = agecat, group = agecat),
              alpha = 0.2) +
  geom_line(data = presc_result_age,
            aes(year, IRR,
                color = agecat, linetype = agecat, group = agecat),
            size = 1) +
  geom_point(data = presc_result_age,
             aes(year, IRR,
                 color = agecat, shape = agecat, group = agecat),
             size = 2) +
  # national average
  geom_ribbon(data = national_presc_age,
              aes(year, ymin = IRR_lo, ymax = IRR_hi),
              fill = "black", alpha = 0.1) +
  geom_line(data = national_presc_age,
            aes(year, IRR,
                color = "National average", linetype = "National average"),
            size = 1.2) +
  geom_point(data = national_presc_age,
             aes(year, IRR,
                 color = "National average", shape = "National average"),
             size = 3) +

  # axes
  scale_x_continuous(breaks = seq(min(presc_result_age$year), max(presc_result_age$year), 1)) +
  scale_y_continuous(
    limits = c(0, 80),
    breaks = seq(0, 80, by = 20),
    labels = scales::number_format(1)
  ) +

  # colour + shape share one legend “Age Category”
  scale_color_manual(
    name   = "Age Category",
    values = c(age_cols, "National average" = "black")
  ) +
  scale_shape_manual(
    name   = "Age Category",
    values = pt_shapes
  ) +

  # hide the linetype & fill legends
  scale_linetype_manual(
    name  = "Age Category",
    values = line_types,
    guide  = FALSE
  ) +
  scale_fill_manual(
    name  = "Age Category",
    values = age_cols,
    guide = FALSE
  ) +

  # no other guides
  guides(
    fill     = "none",
    linetype = "none"
  ) +

  labs(
    x = "Year",
    y = "Prescriptions per 1,000 person-years",
    title = "Smoking cessation drug prescriptions over time by Age Category"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(angle = 45, hjust = 1),
    plot.title         = element_text(hjust = 0.5)
  )

p_age

library(ggplot2)
library(scales)

# ggplot(presc_result_age, aes(x = agecat, y = IRR, group = factor(year), color = factor(year))) +
#   # CI ribbons (optional)
#   geom_ribbon(aes(ymin = IRR_lo, ymax = IRR_hi, fill = factor(year)),
#               alpha = 0.1, colour = NA) +
#   # line + points
#   geom_line(size = 1) +
#   geom_point(size = 2) +
#   scale_y_continuous(labels = number_format(1)) +
#   labs(
#     x     = "Age Category",

#     color = "Year",
#     fill  = "Year",
#     title = "Prescription incidence by Age Category over Time"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )

library(dplyr)
library(ggplot2)
library(scales)

library(data.table)
library(dplyr)
library(ggplot2)
library(scales)

# 1) National average by agecat (across all years)
national_agecat <- prescription_sens[
  !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR     = sum(n_total_incidence)    / sum(person_years_total),
    IRR_lo  = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi  = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = agecat
]

national_agecat[, `:=`(
  IRR    = IRR * 1000,
  IRR_lo = IRR_lo * 1000,
  IRR_hi = IRR_hi * 1000
)]

presc_result_age[, `:=`(
  IRR    = IRR * 1000,
  IRR_lo = IRR_lo * 1000,
  IRR_hi = IRR_hi * 1000
)]

# make sure agecat is a factor in the same order
national_agecat[, agecat := factor(agecat, levels = levels(presc_result_age$agecat))]

library(ggplot2)
library(scales)
library(RColorBrewer)

# 1) Prepare the legend levels (your years plus the 2018-2024 combined)
years       <- sort(unique(presc_result_age$year))
year_labels <- as.character(years)
legend_lvls <- c(year_labels, "2018-2024 combined")

year_cols <- setNames(
  colorRampPalette(c("#deebf7", "#08519c"))(length(years)),
  year_labels
)

# 3) Now the plot
age_prescription_plot <- ggplot() +
  # per‐year CI ribbons
  geom_ribbon(
    data = presc_result_age,
    aes(
      x     = agecat,
      ymin  = IRR_lo,
      ymax  = IRR_hi,
      fill  = factor(year),
      group = factor(year)
    ),
    alpha = 0.1, colour = NA
  ) +
  # national CI ribbon
  geom_ribbon(
    data = national_agecat,
    aes(
      x     = agecat,
      ymin  = IRR_lo,
      ymax  = IRR_hi,
      fill  = "2018-2024 combined",
      group = "2018-2024 combined"
    ),
    alpha = 0.1, colour = NA
  ) +

  # per‐year lines
  geom_line(
    data = presc_result_age,
    aes(
      x        = agecat,
      y        = IRR,
      color    = factor(year),
      linetype = factor(year),
      group    = factor(year)
    ),
    size = 1
  ) +
  # national line
  geom_line(
    data = national_agecat,
    aes(
      x        = agecat,
      y        = IRR,
      color    = "2018-2024 combined",
      linetype = "2018-2024 combined",
      group    = "2018-2024 combined"
    ),
    size = 1
  ) +

  # per‐year points (all circles)
  geom_point(
    data = presc_result_age,
    aes(
      x     = agecat,
      y     = IRR,
      color = factor(year),
      shape = factor(year),
      group = factor(year)
    ),
    size = 2
  ) +
  # national points (also circles)
  geom_point(
    data = national_agecat,
    aes(
      x     = agecat,
      y     = IRR,
      color = "2018-2024 combined",
      shape = "2018-2024 combined",
      group = "2018-2024 combined"
    ),
    size = 2
  ) +

  # scales: colour, fill, linetype, shape all share the same legend "Year"
  scale_color_manual(
    name   = "Year",
    values = c(year_cols, "2018-2024 combined" = "black"),
    breaks = legend_lvls
  ) +
  scale_fill_manual(
    name   = "Year",
    values = c(year_cols, "2018-2024 combined" = "black"),
    breaks = legend_lvls,
    guide  = FALSE   # we don't need a separate fill legend
  ) +
  scale_linetype_manual(
    name   = "Year",
    values = c(
      setNames(rep("solid", length(years)), year_labels),
      "2018-2024 combined" = "longdash"
    ),
    breaks = legend_lvls
  ) +
  scale_shape_manual(
    name   = "Year",
    values = c(
      setNames(rep(16, length(years)), year_labels),
      "2018-2024 combined" = 16
    ),
    breaks = legend_lvls
  ) +

  # axes + labels
  scale_y_continuous(labels = number_format(1)) +
  labs(
    x     = "Age Category",
    y     = "Drug prescriptions per 1,000 person-years",
    title = "Figure 3. Smoking cessation drug prescriptions incidence by age category
in the years 2018-2024"
  ) +

  # theme
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

# 6) Save
ggsave(
  "output/plots/",
  plot  = age_prescription_plot,
  width = 3000,
  height = 1500,
  units = "px"
)

# now we read in all the individual prescription drugs and plot them.

# cohorts
cohort <- 2018:2024

# read & stack all years

# bupropion
bupropion_prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/bupropion_smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist() %>%
  mutate(smoking_cessation_drug = "Buproprion")

# varenicline
varenicline_prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/varenecline_smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist() %>%
  mutate(smoking_cessation_drug = "Varenicline")

# nicotine
nicotine_prescription_sens <- lapply(cohort, function(x) {
  df <- read_xlsx(
    paste0(
      "output/incidences/",
      x,
      "/nicotine_smoking_drug_prescription_sensitivity_incidence_results_all_ages_",
      x, ".xlsx"
    ),
    sheet = "age_gender_region_IMD10_eth5"
  )
  df$year <- x
  return(df)
}) %>%
  rbindlist() %>%
  mutate(smoking_cessation_drug = "Nicotine")

prescription_sens <- rbind(bupropion_prescription_sens, varenicline_prescription_sens,
                           nicotine_prescription_sens)

prescription_sens[, .(totals = sum(ppl_N_total)), .(smoking_cessation_drug, year)]

prescription_sens$agecat <- NA
prescription_sens$agecat[prescription_sens$age %in%  11:15] <- "11-15"
prescription_sens$agecat[prescription_sens$age %in%  16:17] <- "16-17"
prescription_sens$agecat[prescription_sens$age %in%  18:21] <- "18-21"
prescription_sens$agecat[prescription_sens$age %in%  22:24] <- "22-24"
prescription_sens$agecat[prescription_sens$age %in%  25:29] <- "25-29"
prescription_sens$agecat[prescription_sens$age %in%  30:34] <- "30-34"
prescription_sens$agecat[prescription_sens$age %in%  35:39] <- "35-39"
prescription_sens$agecat[prescription_sens$age %in%  40:44] <- "40-44"
prescription_sens$agecat[prescription_sens$age %in%  45:49] <- "45-49"
prescription_sens$agecat[prescription_sens$age %in%  50:54] <- "50-54"
prescription_sens$agecat[prescription_sens$age %in%  55:59] <- "55-59"
prescription_sens$agecat[prescription_sens$age %in%  60:64] <- "60-64"
prescription_sens$agecat[prescription_sens$age %in%  65:69] <- "65-69"
prescription_sens$agecat[prescription_sens$age %in%  70:74] <- "70-74"
prescription_sens$agecat[prescription_sens$age %in%  75:79] <- "75-79"
prescription_sens$agecat[prescription_sens$age %in%  80:84] <- "80-84"
prescription_sens$agecat[prescription_sens$age %in%  85:89] <- "85-89"
prescription_sens$agecat[prescription_sens$age >=  90] <- "90+"

prescription_sens <- prescription_sens[IMD10 != "IMD unavailable"]

prescription_sens$IMD5 <- NA
prescription_sens$IMD5[prescription_sens$IMD10 == 1] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 2] <- "1 (least deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 3] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 4] <- "2"
prescription_sens$IMD5[prescription_sens$IMD10 == 5] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 6] <- "3"
prescription_sens$IMD5[prescription_sens$IMD10 == 7] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 8] <- "4"
prescription_sens$IMD5[prescription_sens$IMD10 == 9] <- "5 (most deprived)"
prescription_sens$IMD5[prescription_sens$IMD10 == 10] <- "5 (most deprived)"

prescription_sens$IMD5 <- factor(prescription_sens$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

# 0) Factor your drug variable
prescription_sens[, smoking_cessation_drug := factor(
  smoking_cessation_drug,
  levels = c("Nicotine", "Buproprion", "Varenicline")
)]

# 1) Aggregate by year & drug, excluding ages 11–17 & missing IMD5
result_drug <- prescription_sens[
  !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR       = sum(n_total_incidence)    / sum(person_years_total),
    IRR_lo    = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRR_hi    = get_ci_hi(sum(n_total_incidence), sum(person_years_total)),
    ppl_denom = sum(ppl_N_total)
  ),
  by = .(year, smoking_cessation_drug)
]

# 2) All drugs combined (all drugs pooled)
national_drug <- prescription_sens[
  !age %in% 11:17 & !is.na(IMD5),
  .(
    IRR    = sum(n_total_incidence)    / (sum(person_years_total)/3),
    IRR_lo = get_ci_lo(sum(n_total_incidence), (sum(person_years_total)/3)),
    IRR_hi = get_ci_hi(sum(n_total_incidence), (sum(person_years_total)/3))
  ),
  by = year
]

for (dt in list(result_drug, national_drug)) {
  dt[, `:=`(
    IRR    = IRR * 1000,
    IRR_lo = IRR_lo * 1000,
    IRR_hi = IRR_hi * 1000
  )]
}

# 4) Define a 3-colour palette for the drugs
drug_cols <- brewer.pal(3, "Set1")
names(drug_cols) <- levels(prescription_sens$smoking_cessation_drug)

# solid lines for the three drugs, dashed for national
line_types <- c(
  setNames(rep("solid", 3), levels(prescription_sens$smoking_cessation_drug)),
  "All drugs combined" = "longdash"
)

# distinct point shapes for drugs, plus one for national
pt_shapes <- c(
  setNames(c(16, 16, 16), levels(prescription_sens$smoking_cessation_drug)),
  "All drugs combined" = 18
)

# 5) Plot
p <- ggplot() +

  # horizontal grid every 10
  geom_hline(yintercept = seq(0, 80, by = 10),
             color = "grey90", linetype = "dashed") +

  # ribbons for each drug
  geom_ribbon(data = result_drug,
              aes(x = year, ymin = IRR_lo, ymax = IRR_hi,
                  fill = smoking_cessation_drug, group = smoking_cessation_drug),
              alpha = 0.2) +

  # drug-specific lines & points
  geom_line(data = result_drug,
            aes(x = year, y = IRR,
                color = smoking_cessation_drug,
                linetype = smoking_cessation_drug,
                group = smoking_cessation_drug),
            size = 1) +
  geom_point(data = result_drug,
             aes(x = year, y = IRR,
                 color = smoking_cessation_drug,
                 shape = smoking_cessation_drug,
                 group = smoking_cessation_drug),
             size = 2) +

  # All drugs combined ribbon, line, point
  geom_ribbon(data = national_drug,
              aes(x = year, ymin = IRR_lo, ymax = IRR_hi),
              fill = "black", alpha = 0.1) +
  geom_line(data = national_drug,
            aes(x = year, y = IRR,
                color = "All drugs combined",
                linetype = "All drugs combined"),
            size = 1.2) +
  geom_point(data = national_drug,
             aes(x = year, y = IRR,
                 color = "All drugs combined",
                 shape = "All drugs combined"),
             size = 3) +

  # axes
  scale_x_continuous(breaks = seq(min(result_drug$year),
                                  max(result_drug$year), 1)) +
  scale_y_continuous(limits = c(0, 80),
                     breaks = seq(0, 80, by = 10),
                     labels = number_format(1)) +

  # combined legend under "Drug"
  scale_color_manual(
    name   = "Drug",
    values = c(drug_cols, "All drugs combined" = "black")
  ) +
  scale_fill_manual(
    name   = "Drug",
    values = drug_cols,
    guide  = FALSE
  ) +
  scale_linetype_manual(
    name   = "Drug",
    values = line_types,
    guide  = FALSE
  ) +
  scale_shape_manual(
    name   = "Drug",
    values = pt_shapes
  ) +

  labs(
    x     = "Year",
    y     = "Prescriptions per 1,000 person-years",
    title = "Figure 1. Smoking cessation drug prescriptions over time"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    legend.position     = "right",
    plot.title          = element_text(hjust = 0)
  )

print(p)

# 10) Save
ggsave(
  "output/plots/",
  plot = p, width = 3000, height = 1500, units = "px"
)

# smoking referrals vs IMD

cohort <- 2018:2024

referral <- lapply(cohort, function(x) {
  steps <- read_xlsx(paste0("output/incidences/", x,
                            "/smoking_referral_incidence_results_all_ages_", x, ".xlsx"), sheet = "age_gender_region_IMD10_eth5")
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

referral <- rbindlist(referral)

referral$agecat <- NA
referral$agecat[referral$age %in%  11:15] <- "11-15"
referral$agecat[referral$age %in%  16:17] <- "16-17"
referral$agecat[referral$age %in%  18:21] <- "18-21"
referral$agecat[referral$age %in%  22:24] <- "22-24"
referral$agecat[referral$age %in%  25:29] <- "25-29"
referral$agecat[referral$age %in%  30:34] <- "30-34"
referral$agecat[referral$age %in%  35:39] <- "35-39"
referral$agecat[referral$age %in%  40:44] <- "40-44"
referral$agecat[referral$age %in%  45:49] <- "45-49"
referral$agecat[referral$age %in%  50:54] <- "50-54"
referral$agecat[referral$age %in%  55:59] <- "55-59"
referral$agecat[referral$age %in%  60:64] <- "60-64"
referral$agecat[referral$age %in%  65:69] <- "65-69"
referral$agecat[referral$age %in%  70:74] <- "70-74"
referral$agecat[referral$age %in%  75:79] <- "75-79"
referral$agecat[referral$age %in%  80:84] <- "80-84"
referral$agecat[referral$age %in%  85:89] <- "85-89"
referral$agecat[referral$age >=  90] <- "90+"

referral <- referral[IMD10 != "IMD unavailable"]

referral$IMD5 <- NA
referral$IMD5[referral$IMD10 == 1] <- "1 (least deprived)"
referral$IMD5[referral$IMD10 == 2] <- "1 (least deprived)"
referral$IMD5[referral$IMD10 == 3] <- "2"
referral$IMD5[referral$IMD10 == 4] <- "2"
referral$IMD5[referral$IMD10 == 5] <- "3"
referral$IMD5[referral$IMD10 == 6] <- "3"
referral$IMD5[referral$IMD10 == 7] <- "4"
referral$IMD5[referral$IMD10 == 8] <- "4"
referral$IMD5[referral$IMD10 == 9] <- "5 (most deprived)"
referral$IMD5[referral$IMD10 == 10] <- "5 (most deprived)"

referral$IMD5 <- factor(referral$IMD5, levels = c("1 (least deprived)", "2", "3", "4", "5 (most deprived)"))

colnames(referral)

table(referral$IMD5)

result <- referral[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  .(year, IMD5)
]

table(referral$year)
summary(result)
plot(result$year, result$IRRsuccess)

referral[, .(total_ppl = sum(ppl_N_total),
                      total_py = sum(person_years_total),
                      ppl_to_py_ratio = sum(ppl_N_total)/sum(person_years_total)),
                  .(year)]
head(referral)

# Add national average
national_referral <- referral[
  !age %in% 11:17 & IMD10 != "IMD unavailable",
  .(
    IRRsuccess    = sum(n_total_incidence) / sum(person_years_total),
    IRRsuccess_lo = get_ci_lo(sum(n_total_incidence), sum(person_years_total)),
    IRRsuccess_hi = get_ci_hi(sum(n_total_incidence), sum(person_years_total))
  ),
  by = year
]

result[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

national_referral[, `:=`(
  IRRsuccess = IRRsuccess * 1000,
  IRRsuccess_lo = IRRsuccess_lo * 1000,
  IRRsuccess_hi = IRRsuccess_hi * 1000
)]

# build linetypes & shapes for the 5 IMD quintiles + national average
imd_levels <- levels(result$IMD5)
line_types <- c(
  setNames(rep("solid", length(imd_levels)), imd_levels),
  "National average" = "longdash"
)
pt_shapes  <- c(
  setNames(c(16, 17, 15, 18, 19), imd_levels),
  "National average" = 18
)

# Plot
ggplot() +
  # Horizontal reference lines
  geom_hline(
    yintercept = seq(0, 110, by = 10),
    color = "grey90",
    size = 0.3,
    linetype = "dashed"
  ) +

  # CI ribbons for IMD5
  geom_ribbon(
    data = result,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi, fill = IMD5, group = IMD5),
    alpha = 0.2,
    color = NA
  ) +

  # IMD lines and points
  geom_line(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, linetype = IMD5, group = IMD5),
    size = 1
  ) +
  geom_point(
    data = result,
    aes(x = year, y = IRRsuccess, color = IMD5, shape = IMD5, group = IMD5),
    size = 2
  ) +

  # National average ribbon
  geom_ribbon(
    data = national_referral,
    aes(x = year, ymin = IRRsuccess_lo, ymax = IRRsuccess_hi),
    fill = "black",
    alpha = 0.1,
    inherit.aes = FALSE
  ) +

  # National average line and point
  geom_line(
    data = national_referral,
    aes(x = year, y = IRRsuccess, color = "National average", linetype = "National average"),
    size = 1.2,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = national_referral,
    aes(x = year, y = IRRsuccess, color = "National average", shape = "National average"),
    size = 3,
    inherit.aes = FALSE
  ) +

  # Scales
  scale_x_continuous(
    breaks = seq(min(result$year), max(result$year), by = 1)
  ) +
  scale_y_continuous(
    limits = c(0, 40),
    labels = scales::number_format(accuracy = 1)
  ) +

  # Aesthetic legends
  scale_color_manual(name = "IMD Quintile", values = imd_cols) +
  scale_fill_manual(name = "IMD Quintile", values = imd_cols) +
  # then in your plot replace the old scales with:
  scale_linetype_manual(name = "IMD Quintile", values = line_types) +
  scale_shape_manual   (name = "IMD Quintile", values = pt_shapes) +

  guides(fill = "none") +

  # Labels and theme
  labs(
    x = "Year",
    y = "Referrals (per 1,000 person-years)",
    title = "Smoking cessation referrals over time by IMD quintile"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x      = element_text(angle = 45, hjust = 1)
  )

ggsave("output/plots/", width = 3000, height = 1500, units = "px")

national_referral

