
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

prescription_sens_save <- prescription_sens

prescription_sens1 <- prescription_sens_save[IMD5 == "1 (least deprived)"]
prescription_sens2 <- prescription_sens_save[IMD5 == "2"]
prescription_sens3 <- prescription_sens_save[IMD5 == "3"]
prescription_sens4 <- prescription_sens_save[IMD5 == "4"]
prescription_sens5 <- prescription_sens_save[IMD5 == "5 (most deprived)"]

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

# # # # # IMD1

prescription_sens <- prescription_sens1

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
  scale_y_continuous(limits = c(0, 100), labels = number_format(1)) +
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
    title = "Nicotine replacement therapy prescription by age group and ethnicity (IMD1)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# # save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)
# # # # # IMD2

prescription_sens <- prescription_sens2

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
  scale_y_continuous(limits = c(0, 100), labels = number_format(1)) +
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
    title = "Nicotine replacement therapy prescription by age group and ethnicity (IMD2)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# # save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)

# # # # # IMD3

prescription_sens <- prescription_sens3

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
  scale_y_continuous(limits = c(0, 100), labels = number_format(1)) +
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
    title = "Nicotine replacement therapy prescription by age group and ethnicity (IMD3)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# # save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)

# # # # # IMD4

prescription_sens <- prescription_sens4

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
  scale_y_continuous(limits = c(0, 100), labels = number_format(1)) +
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
    title = "Nicotine replacement therapy prescription by age group and ethnicity (IMD4)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# # save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)

# # # # # IMD5

prescription_sens <- prescription_sens5

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
  scale_y_continuous(limits = c(0, 100), labels = number_format(1)) +
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
    title = "Nicotine replacement therapy prescription by age group and ethnicity (IMD5)"
  ) +
  theme_minimal() +
  theme(
    panel.grid.minor    = element_blank(),
    axis.text.x         = element_text(angle = 45, hjust = 1),
    plot.title          = element_text(hjust = 0.5)
  )

print(p)

# # save
ggsave(
  "output/plots/",
  plot = p,
  width = 3000, height = 1500, units = "px"
)

prescription_sens_save[, .(n = sum(ppl_N_total)), .(IMD5, eth5)] %>% arrange(IMD5, eth5) %>% print()

