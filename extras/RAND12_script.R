norwegian_labels

# ======================================================
# Setup; load packages, set theme, simulate some data
# ======================================================

packages <- c(
  "ggplot2", "dplyr", "tidyr", "ggsci", "ggrain",
  "viridis", "hrbrthemes"
)
new.packages <- packages[!(packages %in% installed.packages()[, "Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(packages, library, character.only = TRUE)


# Define a custom theme function based on theme_bw()
theme_nice <- function(base_size = 10) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid       = element_blank(),
      strip.background = element_blank(),
      strip.text       = element_text(size = base_size)
    )
}



# ---- Simulate Data (as in Shiny App) ----
set.seed(123)
n_per_year <- 500
years <- 2019:2025
n_total <- n_per_year * length(years)

df <- data.frame(
  year = rep(years, each = n_per_year),
  gender = sample(c("Male", "Female"), n_total, replace = TRUE, prob = c(0.48, 0.52)),
  age_group = sample(c("<30", "30-39", "40-49", "50-59", "60-69", "70+"), n_total, replace = TRUE,
                     prob = c(0.15, 0.2, 0.2, 0.2, 0.15, 0.1)),
  education = sample(c("Low", "Medium", "High"), n_total, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  treatment_status = sample(c("In treatment", "No treatment", "Unknown"), n_total, replace = TRUE,
                            prob = c(0.6, 0.3, 0.1))
)

diagnosis_levels <- paste("diagnosis", 1:15)
df$diagnosis <- sample(diagnosis_levels, n_total, replace = TRUE)
df$diagnosis <- factor(df$diagnosis, levels = diagnosis_levels)

diagnosis_effect_physical <- setNames(seq(10, -10, length.out = 15), diagnosis_levels)
diagnosis_effect_mental   <- setNames(seq(8, -8, length.out = 15), diagnosis_levels)

df <- df %>%
  rowwise() %>%
  mutate(age_num = case_when(
    age_group == "<30"   ~ runif(1, 18, 29),
    age_group == "30-39" ~ runif(1, 30, 39),
    age_group == "40-49" ~ runif(1, 40, 49),
    age_group == "50-59" ~ runif(1, 50, 59),
    age_group == "60-69" ~ runif(1, 60, 69),
    age_group == "70+"   ~ runif(1, 70, 85)
  )) %>%
  ungroup()

df$age_group <- factor(df$age_group, levels = c("<30", "30-39", "40-49", "50-59", "60-69", "70+"))
df$education <- factor(df$education, levels = c("Low", "Medium", "High"))
df$treatment_status <- factor(df$treatment_status, levels = c("In treatment", "No treatment", "Unknown"))
df$gender <- factor(df$gender, levels = c("Male", "Female"))

age_effect_physical <- c("<30" = 0, "30-39" = -2, "40-49" = -4, "50-59" = -6, "60-69" = -8, "70+" = -10)
gender_effect_physical <- c("Male" = 0, "Female" = -10)
edu_effect <- c("Low" = -5, "Medium" = 0, "High" = 10)
treatment_effect_physical <- c("In treatment" = 7, "No treatment" = -4, "Unknown" = 0)

df <- df %>% mutate(year_effect = (year - 2019) * 1.5)

df <- df %>%
  mutate(
    PF = 60 + age_effect_physical[age_group] + gender_effect_physical[gender] +
      edu_effect[education] + treatment_effect_physical[treatment_status] +
      diagnosis_effect_physical[diagnosis] + year_effect + rnorm(n_total, 0, 5),
    RP = 55 + age_effect_physical[age_group] + gender_effect_physical[gender] +
      edu_effect[education] + treatment_effect_physical[treatment_status] +
      diagnosis_effect_physical[diagnosis] + year_effect + rnorm(n_total, 0, 5),
    BP = 35 + age_effect_physical[age_group] + gender_effect_physical[gender] +
      edu_effect[education] + treatment_effect_physical[treatment_status] +
      diagnosis_effect_physical[diagnosis] + year_effect + rnorm(n_total, 0, 5),
    GH = 40 + age_effect_physical[age_group] + gender_effect_physical[gender] +
      edu_effect[education] + treatment_effect_physical[treatment_status] +
      diagnosis_effect_physical[diagnosis] + year_effect + rnorm(n_total, 0, 5)
  )

gender_effect_mental <- c("Male" = 0, "Female" = -10)
treatment_effect_mental <- c("In treatment" = 5, "No treatment" = -6, "Unknown" = 0)

df <- df %>%
  mutate(
    VT = 56 + gender_effect_mental[gender] + edu_effect[education] +
      treatment_effect_mental[treatment_status] + diagnosis_effect_mental[diagnosis] +
      year_effect + rnorm(n_total, 0, 5),
    SF = 40 + gender_effect_mental[gender] + edu_effect[education] +
      treatment_effect_mental[treatment_status] + diagnosis_effect_mental[diagnosis] +
      year_effect + rnorm(n_total, 0, 5),
    RE = 35 + gender_effect_mental[gender] + edu_effect[education] +
      treatment_effect_mental[treatment_status] + diagnosis_effect_mental[diagnosis] +
      year_effect + rnorm(n_total, 0, 5),
    MH = 60 + gender_effect_mental[gender] + edu_effect[education] +
      treatment_effect_mental[treatment_status] + diagnosis_effect_mental[diagnosis] +
      year_effect + rnorm(n_total, 0, 5)
  )

df <- df %>%
  mutate(across(c(PF, RP, BP, GH, VT, SF, RE, MH), ~ pmax(pmin(., 100), 0)))

df <- df %>%
  mutate(
    physical_sum = rowMeans(select(., PF, RP, BP, GH), na.rm = TRUE),
    mental_sum   = rowMeans(select(., VT, SF, RE, MH), na.rm = TRUE)
  )

df$year_effect <- NULL

df_long <- df %>%
  pivot_longer(
    cols = c(PF, RP, BP, GH, VT, SF, RE, MH, physical_sum, mental_sum),
    names_to = "subscale",
    values_to = "score"
  )


#----Defining labels (norwegian/english)

# English labels
english_labels <- c(
  PF = "Physical Functioning",
  RP = "Role-Physical",
  BP = "Bodily Pain",
  GH = "General Health",
  VT = "Vitality",
  SF = "Social Functioning",
  RE = "Role-Emotional",
  MH = "Mental Health",
  physical_sum = "PSC-12 (Physical Total Score)",
  mental_sum   = "MSC-12 (Mental Total Score)"
)

# Norwegian labels
norwegian_labels <- c(
  PF = "Fysisk funksjon",
  RP = "Fysisk rollefunksjon",
  BP = "Kroppssmerter",
  GH = "Generell helse",
  VT = "Vitalitet",
  SF = "Sosial funksjon",
  RE = "Emosjonell rollefunksjon",
  MH = "Mental helse",
  physical_sum = "PSC-12 (Fysisk Totalskåre)",
  mental_sum   = "MSC-12 (Mental Totalskåre)"
)



# -------------------------------
# PLOT 1: Density plots (Summary Scores, faceted)
# -------------------------------
p1 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = score)) +
  geom_density(fill = "#A4A9AD", alpha = 0.8, color = "white") +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Distribution of Summary Scores",
       x = "Score", y = "Density") +
  theme_nice()
# facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels)) # optional
# ggsave("p1_summary_density.pdf", p1, width = 13, height = 8)

p1

# -------------------------------
# PLOT 2: Density with Education Groups
# -------------------------------
p2 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = score, fill = education)) +
  geom_density(alpha = 0.7, color = NA) +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  scale_fill_uchicago() +
  labs(title = "Summary Scores by Education",
       x = "Score", y = "Density") +
  theme_nice()
# facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels)) # optional
# ggsave("p2_summary_density_education.pdf", p2, width = 13, height = 8)

p2

# -------------------------------
# PLOT 3: Violin + Boxplot by Education
# -------------------------------
p3 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = education, y = score, fill = education)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.2, color = "black", alpha = 0.8) +
  scale_fill_uchicago() +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Summary Scores by Education",
       x = "Education", y = "Score") +
  theme_nice()
# facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels)) # optional
# ggsave("p3_summary_box_violin.pdf", p3, width = 13, height = 8)

p3


#----Adapting plot3; add significance tests between groups within each facet----
#install.packages("ggsignif")
library(ggsignif)
#run ?ggsignif in console for info

library(ggsignif)


p3_2 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
               aes(x = education, y = score, fill = education)) +
  geom_violin(alpha = 0.5, color = NA) +
  geom_boxplot(width = 0.2, color = "black", alpha = 0.8) +
  geom_signif(comparisons = list(c("Low", "Medium"),
                                 c("Low", "High"),
                                 c("Medium", "High")),
              test = "t.test",
              map_signif_level = T,
              step_increase = 0.1) +
  scale_fill_uchicago() +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Summary Scores by Education",
       x = "Education", y = "Score") +
  theme_nice()

p3_2


# -------------------------------
# PLOT 4: Histogram of MSC-12
# -------------------------------
p4 <- ggplot(df_long %>% filter(subscale == "mental_sum"),
             aes(x = score)) +
  geom_histogram(binwidth = 2, fill = "#A4A9AD", color = "white", alpha = 0.8) +
  labs(title = "Distribution of MSC-12",
       x = "Score", y = "Count") +
  theme_nice()
# ggsave("p4_hist_mental_sum.pdf", p4, width = 13, height = 8)

p4

# -------------------------------
# PLOT 5: Raincloud Plot by Gender, Faceted by Summary Scale
# -------------------------------
p5 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = gender, y = score, fill = gender, color = gender)) +
  geom_rain(alpha = 0.7,
            point.args = list(alpha = 0.04, size = 3),
            boxplot.args = list(color = "black", alpha = 0.9, width = 0.1)) +
  scale_fill_uchicago() +
  scale_color_uchicago() +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Summary Scores by Gender",
       x = "Gender", y = "Score") +
  theme_nice()
# facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels)) # optional Norwegian labels
# ggsave("p5_raincloud_summary_gender_facet.pdf", p5, width = 13, height = 8)

p5

# -------------------------------
# PLOT 6: Raincloud by Treatment Status with Group-Wise Means
# -------------------------------
p6 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = treatment_status, y = score,
                 fill = treatment_status, color = treatment_status)) +
  geom_rain(
    point.args = list(alpha = 0.04, size = 3),
    boxplot.args = list(color = "black", outlier.shape = NA, alpha = 0.7, width = 0.1),
    boxplot.args.pos = list(position = ggpp::position_dodgenudge(x = 0.08, width = 0.12)),
    violin.args = list(alpha = 0.6)
  ) +
  stat_summary(fun = mean, geom = "hline",
               aes(yintercept = ..y.., group = treatment_status, color = treatment_status),
               linewidth = 0.4, linetype = "dashed", show.legend = FALSE) +
  scale_fill_uchicago() +
  scale_color_uchicago() +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Summary Scores by Treatment Status (with Group Means)",
       x = "Treatment Status", y = "Score", colour = "", fill = "") +
  theme_nice() +
  theme(legend.position = "bottom")
# facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels)) # optional Norwegian labels
# ggsave("p6_treatment_raincloud_ref.pdf", p6, width = 13, height = 8)

p6

# -------------------------------
# PLOT 7: Boxplot by Age Group
# -------------------------------
p7 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = age_group, y = score, fill = age_group)) +
  geom_boxplot(color = "black", alpha = 0.7) +
  scale_fill_viridis_d(option = "cividis") +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Summary Scores by Age Group",
       x = "Age Group", y = "Score", fill = "Age groups") +
  theme_nice()
# facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels)) # optional
# ggsave("p7_boxplot_age_summary.pdf", p7, width = 13, height = 8)

p7

# -------------------------------
# PLOT 8: Trend (Observed Means) by Gender
# -------------------------------
p8 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = year, y = score, color = gender)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun = mean, geom = "point", size = 5) +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  scale_color_uchicago() +
  labs(title = "Summary Scores Over Time by Gender",
       x = "Year", y = "Score") +
  theme_nice()
# ggsave("p8_trend_gender_summary.pdf", p8, width = 13, height = 8)

p8

# -------------------------------
# PLOT 9: Trend (Loess) by Age Group, Faceted by Gender and Subscale
# -------------------------------
p9 <- ggplot(df_long %>% filter(subscale %in% c("physical_sum", "mental_sum")),
             aes(x = year, y = score, color = age_group)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.4, linewidth = 0.8) +
  facet_grid(rows = vars(gender), cols = vars(subscale),
             labeller = labeller(subscale = english_labels)) +
  scale_color_uchicago() +
  labs(title = "Summary Scores Over Time by Age Group and Gender (Loess)",
       x = "Year", y = "Score", colour = "Age groups") +
  coord_cartesian(ylim = c(20, 75)) +
  theme_nice() +
  guides(colour = guide_legend(override.aes = list(fill = NA)))
# facet_grid(rows = vars(gender), cols = vars(subscale),
#            labeller = labeller(subscale = norwegian_labels)) # optional Norwegian labels
# ggsave("p9_trend_loess_age_gender_summary.pdf", p9, width = 13, height = 8)

p9


# -------------------------------
# PLOT 9: Boxplot by Diagnosis (Mental Subdomains)
# -------------------------------
p10 <- ggplot(df_long %>% filter(subscale %in% c("VT", "SF")),
             aes(x = diagnosis, y = score, fill = diagnosis)) +
  geom_boxplot(color = "black", alpha = 0.8) +
  scale_fill_viridis_d(option = "cividis") +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Mental Health Subdomains by Diagnosis",
       x = "Diagnosis", y = "Score") +
  theme_nice() +
  coord_flip() +
  theme(legend.position = "none")
# ggsave("p9_boxplot_diagnosis_mental.pdf", p9, width = 13, height = 8)

p10

# -------------------------------
# PLOT 10: Raincloud by Diagnosis (Mental Subdomains)
# -------------------------------
p11 <- ggplot(df_long %>% filter(subscale %in% c("VT", "SF")),
              aes(x = diagnosis, y = score, fill = diagnosis)) +
  geom_rain(alpha = 0.6,
            point.args = list(alpha = 0.05, shape = 21, size = 1.6),
            boxplot.args = list(color = "black", alpha = 0.8)) +
  scale_fill_viridis_d(option = "cividis") +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  labs(title = "Mental Subdomains by Diagnosis (Raincloud)",
       x = "Diagnosis", y = "Score") +
  theme_nice() +
  coord_flip() +
  geom_hline(yintercept = 50, linetype = "dashed") +
  theme(legend.position = "none")
# ggsave("p10_raincloud_diagnosis_mental.pdf", p10, width = 13, height = 8)

p11


# -------------------------------
# PLOT 12: Trend Plot with Loess by Education (All Subdomains)
# -------------------------------
p12 <- ggplot(df_long %>% filter(subscale %in% c("PF", "RP", "BP", "GH", "VT", "SF", "RE", "MH")),
              aes(x = year, y = score, color = education)) +
  geom_smooth(method = "loess", se = TRUE, alpha = 0.5, linewidth = 0.8) +
  facet_wrap(~ subscale, labeller = as_labeller(english_labels)) +
  scale_color_jama() +
  labs(title = "Subdomain Scores Over Time by Education",
       x = "Year", y = "Score", colour = "Education") +
  coord_cartesian(ylim = c(20, 80)) +
  theme_nice() +
  guides(colour = guide_legend(override.aes = list(fill = NA)))
# ggsave("p12_trend_loess_subdomains.pdf", p12, width = 13, height = 8)

p12


# ============================================================
# Notes on Customizing ggplot Visualizations (RAND-12 Script)
# ============================================================

# All plots in this script are created with ggplot2 and use a clean visual style (`theme_nice()`),
# with grouped comparisons shown using color fills or outlines. You can easily customize many aspects
# of these plots, as shown in the examples above.

# -------------------------------
# Themes:
# -------------------------------
# We use `theme_nice()` for a clean and readable default.
# You can switch to other built-in ggplot themes by replacing `theme_nice()` with:
#   - theme_bw()
#   - theme_classic()
#   - theme_minimal()
#   - theme_gray()
#   - theme_void()     # for blank background

# -------------------------------
# Color Palettes:
# -------------------------------
# Most plots use the `uchicago` palette from the `ggsci` package for clear, readable groups:
#   scale_fill_uchicago()
#   scale_color_uchicago()
#
# You can switch to other built-in palettes depending on your target audience or aesthetic preferences.
# These are all available via packages already loaded in this script:
#
# From `ggsci`:
#   - scale_fill_npg()         # Nature Publishing Group
#   - scale_fill_lancet()      # The Lancet
#   - scale_fill_jco()         # Journal of Clinical Oncology
#   - scale_fill_nejm()        # New England Journal of Medicine
#   - scale_fill_d3()
#   - scale_fill_simpsons()
#   - scale_fill_tron()
#   - scale_fill_futurama()
#   - scale_fill_rickandmorty()
#
#   And similarly for outlines:
#   - scale_color_npg(), scale_color_lancet(), etc.
#
# From `viridis` via `ggplot2`:
#   - scale_fill_viridis_d(option = "cividis")
#   - scale_fill_viridis_d(option = "magma")
#   - scale_fill_viridis_d(option = "plasma")
#   - scale_fill_viridis_d(option = "inferno")
#   - scale_fill_viridis_d(option = "viridis")
#
# These palettes are perceptually uniform and colorblind-friendly.

# -------------------------------
# Manually Setting Colors:
# -------------------------------
# You can manually set colors using `scale_fill_manual()` or `scale_color_manual()`.

# Example for fills:
#   scale_fill_manual(values = c("Low" = "#E41A1C", "Medium" = "#377EB8", "High" = "#4DAF4A"))

# Example for outlines:
#   scale_color_manual(values = c("Male" = "#984EA3", "Female" = "#FF7F00"))

# Useful resources:
#   - https://colorbrewer2.org/              # ColorBrewer palettes
#   - https://coolors.co/                    # Custom palettes and inspiration
#   - https://r-graph-gallery.com/ggplot2-color.html # Examples of R-friendly color scales

# -------------------------------
# Facet Labels Language:
# -------------------------------
# All `facet_wrap(~ subscale)` lines use `english_labels` by default.
# To switch to Norwegian, replace with:
#   facet_wrap(~ subscale, labeller = as_labeller(norwegian_labels))

# -------------------------------
# Saving Figures:
# -------------------------------
# All plots are stored as named objects (`p1`, `p2`, etc.). You can export them using ggsave:
#   ggsave("plotname.pdf", p1, width = 13, height = 8)
#   ggsave("plotname.png", p1, width = 13, height = 8, dpi = 300)
# All saving lines are already included and commented out after each plot.

# ------------------------------------------------------------
# Great resoruces:
#   - ggplot2 documentation: https://ggplot2.tidyverse.org/
#   - ggplot2 graph gallery: https://r-graph-gallery.com/ggplot2-package.html

