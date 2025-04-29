# ====================================
# Load required libraries (quietly)
# ====================================
suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinythemes))
suppressPackageStartupMessages(library(shinyWidgets))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(colourpicker))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(ggsci))
suppressPackageStartupMessages(library(haven))
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(ggrain))

# ===== Language Dictionaries =====

# Define language dictionaries
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

norwegian_labels <- c(
  PF = "Fysisk funksjon",
  RP = "Fysisk rollefunksjon",
  BP = "Kroppssmerter",
  GH = "Generell helse",
  VT = "Vitalitet",
  SF = "Sosial funksjon",
  RE = "Emosjonell rollefunksjon",
  MH = "Mental helse",
  physical_sum = "PSC-12 (Fysisk totalskåre)",
  mental_sum   = "MSC-12 (Mental totalskåre)"
)

# ===== Data Simulation =====
set.seed(123)
n_per_year <- 500
years <- 2019:2025
n_total <- n_per_year * length(years)

# background variables
sim_data <- data.frame(
  årstall = rep(years, each = n_per_year),
  kjønn = sample(c("Mann", "Kvinne"), n_total, replace = TRUE, prob = c(0.48, 0.52)),
  aldersgruppe = sample(c("<30", "30-39", "40-49", "50-59", "60-69", "70+"), n_total, replace = TRUE,
                        prob = c(0.15, 0.2, 0.2, 0.2, 0.15, 0.1)),
  utdanning = sample(c("Lav", "Middels", "Høy"), n_total, replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  behandlingsstatus = sample(c("Under behandling", "Ingen behandling", "Ukjent"), n_total, replace = TRUE,
                             prob = c(0.6, 0.3, 0.1))
)
# Diagnosis variables
diagnosis_levels <- paste("diagnosis", 1:15)
sim_data$diagnosis <- sample(diagnosis_levels, n_total, replace = TRUE)
sim_data$diagnosis <- factor(sim_data$diagnosis, levels = diagnosis_levels)
# Define diagnosis effects for physical and mental scores
diagnosis_effect_physical <- setNames(seq(10, -10, length.out = 15), diagnosis_levels)
diagnosis_effect_mental   <- setNames(seq(8, -8, length.out = 15), diagnosis_levels)

# Add continuous age variable "Alder_num"
sim_data <- sim_data %>%
  rowwise() %>%
  mutate(Alder_num = case_when(
    aldersgruppe == "<30"  ~ runif(1, 18, 29),
    aldersgruppe == "30-39" ~ runif(1, 30, 39),
    aldersgruppe == "40-49" ~ runif(1, 40, 49),
    aldersgruppe == "50-59" ~ runif(1, 50, 59),
    aldersgruppe == "60-69" ~ runif(1, 60, 69),
    aldersgruppe == "70+"   ~ runif(1, 70, 85)
  )) %>%
  ungroup()

# Convert categorical variables to factors with meaningful order
sim_data$aldersgruppe <- factor(sim_data$aldersgruppe, levels = c("<30", "30-39", "40-49", "50-59", "60-69", "70+"))
sim_data$utdanning <- factor(sim_data$utdanning, levels = c("Lav", "Middels", "Høy"))
sim_data$behandlingsstatus <- factor(sim_data$behandlingsstatus, levels = c("Under behandling", "Ingen behandling", "Ukjent"))
sim_data$kjønn <- factor(sim_data$kjønn, levels = c("Mann", "Kvinne"))

# Define effects for physical subscales (PF, RP, BP, GH)
age_effect_physical <- c("<30" = 0, "30-39" = -2, "40-49" = -4, "50-59" = -6, "60-69" = -8, "70+" = -10)
gender_effect_physical <- c("Mann" = 0, "Kvinne" = -10)
edu_effect <- c("Lav" = -5, "Middels" = 0, "Høy" = 10)
treatment_effect_physical <- c("Under behandling" = 7, "Ingen behandling" = -4, "Ukjent" = 0)

# Year effect: scores increase over time
sim_data <- sim_data %>% mutate(year_effect = (årstall - 2019) * 1.5)

# Simulate physical subscale scores: PF, RP, BP, GH
sim_data <- sim_data %>%
  mutate(
    PF = 60 + as.numeric(age_effect_physical[as.character(aldersgruppe)]) +
      gender_effect_physical[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_physical[as.character(behandlingsstatus)] +
      diagnosis_effect_physical[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5),
    RP = 55 + as.numeric(age_effect_physical[as.character(aldersgruppe)]) +
      gender_effect_physical[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_physical[as.character(behandlingsstatus)] +
      diagnosis_effect_physical[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5),
    BP = 35 + as.numeric(age_effect_physical[as.character(aldersgruppe)]) +
      gender_effect_physical[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_physical[as.character(behandlingsstatus)] +
      diagnosis_effect_physical[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5),
    GH = 40 + as.numeric(age_effect_physical[as.character(aldersgruppe)]) +
      gender_effect_physical[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_physical[as.character(behandlingsstatus)] +
      diagnosis_effect_physical[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5)
  )

# Define effects for mental subscales (VT, SF, RE, MH)
gender_effect_mental <- c("Mann" = 0, "Kvinne" = -10)
treatment_effect_mental <- c("Under behandling" = 5, "Ingen behandling" = -6, "Ukjent" = 0)

sim_data <- sim_data %>%
  mutate(
    VT = 56 + gender_effect_mental[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_mental[as.character(behandlingsstatus)] +
      diagnosis_effect_mental[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5),
    SF = 40 + gender_effect_mental[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_mental[as.character(behandlingsstatus)] +
      diagnosis_effect_mental[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5),
    RE = 35 + gender_effect_mental[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_mental[as.character(behandlingsstatus)] +
      diagnosis_effect_mental[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5),
    MH = 60 + gender_effect_mental[as.character(kjønn)] +
      edu_effect[as.character(utdanning)] +
      treatment_effect_mental[as.character(behandlingsstatus)] +
      diagnosis_effect_mental[as.character(diagnosis)] +  # Added diagnosis effect
      year_effect + rnorm(n_total, 0, 5)
  )

# Bound the subscale scores between 0 and 100
sim_data <- sim_data %>% mutate_at(vars(PF, RP, BP, GH, VT, SF, RE, MH), ~ pmax(pmin(., 100), 0))

# Compute summary scores: physical_sum and mental_sum
sim_data <- sim_data %>%
  mutate(
    physical_sum = rowMeans(select(., PF, RP, BP, GH), na.rm = TRUE),
    mental_sum = rowMeans(select(., VT, SF, RE, MH), na.rm = TRUE)
  )

# Remove helper variable year_effect
sim_data$year_effect <- NULL

# Convert data to long format
simulated_long <- sim_data %>% pivot_longer(
  cols = c(PF, RP, BP, GH, VT, SF, RE, MH, physical_sum, mental_sum),
  names_to = "subscale",
  values_to = "score"
)

# Identify categorical variables
group_vars <- names(simulated_long)[sapply(simulated_long, function(x) is.factor(x) || is.character(x))]
group_vars <- setdiff(group_vars, "subscale")


# ===== UI =====

## ---- Navbar page ----
ui <- navbarPage("RAND-12 Data Visualization",
                 id = "navbar",
                 theme = shinytheme("cerulean"),
                 inverse = TRUE,
                 header = tagList(
                   conditionalPanel(
                     condition = "input.navbar == 'summary' || input.navbar == 'subdomain'",
                     absolutePanel(
                       id = "downloadPanel", fixed = TRUE, draggable = FALSE,
                       top = NA, bottom = 20, right = 20, width = 250,
                       actionButton("download_btn", "Download Figure", icon = icon("download"))
                     )
                   ),
                   conditionalPanel(
                     condition = "input.navbar == 'summary' || input.navbar == 'subdomain'",
                     absolutePanel(
                       id = "languagePanel", fixed = TRUE, draggable = FALSE,
                       bottom = 10, left = 10, width = 300,
                       style = "background-color: rgba(255,255,255,0.8); padding: 10px; border-radius: 8px;",
                       h5("Facet Labels Language"),
                       p("Select the language for facet labels (only affects faceted plots):"),
                       radioButtons("language", NULL, choices = c("English", "Norwegian"),
                                    selected = "English", inline = TRUE)
                     )
                   )
                 ),

                 ### ---- Instructions Tab ----
                 tabPanel("Instructions", value = "instructions",
                          tags$div(
                            tags$h3("About"),
                            tags$p("This app is developed to quickly create a variety of professional data visualizations for the RAND-12 instrument.
                                   It is designed for researchers, clinicians, and anyone needing high-quality plots for reports, research articles, exploratory analysis, or just for fun."),
                            tags$p("The RAND 12-Item Health Survey (RAND-12) is a measure of overall health status that assesses physical and mental components of health
                                    through 12 items spanning domains such as physical functioning, role limitations, bodily pain, general health, vitality,
                                    social functioning, and mental health. Developed from the SF-36 instrument, the RAND-12 yields two main summary scores:"),
                            tags$ul(
                              tags$li(tags$strong("PSC-12 (Physical Summary Component):"), " Measures overall physical health."),
                              tags$li(tags$strong("MSC-12 (Mental Summary Component):"), " Measures overall mental health.")
                            ),

                            tags$h4("Uploading Your Own Data"),
                            tags$p("You can either use the built-in simulated data or upload your own dataset via the 'Data' tab. Accepted file formats are: SPSS (.sav), Stata (.dta), Excel (.xls, .xlsx), and CSV (.csv).",
                                   tags$br(),
                                   "For the app to function properly, your data must include the following RAND-12 variables with these naming conventions:"),
                            tags$table(
                              class = "table table-bordered table-condensed table-striped table-hover",
                              style = "background-color: #ffffff; padding: 2px; border-radius: 3px;
                                       max-width: 400px; margin-left: 0; border-collapse: collapse;
                                       font-size: 12.5px; line-height: 1.1;",
                              tags$thead(
                                style = "background-color: #f0f0f0; font-weight: bold; text-align: left;",
                                tags$tr(
                                  tags$th(style = "width: 25%; padding: 2px;", "Variable Name"),
                                  tags$th(style = "width: 75%; padding: 2px;", "Full Domain Name")
                                )
                              ),
                              tags$tbody(
                                lapply(list(
                                  c("PF", "Physical Functioning"),
                                  c("RP", "Role-Physical"),
                                  c("BP", "Bodily Pain"),
                                  c("GH", "General Health"),
                                  c("VT", "Vitality"),
                                  c("SF", "Social Functioning"),
                                  c("RE", "Role-Emotional"),
                                  c("MH", "Mental Health"),
                                  c("physical_sum", "PSC-12 (Physical Summary Score)"),
                                  c("mental_sum", "MSC-12 (Mental Summary Score)")
                                ), function(row) {
                                  tags$tr(
                                    tags$td(style = "padding: 2px; text-align: left;", row[[1]]),
                                    tags$td(style = "padding: 2px;", row[[2]])
                                  )
                                })
                              )
                            ),
                            tags$p("The app automatically converts these RAND-12 variables from wide to long format to facilitate plotting multiple dimensions in one graph. It also converts any labelled variables (from SPSS or Stata) into factors, and automatically identifies categorical variables for group comparisons"),

                            tags$h4("Navigating the App"),
                            tags$p("The app is organized into several tabs:"),
                            tags$ul(
                              tags$li(tags$strong("'Data' Tab:"), " Preview your dataset."),
                              tags$li(tags$strong("'PSC-12 & MCS-12 (Summary scales)' Tab:"), " Visualize the overall summary scores (PSC-12 & MSC-12) using various plot types. Customize appearance settings such as labels, themes, and color palettes."),
                              tags$li(tags$strong("'Health profiles (sub domains)' Tab:"), " Explore detailed subscale scores across health domains. Choose from various visualizations and group comparison options.")
                            ),
                            tags$p("Once customized, click ",
                                   tags$strong("Download Figure"), " ",
                                   shiny::icon("download"), " to save as PDF or PNG.")
                          )
                 ),

                 ### ---- Data Tab ----
                 tabPanel("Data", value = "data",
                          sidebarLayout(
                            sidebarPanel(
                              radioButtons("data_source", "Select Data Source:",
                                           choices = c("Simulated Data" = "simulated", "Upload Your Own Data" = "upload"),
                                           selected = "simulated"),
                              conditionalPanel(
                                condition = "input.data_source == 'upload'",
                                fileInput("uploaded_file", "Upload File",
                                          accept = c(".sav", ".dta", ".xls", ".xlsx", ".csv")),
                                helpText("Note: The app automatically converts uploaded data from wide format (each subscale in its own column) to long format. In the long format, your data will have a 'subscale' variable (e.g., PF, RP, BP, GH, VT, SF, RE, MH, physical_sum, mental_sum) and a 'score' variable.")
                              )
                            ),
                            mainPanel(
                              h4("Data Preview (first 10 rows)"),
                              DT::dataTableOutput("data_preview")
                            )
                          )
                 ),

                 ### ---- Summary Tab ----
                 tabPanel("PSC-12 & MCS-12 (Summary scales)", value = "summary",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("summary_choice", "Choose Summary Score:",
                                          choices = c("Physical Summary" = "physical_sum", "Mental Summary" = "mental_sum", "Both" = "both"),
                                          selected = "both"),
                              selectInput("summary_plot_type", "Choose Plot Type:",
                                          choices = c("None" = "none", "Histogram" = "histogram", "Density Plot" = "density",
                                                      "Boxplot" = "boxplot", "Violin Plot" = "violin", "Jitter Plot" = "jitter",
                                                      "Raincloud Plot" = "raincloud", "Trend Line" = "trend"),
                                          selected = "none"),
                              conditionalPanel(
                                condition = "input.summary_plot_type == 'trend'",
                                selectInput("summary_trend_type", "Choose Trend Method:",
                                            choices = c("Observed Means" = "observed", "Smooth (lm)" = "lm", "Smooth (loess)" = "loess"),
                                            selected = "observed"),
                                selectInput("summary_xvar", "X Variable for Trend:", choices = NULL)
                              ),
                              selectInput("summary_group", "Compare Groups:",
                                          choices = c("None" = "none", setNames(group_vars, group_vars)),
                                          selected = "none"),
                              dropdownButton(
                                label = "Reference Lines",
                                icon = icon("sliders-h"),
                                status = "primary",
                                circle = FALSE,
                                width = "300px",
                                radioButtons("ref_statistic", "Reference Statistic:",
                                             choices = c("Mean" = "mean", "Median" = "median"), selected = "mean"),
                                checkboxInput("ref_show_overall", "Show Grand Reference Line", value = FALSE),
                                conditionalPanel(
                                  condition = "input.ref_show_overall == true",
                                  colourInput("ref_overall_color", "Grand Line Color:", value = "#000000"),
                                  numericInput("ref_overall_linewidth", "Grand Line Width:", value = 1, min = 0.1, step = 0.1),
                                  selectInput("ref_overall_linetype", "Grand Line Linetype:",
                                              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                              selected = "dashed")
                                ),
                                checkboxInput("ref_show_groups", "Show Group Reference Lines", value = FALSE),
                                conditionalPanel(
                                  condition = "input.ref_show_groups == true",
                                  numericInput("ref_group_linewidth", "Group Line Width:", value = 1, min = 0.1, step = 0.1),
                                  selectInput("ref_group_linetype", "Group Line Linetype:",
                                              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                              selected = "dashed")
                                ),
                                checkboxInput("ref_custom", "Show Custom Reference Lines", value = FALSE),
                                conditionalPanel(
                                  condition = "input.ref_custom == true",
                                  colourInput("ref_custom_color", "Custom Line Color:", value = "#000000"),
                                  numericInput("ref_custom_linewidth", "Custom Line Width:", value = 1, min = 0.1, step = 0.1),
                                  selectInput("ref_custom_linetype", "Custom Line Linetype:",
                                              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                              selected = "dashed"),
                                  textInput("ref_custom_values", "Custom Reference Line Values (comma-separated):", value = "")
                                )
                              ),
                              dropdownButton(
                                label = "Labels",
                                icon = icon("font"),
                                status = "primary",
                                circle = FALSE,
                                width = "300px",
                                textInput("summary_title", "Plot Title:", value = "Summary Scores"),
                                textInput("summary_xlabel", "X-axis Label (leave blank for default):", value = ""),
                                textInput("summary_ylabel", "Y-axis Label (leave blank for default):", value = ""),
                                textInput("summary_legend_title", "Legend Title:", value = "Group"),
                                numericInput("summary_base_size", "Size Option:", value = 14, min = 8, max = 50, step = 1)
                              ),
                              dropdownButton(
                                label = "Appearance",
                                icon = icon("paint-brush"),
                                status = "primary",
                                circle = FALSE,
                                width = "300px",
                                selectInput("summary_theme", "Choose ggplot Theme:",
                                            choices = c("nice", "bw", "classic", "minimal", "gray", "void"),
                                            selected = "nice"),
                                # New dual palette selection controls for Summary tab
                                radioButtons("summary_color_mode", "Choose Color Mode:",
                                             choices = c("Sequential" = "seq", "Qualitative" = "qual"),
                                             selected = "qual", inline = TRUE),
                                conditionalPanel(
                                  condition = "input.summary_color_mode == 'qual'",
                                  selectInput("summary_qual_palette", "Qualitative Palette:",
                                              choices = c("uchicago", "npg", "lancet", "jco", "nejm",
                                                          "d3", "simpsons", "tron", "futurama", "rickandmorty"),
                                              selected = "uchicago")
                                ),
                                conditionalPanel(
                                  condition = "input.summary_color_mode == 'seq'",
                                  selectInput("summary_seq_palette", "Sequential Palette:",
                                              choices = c("Greens", "Purples", "Blues", "Viridis", "Magma", "Plasma", "Inferno", "Cividis", "Greys"),
                                              selected = "Blues")
                                ),
                                conditionalPanel(
                                  condition = "input.summary_plot_type == 'trend'",
                                  sliderInput("summary_line_width", "Trend Line Width:",
                                              min = 0.1, max = 5, value = 1, step = 0.1),
                                  sliderInput("summary_point_size", "Trend Point Size:",
                                              min = 1, max = 10, value = 2, step = 0.5),
                                  conditionalPanel(
                                    condition = "input.summary_trend_type == 'lm' || input.summary_trend_type == 'loess'",
                                    checkboxInput("summary_show_ci", "Show Confidence Interval", value = TRUE),
                                    conditionalPanel(
                                      condition = "input.summary_show_ci == true",
                                      sliderInput("summary_ci_alpha", "CI Transparency (Alpha):",
                                                  min = 0, max = 1, value = 0.2, step = 0.1)
                                    )
                                  ),
                                  checkboxInput("summary_show_points", "Show Raw Data Points", value = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.summary_plot_type == 'histogram'",
                                  sliderInput("summary_binwidth", "Bin Width:", min = 1, max = 50, value = 2)
                                ),
                                conditionalPanel(
                                  condition = "input.summary_plot_type == 'raincloud' || input.summary_plot_type == 'boxplot' || input.summary_plot_type == 'violin' || input.summary_plot_type == 'jitter'",
                                  checkboxInput("summary_coord_flip", "Flip Coordinates?", value = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.summary_choice == 'both' && (input.summary_plot_type == 'boxplot' || input.summary_plot_type == 'violin' || input.summary_plot_type == 'jitter' || input.summary_plot_type == 'raincloud')",
                                  checkboxInput("summary_facet_subscale", "Facet by Subscale?", value = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.summary_group != 'none'",
                                  checkboxInput("summary_group_n_caption", "Display group counts in caption?", value = FALSE)
                                ),
                                sliderInput("summary_alpha", "Transparency (Alpha):", min = 0, max = 1, value = 0.7, step = 0.1),
                                textInput("summary_xlim", "X-axis Limits (e.g., 0,100):", value = ""),
                                textInput("summary_ylim", "Y-axis Limits (e.g., 0,100):", value = "")
                              )
                            ),
                            mainPanel(
                              plotOutput("summary_plot", height = "70vh")
                            )
                          )
                 ),

                 ### ---- Subdomain Tab ----
                 tabPanel("Health profiles (sub domains)", value = "subdomain",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("subdomain_type", "Choose Subdomain Type:",
                                          choices = c("Physical Subdomains", "Mental Subdomains"),
                                          selected = "Physical Subdomains"),
                              selectInput("subdomain_plot_type", "Choose Plot Type:",
                                          choices = c("None" = "none",
                                                      "Histogram" = "histogram",
                                                      "Density Plot" = "density",
                                                      "Boxplot" = "boxplot",
                                                      "Violin Plot" = "violin",
                                                      "Jitter Plot" = "jitter",
                                                      "Raincloud Plot" = "raincloud",
                                                      "Trend Line" = "trend"),
                                          selected = "none"),
                              conditionalPanel(
                                condition = "input.subdomain_plot_type == 'trend'",
                                selectInput("subdomain_trend_type", "Choose Trend Method:",
                                            choices = c("Observed Means" = "observed",
                                                        "Smooth (lm)" = "lm",
                                                        "Smooth (loess)" = "loess"),
                                            selected = "observed"),
                                selectInput("subdomain_xvar", "X Variable for Trend:", choices = NULL)
                              ),
                              selectInput("subdomain_group", "Compare Groups:",
                                          choices = c("None" = "none", setNames(group_vars, group_vars)),
                                          selected = "none"),
                              dropdownButton(
                                label = "Reference Lines",
                                icon = icon("sliders-h"),
                                status = "primary",
                                circle = FALSE,
                                width = "300px",
                                radioButtons("ref_statistic_subdomain", "Reference Statistic:",
                                             choices = c("Mean" = "mean", "Median" = "median"), selected = "mean"),
                                checkboxInput("ref_show_overall_subdomain", "Show Grand Reference Line", value = FALSE),
                                conditionalPanel(
                                  condition = "input.ref_show_overall_subdomain == true",
                                  colourInput("ref_overall_color_subdomain", "Grand Line Color:", value = "#000000"),
                                  numericInput("ref_overall_linewidth_subdomain", "Grand Line Width:", value = 1, min = 0.1, step = 0.1),
                                  selectInput("ref_overall_linetype_subdomain", "Grand Line Linetype:",
                                              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                              selected = "dashed")
                                ),
                                checkboxInput("ref_show_groups_subdomain", "Show Group Reference Lines", value = FALSE),
                                conditionalPanel(
                                  condition = "input.ref_show_groups_subdomain == true",
                                  numericInput("ref_group_linewidth_subdomain", "Group Line Width:", value = 1, min = 0.1, step = 0.1),
                                  selectInput("ref_group_linetype_subdomain", "Group Line Linetype:",
                                              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                              selected = "dashed")
                                ),
                                checkboxInput("ref_custom_subdomain", "Show Custom Reference Lines", value = FALSE),
                                conditionalPanel(
                                  condition = "input.ref_custom_subdomain == true",
                                  colourInput("ref_custom_color_subdomain", "Custom Line Color:", value = "#000000"),
                                  numericInput("ref_custom_linewidth_subdomain", "Custom Line Width:", value = 1, min = 0.1, step = 0.1),
                                  selectInput("ref_custom_linetype_subdomain", "Custom Line Linetype:",
                                              choices = c("solid", "dashed", "dotted", "dotdash", "longdash", "twodash"),
                                              selected = "dashed"),
                                  textInput("ref_custom_values_subdomain", "Custom Reference Line Values (comma-separated):", value = "")
                                )
                              ),
                              dropdownButton(
                                label = "Labels",
                                icon = icon("font"),
                                status = "primary",
                                circle = FALSE,
                                width = "300px",
                                textInput("subdomain_title", "Plot Title:", value = "Subdomain Scores"),
                                textInput("subdomain_xlabel", "X-axis Label (leave blank for default):", value = ""),
                                textInput("subdomain_ylabel", "Y-axis Label (leave blank for default):", value = ""),
                                textInput("subdomain_legend_title", "Legend Title:", value = "Group"),
                                numericInput("subdomain_base_size", "Size Option:", value = 14, min = 8, max = 50, step = 1)
                              ),
                              dropdownButton(
                                label = "Appearance",
                                icon = icon("paint-brush"),
                                status = "primary",
                                circle = FALSE,
                                width = "300px",
                                selectInput("subdomain_theme", "Choose ggplot Theme:",
                                            choices = c("nice", "bw", "classic", "minimal", "gray", "void"),
                                            selected = "nice"),
                                # New dual palette selection controls for Subdomain tab
                                radioButtons("subdomain_color_mode", "Choose Color Mode:",
                                             choices = c("Sequential" = "seq", "Qualitative" = "qual"),
                                             selected = "qual", inline = TRUE),
                                conditionalPanel(
                                  condition = "input.subdomain_color_mode == 'qual'",
                                  selectInput("subdomain_qual_palette", "Qualitative Palette:",
                                              choices = c("uchicago", "npg", "lancet", "jco", "nejm",
                                                          "d3", "simpsons", "tron", "futurama", "rickandmorty"),
                                              selected = "uchicago")
                                ),
                                conditionalPanel(
                                  condition = "input.subdomain_color_mode == 'seq'",
                                  selectInput("subdomain_seq_palette", "Sequential Palette:",
                                              choices = c("Greens", "Purples", "Blues", "Viridis", "Magma", "Plasma", "Inferno", "Cividis", "Greys"),
                                              selected = "Blues")
                                ),
                                conditionalPanel(
                                  condition = "input.subdomain_plot_type == 'trend'",
                                  sliderInput("subdomain_line_width", "Trend Line Width:",
                                              min = 0.1, max = 5, value = 1, step = 0.1),
                                  sliderInput("subdomain_point_size", "Trend Point Size:",
                                              min = 1, max = 10, value = 2, step = 0.5),
                                  conditionalPanel(
                                    condition = "input.subdomain_trend_type == 'lm' || input.subdomain_trend_type == 'loess'",
                                    checkboxInput("subdomain_show_ci", "Show Confidence Interval", value = TRUE),
                                    conditionalPanel(
                                      condition = "input.subdomain_show_ci == true",
                                      sliderInput("subdomain_ci_alpha", "CI Transparency (Alpha):",
                                                  min = 0, max = 1, value = 0.2, step = 0.1)
                                    )
                                  ),
                                  checkboxInput("subdomain_show_points", "Show Raw Data Points", value = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.subdomain_plot_type == 'histogram'",
                                  sliderInput("subdomain_binwidth", "Bin Width:", min = 1, max = 50, value = 2)
                                ),
                                conditionalPanel(
                                  condition = "input.subdomain_plot_type == 'jitter' || input.subdomain_plot_type == 'boxplot' || input.subdomain_plot_type == 'violin' || input.subdomain_plot_type == 'raincloud'",
                                  checkboxInput("subdomain_coord_flip", "Flip Coordinates?", value = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.subdomain_plot_type == 'boxplot' || input.subdomain_plot_type == 'violin' || input.subdomain_plot_type == 'jitter' || input.subdomain_plot_type == 'raincloud'",
                                  checkboxInput("subdomain_facet_subscale", "Facet by Subscale?", value = FALSE)
                                ),
                                conditionalPanel(
                                  condition = "input.subdomain_group != 'none'",
                                  checkboxInput("subdomain_group_n_caption", "Display group counts in caption?", value = FALSE)
                                ),
                                sliderInput("subdomain_alpha", "Transparency (Alpha):", min = 0, max = 1, value = 0.7, step = 0.1),
                                textInput("subdomain_xlim", "X-axis Limits (e.g., 0,100):", value = ""),
                                textInput("subdomain_ylim", "Y-axis Limits (e.g., 0,100):", value = "")
                              )
                            ),
                            mainPanel(
                              plotOutput("subdomain_plot", height = "70vh")
                            )
                          )
                 ),

                 ### ---- About Tab ----
                 tabPanel("About", value = "about",
                          tags$div(
                            tags$h3("About This Application"),
                            tags$p(
                              "This Shiny application was created by Senior Researcher",
                              tags$strong("Sondre Aasen Nilsen"),
                              " on behalf of the ",
                              tags$strong("Center for Patient-Reported Data at Haukeland University Hospital, Bergen, Norway"), "."
                            ),
                            tags$p("For any inquiries, suggestions, or bug-reports, please send an e-mail to: sondre.aa.nilsen@gmail.com"),
                            tags$p("The app leverages the following amazing open-source R packages for data manipulation and visualization:"),
                            tags$ul(
                              tags$li(
                                tags$strong("shiny"), ": Chang, W., Cheng, J., Allaire, J.J., Sievert, C., Schloerke, B., Xie, Y., Allen, J., McPherson, J., Dipert, A., & Borges, B. (2024).",
                                tags$em("shiny: Web Application Framework for R."),
                                "R package version 1.10.0. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=shiny", "https://CRAN.R-project.org/package=shiny")
                              ),
                              tags$li(
                                tags$strong("shinythemes"), ": Chang, W. (2021).",
                                tags$em("shinythemes: Themes for Shiny."),
                                "R package version 1.2.0. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=shinythemes", "https://CRAN.R-project.org/package=shinythemes")
                              ),
                              tags$li(
                                tags$strong("shinyWidgets"), ": Perrier, V., Meyer, F., & Granjon, D. (2024).",
                                tags$em("shinyWidgets: Custom Inputs Widgets for Shiny."),
                                "R package version 0.8.7. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=shinyWidgets", "https://CRAN.R-project.org/package=shinyWidgets")
                              ),
                              tags$li(
                                tags$strong("ggplot2"), ": Wickham, H. (2016).",
                                tags$em("ggplot2: Elegant Graphics for Data Analysis."),
                                "Springer-Verlag New York. Retrieved from ",
                                tags$a(href="https://ggplot2.tidyverse.org", "https://ggplot2.tidyverse.org")
                              ),
                              tags$li(
                                tags$strong("dplyr"), ": Wickham, H., François, R., Henry, L., Müller, K., & Vaughan, D. (2023).",
                                tags$em("dplyr: A Grammar of Data Manipulation."),
                                "R package version 1.1.4. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=dplyr", "https://CRAN.R-project.org/package=dplyr")
                              ),
                              tags$li(
                                tags$strong("tidyr"), ": Wickham, H., Vaughan, D., & Girlich, M. (2024).",
                                tags$em("tidyr: Tidy Messy Data."),
                                "R package version 1.3.1. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=tidyr", "https://CRAN.R-project.org/package=tidyr")
                              ),
                              tags$li(
                                tags$strong("RColorBrewer"), ": Neuwirth, E. (2022).",
                                tags$em("RColorBrewer: ColorBrewer Palettes."),
                                "R package version 1.1-3. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=RColorBrewer", "https://CRAN.R-project.org/package=RColorBrewer")
                              ),
                              tags$li(
                                tags$strong("ggsci"), ": Xiao, N. (2024).",
                                tags$em("ggsci: Scientific Journal and Sci-Fi Themed Color Palettes for 'ggplot2'."),
                                "R package version 3.2.0. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=ggsci", "https://CRAN.R-project.org/package=ggsci")
                              ),
                              tags$li(
                                tags$strong("ggthemes"), ": Jeffrey B. Arnold (2024).",
                                tags$em("ggthemes: Extra Themes and Geoms for 'ggplot2'."),
                                "R package version 5.1.0. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=ggthemes", "https://CRAN.R-project.org/package=ggthemes")
                              ),
                              tags$li(
                                tags$strong("haven"), ": Wickham, H., Miller, E., & Smith, D. (2023).",
                                tags$em("haven: Import and Export 'SPSS', 'Stata' and 'SAS' Files."),
                                "R package version 2.5.4. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=haven", "https://CRAN.R-project.org/package=haven")
                              ),
                              tags$li(
                                tags$strong("readxl"), ": Wickham, H., & Bryan, J. (2023).",
                                tags$em("readxl: Read Excel Files."),
                                "R package version 1.4.3. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=readxl", "https://CRAN.R-project.org/package=readxl")
                              ),
                              tags$li(
                                tags$strong("readr"), ": Wickham, H., Hester, J., & Bryan, J. (2024).",
                                tags$em("readr: Read Rectangular Text Data."),
                                "R package version 2.1.5. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=readr", "https://CRAN.R-project.org/package=readr")
                              ),
                              tags$li(
                                tags$strong("DT"), ": Xie, Y., Cheng, J., & Tan, X. (2024).",
                                tags$em("DT: A Wrapper of the JavaScript Library 'DataTables'."),
                                "R package version 0.33. Retrieved from ",
                                tags$a(href="https://CRAN.R-project.org/package=DT", "https://CRAN.R-project.org/package=DT")
                              ),
                              tags$li(
                                tags$strong("ggrain"), ": Allen, M., Poggiali, D., Whitaker, K., Marshall, T.R., van Langen, J., & Kievit, R.A. (2021).",
                                tags$em("Raincloud plots: a multi-platform tool for robust data visualization [version 2; peer review: 2 approved]."),
                                "Wellcome Open Research, 4(63). https://doi.org/10.12688/wellcomeopenres.15191.2"
                              )),

                            tags$p("All these packages are licensed under open-source terms, enabling us to build this app and share it freely with you.")
                          )
                 )
)


# ===== Server =====

server <- function(input, output, session) {

  ## ---- Helper Functions ----

  ### ---- Qualitative palettes ----

  get_palette_funcs <- function(palette_name) {
    col_fun <- switch(palette_name,
                      "uchicago" = scale_color_uchicago,
                      "npg" = scale_color_npg,
                      "lancet" = scale_color_lancet,
                      "jco" = scale_color_jco,
                      "nejm" = scale_color_nejm,
                      "d3" = scale_color_d3,
                      "simpsons" = scale_color_simpsons,
                      "tron" = scale_color_tron,
                      "futurama" = scale_color_futurama,
                      "rickandmorty" = scale_color_rickandmorty,
                      scale_color_brewer(palette = "Set2"))
    fill_fun <- switch(palette_name,
                       "uchicago" = scale_fill_uchicago,
                       "npg" = scale_fill_npg,
                       "lancet" = scale_fill_lancet,
                       "jco" = scale_fill_jco,
                       "nejm" = scale_fill_nejm,
                       "d3" = scale_fill_d3,
                       "simpsons" = scale_fill_simpsons,
                       "tron" = scale_fill_tron,
                       "futurama" = scale_fill_futurama,
                       "rickandmorty" = scale_fill_rickandmorty,
                       scale_fill_brewer(palette = "Set2"))
    list(fill_fun = fill_fun, col_fun = col_fun)
  }

  ### ---- Sequential palettes ----

  get_seq_palette_funcs <- function(palette_name) {
    if(palette_name %in% c("Greens", "Purples", "Blues", "Greys", "Oranges", "Reds")){
      pal_fun_fill <- function(...) scale_fill_brewer(palette = palette_name, ...)
      pal_fun_colour <- function(...) scale_color_brewer(palette = palette_name, ...)
    } else if(tolower(palette_name) %in% c("viridis", "magma", "plasma", "inferno")){
      pal_fun_fill <- function(...) scale_fill_viridis_d(option = tolower(palette_name), ...)
      pal_fun_colour <- function(...) scale_color_viridis_d(option = tolower(palette_name), ...)
    } else if(palette_name == "Cividis"){
      pal_fun_fill <- function(...) scale_fill_viridis_d(option = "cividis", ...)
      pal_fun_colour <- function(...) scale_color_viridis_d(option = "cividis", ...)
    } else {
      pal_fun_fill <- function(...) scale_fill_brewer(palette = "Blues", ...)
      pal_fun_colour <- function(...) scale_color_brewer(palette = "Blues", ...)
    }
    list(fill_fun = pal_fun_fill, col_fun = pal_fun_colour)
  }

  ### ---- add_scales ----

  add_scales <- function(ggplot_obj, prefix) {
    group_selected <- (input[[paste0(prefix, "_group")]] != "none")
    if (!group_selected) return(ggplot_obj)
    if (input[[paste0(prefix, "_color_mode")]] == "qual") {
      palette_choice <- input[[paste0(prefix, "_qual_palette")]]
      pal_funcs <- get_palette_funcs(palette_choice)
    } else {
      palette_choice <- input[[paste0(prefix, "_seq_palette")]]
      pal_funcs <- get_seq_palette_funcs(palette_choice)
    }
    ggplot_obj + pal_funcs$fill_fun() + pal_funcs$col_fun()
  }

  ### ---- Add reference lines ----

  add_reference_lines <- function(gg, data, plot_type, group_var, group_selected, facet_subscale,
                                  ref_stat, ref_show_overall, ref_overall_color, ref_overall_linewidth, ref_overall_linetype,
                                  ref_show_groups, ref_group_linewidth, ref_group_linetype,
                                  ref_custom, ref_custom_color, ref_custom_linewidth, ref_custom_linetype, ref_custom_values) {
    # For boxplot, jitter, raincloud, and violin plots, only add reference lines if facet_subscale is TRUE.
    allowed_plots <- c("boxplot", "jitter", "raincloud", "violin")
    if (plot_type %in% allowed_plots && !facet_subscale) {
      return(gg)
    }

    is_xaxis_ref <- plot_type %in% c("histogram", "density")

    if (!is.null(ref_stat)) {
      # Overall reference line(s)
      if (ref_show_overall) {
        overall_refs <- data %>%
          group_by(subscale) %>%
          summarise(ref_value = if (ref_stat == "mean") mean(score) else median(score), .groups = "drop")
        if (is_xaxis_ref) {
          gg <- gg + geom_vline(data = overall_refs, aes(xintercept = ref_value),
                                color = ref_overall_color, size = ref_overall_linewidth,
                                linetype = ref_overall_linetype)
        } else {
          gg <- gg + geom_hline(data = overall_refs, aes(yintercept = ref_value),
                                color = ref_overall_color, size = ref_overall_linewidth,
                                linetype = ref_overall_linetype)
        }
      }
      # Group-specific reference line(s)
      if (ref_show_groups && group_selected) {
        group_refs <- data %>%
          group_by(subscale, grp = .data[[group_var]]) %>%
          summarise(ref_value = if (ref_stat == "mean") mean(score) else median(score), .groups = "drop")
        if (is_xaxis_ref) {
          gg <- gg + geom_vline(data = group_refs, aes(xintercept = ref_value, color = grp),
                                size = ref_group_linewidth, linetype = ref_group_linetype,
                                show.legend = FALSE)
        } else {
          gg <- gg + geom_hline(data = group_refs, aes(yintercept = ref_value, color = grp),
                                size = ref_group_linewidth, linetype = ref_group_linetype,
                                show.legend = FALSE)
        }
      }
      # Custom reference line(s)
      if (ref_custom) {
        custom_vals <- as.numeric(unlist(strsplit(ref_custom_values, ",")))
        if (length(custom_vals) > 0 && any(!is.na(custom_vals))) {
          custom_vals <- custom_vals[!is.na(custom_vals)]
          for (val in custom_vals) {
            if (is_xaxis_ref) {
              gg <- gg + geom_vline(xintercept = val,
                                    color = ref_custom_color, size = ref_custom_linewidth,
                                    linetype = ref_custom_linetype)
            } else {
              gg <- gg + geom_hline(yintercept = val,
                                    color = ref_custom_color, size = ref_custom_linewidth,
                                    linetype = ref_custom_linetype)
            }
          }
        }
      }
    }

    return(gg)
  }


  ## ---- Reactive Data & Dynamic UI ----
  ### ---- current_data Reactive ----
  current_data <- reactive({
    if (input$data_source == "simulated") {
      simulated_long
    } else {
      inFile <- input$uploaded_file
      if (is.null(inFile)) return(NULL)
      ext <- tools::file_ext(inFile$name)
      df <- switch(ext,
                   "sav" = haven::read_sav(inFile$datapath),
                   "dta" = haven::read_dta(inFile$datapath),
                   "xls" = readxl::read_excel(inFile$datapath),
                   "xlsx" = readxl::read_excel(inFile$datapath),
                   "csv" = readr::read_csv(inFile$datapath),
                   NULL)
      if (is.null(df)) return(NULL)
      df_long <- df %>% pivot_longer(
        cols = c("PF", "RP", "BP", "GH", "VT", "SF", "RE", "MH", "physical_sum", "mental_sum"),
        names_to = "subscale",
        values_to = "score"
      )
      return(df_long)
    }
  })

  ### ---- group_vars_dynamic Reactive ----
  group_vars_dynamic <- reactive({
    df <- current_data()
    if (is.null(df)) return(character(0))
    vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
    setdiff(vars, "subscale")
  })

  ### ---- UI Update Observers ----
  observe({
    updateSelectInput(session, "summary_group",
                      choices = c("None" = "none", setNames(group_vars_dynamic(), group_vars_dynamic())))
    updateSelectInput(session, "subdomain_group",
                      choices = c("None" = "none", setNames(group_vars_dynamic(), group_vars_dynamic())))
  })

  observe({
    df <- current_data()
    if (!is.null(df)) {
      updateSelectInput(session, "summary_xvar",
                        choices = names(df),
                        selected = if("årstall" %in% names(df)) "årstall" else names(df)[1])
      updateSelectInput(session, "subdomain_xvar",
                        choices = names(df),
                        selected = if("årstall" %in% names(df)) "årstall" else names(df)[1])
    }
  })

  ## ---- Data Preview Output ----
  output$data_preview <- DT::renderDT({
    head(current_data(), 10)
  })

  ## ---- Filtering Functions ----

  ### ---- get_summary_data ----
  get_summary_data <- function() {
    df <- current_data()
    if(is.null(df)) return(NULL)
    choice <- input$summary_choice
    if(choice == "physical_sum") {
      df %>% filter(subscale == "physical_sum")
    } else if(choice == "mental_sum") {
      df %>% filter(subscale == "mental_sum")
    } else if(choice == "both") {
      df %>% filter(subscale %in% c("physical_sum", "mental_sum"))
    }
  }

  ### ---- get_subdomain_data ----
  get_subdomain_data <- function() {
    df <- current_data()
    if(is.null(df)) return(NULL)
    if(input$subdomain_type == "Physical Subdomains") {
      df %>% filter(subscale %in% c("PF", "RP", "BP", "GH"))
    } else {
      df %>% filter(subscale %in% c("VT", "SF", "RE", "MH"))
    }
  }

  ## ---- Main Plotting Function ----

  render_custom_plot <- function(prefix, data_func, group_input_value) {
    data_filtered <- data_func()
    req(data_filtered)

    # Compute counts for each subscale used in this plot
    facet_counts <- data_filtered %>%
      dplyr::filter(!is.na(score)) %>%
      dplyr::group_by(subscale) %>%
      dplyr::summarise(n = n(), .groups = "drop")

    #Define the group variable and whether a group is selected
    group_var <- input[[paste0(prefix, "_group")]]
    group_selected <- (group_var != "none")


    plot_type    <- input[[paste0(prefix, "_plot_type")]]
    alpha_val    <- input[[paste0(prefix, "_alpha")]]
    plot_title   <- input[[paste0(prefix, "_title")]]
    x_label      <- input[[paste0(prefix, "_xlabel")]]
    y_label      <- input[[paste0(prefix, "_ylabel")]]
    theme_choice <- input[[paste0(prefix, "_theme")]]
    base_size_val<- input[[paste0(prefix, "_base_size")]]

    if (plot_type == "none") return(NULL)

    # Coordinate flip toggles (both tabs)
    if(prefix == "summary"){
      do_flip <- if("summary_coord_flip" %in% names(input)) input$summary_coord_flip else FALSE
    } else if(prefix == "subdomain"){
      do_flip <- if("subdomain_coord_flip" %in% names(input)) input$subdomain_coord_flip else FALSE
    } else {
      do_flip <- FALSE
    }

    # For the Subdomain tab, default facet_subscale to TRUE if not provided.
    facet_subscale <- if(paste0(prefix, "_facet_subscale") %in% names(input)) {
      input[[paste0(prefix, "_facet_subscale")]]
    } else {
      (prefix == "subdomain")
    }
    if(prefix == "summary" && input$summary_choice != "both") facet_subscale <- FALSE

    lang_labels <- if(input$language == "Norwegian") norwegian_labels else english_labels

    custom_labeller <- function(labels) {
      # Check if the facet variable "subscale" is among the labels
      if ("subscale" %in% names(labels)) {
        # For each value of subscale, lookup the language-specific label and append the count
        labels$subscale <- sapply(labels$subscale, function(x) {
          label <- lang_labels[[x]]
          count <- facet_counts$n[facet_counts$subscale == x]
          if (length(count) == 0) count <- 0
          paste0(label, " (n = ", count, ")")
        })
      }
      return(labels)
    }

    # For trend plots, define show_ci with a default FALSE (especially in subdomain where it's not provided)
    show_ci <- if(paste0(prefix, "_show_ci") %in% names(input)) input[[paste0(prefix, "_show_ci")]] else FALSE

    ### ---- Build ggplot: Plot Types ----

    #### ---- Trend lines ----

    gg <- NULL
    if (plot_type == "trend") {
      xvar <- input[[paste0(prefix, "_xvar")]]
      if(!is.numeric(data_filtered[[xvar]])) {
        data_filtered <- data_filtered %>% mutate(.xvar_numeric = as.numeric(factor(.data[[xvar]])))
        xvar_plot <- ".xvar_numeric"
        x_levels  <- levels(factor(data_filtered[[xvar]]))
      } else {
        xvar_plot <- xvar
      }
      gg <- if(group_selected) {
        ggplot(data_filtered, aes(x = .data[[xvar_plot]], y = score, color = .data[[group_var]])) +
          facet_wrap(~ subscale, labeller = custom_labeller)
      } else {
        ggplot(data_filtered, aes(x = .data[[xvar_plot]], y = score)) +
          facet_wrap(~ subscale, labeller = custom_labeller)
      }

      if(trend_type <- input[[paste0(prefix, "_trend_type")]] == "observed") {
        if(group_selected) {
          agg_data <- data_filtered %>%
            group_by(xvar_val = .data[[xvar_plot]], subscale, grp = .data[[group_var]]) %>%
            summarise(mean_score = mean(score), .groups = "drop")
          gg <- gg + geom_line(data = agg_data,
                               aes(x = xvar_val, y = mean_score, group = interaction(subscale, grp), color = grp),
                               linewidth = input[[paste0(prefix, "_line_width")]],
                               alpha = alpha_val) +
            geom_point(data = agg_data,
                       aes(x = xvar_val, y = mean_score, group = interaction(subscale, grp), color = grp),
                       size = input[[paste0(prefix, "_point_size")]],
                       alpha = alpha_val)
        } else {
          agg_data <- data_filtered %>%
            group_by(xvar_val = .data[[xvar_plot]], subscale) %>%
            summarise(mean_score = mean(score), .groups = "drop")
          gg <- gg + geom_line(data = agg_data,
                               aes(x = xvar_val, y = mean_score, group = subscale),
                               linewidth = input[[paste0(prefix, "_line_width")]],
                               alpha = alpha_val) +
            geom_point(data = agg_data,
                       aes(x = xvar_val, y = mean_score, group = subscale),
                       size = input[[paste0(prefix, "_point_size")]],
                       alpha = alpha_val)
        }
      } else {
        gg <- gg + geom_smooth(aes(x = .data[[xvar_plot]], y = score),
                               method = input[[paste0(prefix, "_trend_type")]],
                               se = show_ci,
                               linewidth = input[[paste0(prefix, "_line_width")]],
                               alpha  = if(input[[paste0(prefix, "_trend_type")]] %in% c("lm","loess") && show_ci)
                                 input[[paste0(prefix, "_ci_alpha")]] else alpha_val)
        if(input[[paste0(prefix, "_show_points")]])
          gg <- gg + geom_point(aes(x = .data[[xvar_plot]], y = score),
                                alpha = alpha_val,
                                size  = input[[paste0(prefix, "_point_size")]])
      }
      if(!is.numeric(data_filtered[[xvar]]))
        gg <- gg + scale_x_continuous(breaks = seq_along(x_levels), labels = x_levels)
      if((prefix == "summary" && facet_subscale) ||
         (prefix == "subdomain" && length(unique(data_filtered$subscale)) > 1))
        gg <- gg + facet_wrap(~ subscale, labeller = custom_labeller)
      gg <- add_scales(gg, prefix)

      #### ---- Density plots ----

    } else if (plot_type == "density") {
      gg <- if(group_selected) {
        ggplot(data_filtered, aes(x = score, fill = .data[[group_var]], color = .data[[group_var]])) +
          geom_density(alpha = alpha_val, position = "identity")
      } else {
        ggplot(data_filtered, aes(x = score)) +
          geom_density(alpha = alpha_val, fill = "#A4A9AD", color = "#A4A9AD")
      }
      gg <- gg + facet_wrap(~ subscale, labeller = custom_labeller)
      gg <- add_scales(gg, prefix)

      #### ---- Raincloud plots ----

    } else if (plot_type == "raincloud") {
      if(prefix == "summary" && input$summary_choice != "both") facet_subscale <- FALSE
      if(!facet_subscale) {
        if(group_selected) {
          gg <- ggplot(data_filtered, aes(x = subscale, y = score,
                                          fill = factor(.data[[group_var]]),
                                          color = factor(.data[[group_var]])))
        } else {
          gg <- ggplot(data_filtered, aes(x = subscale, y = score,
                                          fill = I("#A4A9AD"), color = I("#A4A9AD")))
        }
        gg <- gg + scale_x_discrete(labels = lang_labels, expand = c(0, 0.05))
      } else {
        if(group_selected) {
          gg <- ggplot(data_filtered, aes(x = factor(.data[[group_var]]), y = score,
                                          fill = factor(.data[[group_var]]), color = factor(.data[[group_var]])))
        } else {
          gg <- ggplot(data_filtered, aes(x = factor(1), y = score,
                                          fill = I("#A4A9AD"), color = I("#A4A9AD")))
        }
      }
      pos_box <- list(position = ggpp::position_dodgenudge(x = 0.09, width = 0.12))
      point_args <- list(alpha = 0.08, size = 1.6, shape = 21)
      gg <- gg + geom_rain(alpha = alpha_val,
                           point.args = point_args,
                           boxplot.args = list(color = "black", outlier.shape = NA, alpha = alpha_val,
                                               width = if(group_selected) 0.1 else 0.05),
                           boxplot.args.pos = pos_box)
      if(facet_subscale)
        gg <- gg + facet_wrap(~ subscale, labeller = custom_labeller)
      gg <- add_scales(gg, prefix)
      if(do_flip) gg <- gg + coord_flip()

      #### ---- Histograms ----

    } else if (plot_type == "histogram") {
      gg <- if(group_selected) {
        ggplot(data_filtered, aes(x = score, fill = .data[[group_var]], color = .data[[group_var]])) +
          geom_histogram(binwidth = input[[paste0(prefix, "_binwidth")]],
                         color = "white", alpha = alpha_val, position = "dodge") +
          facet_wrap(~ subscale, labeller = custom_labeller)
      } else {
        ggplot(data_filtered, aes(x = score)) +
          geom_histogram(binwidth = input[[paste0(prefix, "_binwidth")]],
                         fill = "#A4A9AD", color = "white", alpha = alpha_val) +
          facet_wrap(~ subscale, labeller = custom_labeller)
      }
      if(group_selected) gg <- add_scales(gg, prefix)

      #### ---- Boxplots ----

    } else if (plot_type == "boxplot") {
      if (!facet_subscale) {
        if (group_selected) {
          gg <- ggplot(data_filtered,
                       aes(x = subscale, y = score,
                           fill  = .data[[group_var]],
                           color = .data[[group_var]])) +
            geom_boxplot(width = 0.5, alpha = alpha_val)
        } else {
          gg <- ggplot(data_filtered,
                       aes(x = subscale, y = score)) +
            geom_boxplot(width = 0.5, alpha = alpha_val,
                         fill  = "#A4A9AD",
                         color = "#A4A9AD")
        }
        gg <- gg + scale_x_discrete(labels = lang_labels)
      } else {
        if (group_selected) {
          gg <- ggplot(data_filtered,
                       aes(x = factor(.data[[group_var]]), y = score,
                           fill  = .data[[group_var]],
                           color = .data[[group_var]])) +
            geom_boxplot(width = 0.3, alpha = alpha_val) +
            facet_wrap(~ subscale, labeller = custom_labeller)
        } else {
          gg <- ggplot(data_filtered,
                       aes(x = factor(1), y = score)) +
            geom_boxplot(width = 0.3, alpha = alpha_val,
                         fill  = "#A4A9AD",
                         color = "#A4A9AD") +
            facet_wrap(~ subscale, labeller = custom_labeller)
        }
      }
      if (do_flip) gg <- gg + coord_flip()
      gg <- add_scales(gg, prefix)

      #### ---- Violin plots ----

    } else if (plot_type == "violin") {
      if (!facet_subscale) {
        if (group_selected) {
          gg <- ggplot(data_filtered,
                       aes(x = subscale, y = score,
                           fill  = .data[[group_var]],
                           color = .data[[group_var]])) +
            geom_violin(alpha = alpha_val)
        } else {
          gg <- ggplot(data_filtered,
                       aes(x = subscale, y = score)) +
            geom_violin(alpha = alpha_val,
                        fill  = "#A4A9AD",
                        color = "#A4A9AD")
        }
        gg <- gg + scale_x_discrete(labels = lang_labels)
      } else {
        if (group_selected) {
          gg <- ggplot(data_filtered,
                       aes(x = factor(.data[[group_var]]), y = score,
                           fill  = .data[[group_var]],
                           color = .data[[group_var]])) +
            geom_violin(alpha = alpha_val) +
            facet_wrap(~ subscale, labeller = custom_labeller)
        } else {
          gg <- ggplot(data_filtered,
                       aes(x = factor(1), y = score)) +
            geom_violin(alpha = alpha_val,
                        fill  = "#A4A9AD",
                        color = "#A4A9AD") +
            facet_wrap(~ subscale, labeller = custom_labeller)
        }
      }
      if (do_flip) gg <- gg + coord_flip()
      gg <- add_scales(gg, prefix)

      #### ---- Jitter ----

    } else if (plot_type == "jitter") {
      if (!facet_subscale) {
        if (group_selected) {
          gg <- ggplot(data_filtered,
                       aes(x = subscale, y = score,
                           fill  = .data[[group_var]],
                           color = .data[[group_var]])) +
            geom_jitter(width = 0.2, alpha = alpha_val)
        } else {
          gg <- ggplot(data_filtered,
                       aes(x = subscale, y = score)) +
            geom_jitter(width = 0.2, alpha = alpha_val)
        }
        gg <- gg + scale_x_discrete(labels = lang_labels)
      } else {
        if (group_selected) {
          gg <- ggplot(data_filtered,
                       aes(x = factor(.data[[group_var]]), y = score,
                           fill  = .data[[group_var]],
                           color = .data[[group_var]])) +
            geom_jitter(width = 0.2, alpha = alpha_val) +
            facet_wrap(~ subscale, labeller = custom_labeller)
        } else {
          gg <- ggplot(data_filtered,
                       aes(x = factor(1), y = score)) +
            geom_jitter(width = 0.2, alpha = alpha_val) +
            facet_wrap(~ subscale, labeller = custom_labeller)
        }
      }
      if (do_flip) gg <- gg + coord_flip()
      gg <- add_scales(gg, prefix)
    }


    # ---- Add group counts to caption ----
    if (group_selected && input[[paste0(prefix, "_group_n_caption")]]) {
      # compute per‐facet, per‐group counts
      group_counts <- data_filtered %>%
        filter(!is.na(score)) %>%
        count(subscale, grp = .data[[group_var]])

      # one neat line per facet: “Subscale Label: G1 (n=…) ; G2 (n=…)”
      caption_lines <- group_counts %>%
        group_by(subscale) %>%
        summarise(
          line = paste0(
            lang_labels[[first(subscale)]], ": ",
            paste0(grp, ", n=", n, collapse = "; ")
          ),
          .groups = "drop"
        ) %>%
        pull(line)

      # collapse with single newlines (no blank line)
      caption_text <- paste(caption_lines, collapse = "\n")
      gg <- gg + labs(caption = caption_text)
    }


    ### ---- Add Labels, Themes, Coordinates, Reference lines ----

    #### ---- Labels ----

    gg <- gg + labs(
      title = plot_title,
      x = if(nchar(x_label) > 0) x_label else waiver(),
      y = if(nchar(y_label) > 0) y_label else waiver(),
      color = if(group_selected) input[[paste0(prefix, "_legend_title")]] else waiver(),
      fill = if(group_selected) input[[paste0(prefix, "_legend_title")]] else waiver()
    )

    #### ---- Themes ----
    if (theme_choice == "nice") {
      gg <- gg + theme_bw(base_size = base_size_val) +
        theme(panel.grid = element_blank(),
              strip.background = element_blank(),
              strip.text = element_text(size = base_size_val))
    } else if (theme_choice == "bw") {
      gg <- gg + theme_bw(base_size = base_size_val)
    } else if (theme_choice == "classic") {
      gg <- gg + theme_classic(base_size = base_size_val)
    } else if (theme_choice == "minimal") {
      gg <- gg + theme_minimal(base_size = base_size_val)
    } else if (theme_choice == "gray") {
      gg <- gg + theme_gray(base_size = base_size_val)
    } else if (theme_choice == "void") {
      gg <- gg + theme_void()
    }

    if(!group_selected) gg <- gg + theme(legend.position = "none")

    #### ---- Reference lines ----

    if (prefix == "summary") {
      gg <- add_reference_lines(
        gg = gg,
        data = data_filtered,
        plot_type = plot_type,
        group_var = group_var,
        group_selected = group_selected,
        facet_subscale = input$summary_facet_subscale,  # Pass the checkbox value here
        ref_stat = input$ref_statistic,
        ref_show_overall = input$ref_show_overall,
        ref_overall_color = input$ref_overall_color,
        ref_overall_linewidth = input$ref_overall_linewidth,
        ref_overall_linetype = input$ref_overall_linetype,
        ref_show_groups = input$ref_show_groups,
        ref_group_linewidth = input$ref_group_linewidth,
        ref_group_linetype = input$ref_group_linetype,
        ref_custom = input$ref_custom,
        ref_custom_color = input$ref_custom_color,
        ref_custom_linewidth = input$ref_custom_linewidth,
        ref_custom_linetype = input$ref_custom_linetype,
        ref_custom_values = input$ref_custom_values
      )
    } else if (prefix == "subdomain") {
      gg <- add_reference_lines(
        gg = gg,
        data = data_filtered,
        plot_type = plot_type,
        group_var = group_var,
        group_selected = group_selected,
        facet_subscale = input$subdomain_facet_subscale,  # Pass the checkbox value here
        ref_stat = input$ref_statistic_subdomain,
        ref_show_overall = input$ref_show_overall_subdomain,
        ref_overall_color = input$ref_overall_color_subdomain,
        ref_overall_linewidth = input$ref_overall_linewidth_subdomain,
        ref_overall_linetype = input$ref_overall_linetype_subdomain,
        ref_show_groups = input$ref_show_groups_subdomain,
        ref_group_linewidth = input$ref_group_linewidth_subdomain,
        ref_group_linetype = input$ref_group_linetype_subdomain,
        ref_custom = input$ref_custom_subdomain,
        ref_custom_color = input$ref_custom_color_subdomain,
        ref_custom_linewidth = input$ref_custom_linewidth_subdomain,
        ref_custom_linetype = input$ref_custom_linetype_subdomain,
        ref_custom_values = input$ref_custom_values_subdomain
      )
    }

    #### ---- Coordinates -----

    xlims <- if(nchar(input[[paste0(prefix, "_xlim")]]) > 0)
      as.numeric(unlist(strsplit(input[[paste0(prefix, "_xlim")]], ","))) else NULL
    ylims <- if(nchar(input[[paste0(prefix, "_ylim")]]) > 0)
      as.numeric(unlist(strsplit(input[[paste0(prefix, "_ylim")]], ","))) else NULL
    if(!is.null(xlims) && length(xlims) == 2 && !any(is.na(xlims)) &&
       !is.null(ylims) && length(ylims) == 2 && !any(is.na(ylims))) {
      gg <- gg + coord_cartesian(xlim = xlims, ylim = ylims)
    } else if(!is.null(xlims) && length(xlims) == 2 && !any(is.na(xlims))) {
      gg <- gg + coord_cartesian(xlim = xlims)
    } else if(!is.null(ylims) && length(ylims) == 2 && !any(is.na(ylims))) {
      gg <- gg + coord_cartesian(ylim = ylims)
    }

    return(gg)
  }


  ## ---- Plot Outputs ----
  output$summary_plot <- renderPlot({
    render_custom_plot(prefix = "summary", data_func = get_summary_data, group_input_value = input$summary_group)
  })

  output$subdomain_plot <- renderPlot({
    render_custom_plot(prefix = "subdomain", data_func = get_subdomain_data, group_input_value = input$subdomain_group)
  })

  ## ---- Download Handlers ----
  observeEvent(input$download_btn, {
    showModal(modalDialog(
      title = "Download Figure",
      radioButtons("download_format", "Select File Format:",
                   choices = c("PDF", "PNG"), selected = "PDF"),
      conditionalPanel(
        condition = "input.download_format == 'PNG'",
        selectInput("download_dpi", "Select DPI:",
                    choices = c(300, 600, 900, 1200, 1500, 1800, 2100), selected = 1500)
      ),
      numericInput("download_width", "Width (inches):", value = 10, min = 1),
      numericInput("download_height", "Height (inches):", value = 6, min = 1),
      footer = tagList(
        modalButton("Cancel"),
        downloadButton("download_plot", "Download Figure")
      ),
      helpText(tags$small("Set 'Width' and 'Height' to get the appropriate size of the figure.
                            As a default, for non-paneled figures, 'Width' = 10, and 'Height' = 6 may be appropriate.
                            For multipaneled figures, a sensible default may be 'Width' = 14 and 'Height' = 8 (depending on the number of panels/group comparisons)")),
      easyClose = TRUE
    ))
  })

  get_current_plot <- reactive({
    if (input$navbar %in% c("summary", "subdomain")) {
      if (input$navbar == "summary") {
        render_custom_plot("summary", get_summary_data, input$summary_group)
      } else {
        render_custom_plot("subdomain", get_subdomain_data, input$subdomain_group)
      }
    } else {
      NULL
    }
  })

  output$download_plot <- downloadHandler(
    filename = function() {
      ext <- if (input$download_format == "PDF") "pdf" else "png"
      paste0("Figure.", ext)
    },
    content = function(file) {
      plot_obj <- get_current_plot()
      req(plot_obj)
      width  <- input$download_width
      height <- input$download_height
      if (input$download_format == "PDF") {
        pdf(file, width = width, height = height)
        print(plot_obj)
        dev.off()
      } else {
        png(file, width = width, height = height, units = "in", res = as.numeric(input$download_dpi))
        print(plot_obj)
        dev.off()
      }
    }
  )
}


# ===== Launch app =====
shinyApp(ui =  ui, server = server)
