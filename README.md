# Shiny.RAND12

**RAND-12 Data Visualization Shiny App**

------------------------------------------------------------------------

## Table of Contents

-   [Description](#description)
-   [Installation](#installation)
-   [Usage](#usage)
-   [Features](#features)
-   [Dependencies](#dependencies)
-   [Citation](#citation)
-   [Issues & Feature Requests](#issues--feature-requests)
-   [License](#license)
-   [Contact](#contact)

------------------------------------------------------------------------

## Description

**Shiny.RAND12** is an R package wrapping a Shiny application for simulating or uploading RAND-12 health survey data and generating publication-quality plots of summary and subdomain scores. It offers flexible plot types, language toggles (English/Norwegian), and one-click figure downloads (PDF/PNG).

------------------------------------------------------------------------

## Installation

Install the development version from GitHub:

``` r
install.packages("remotes")         # if you don’t already have it
remotes::install_github("YOUR_USERNAME/Shiny.RAND12")
```

------------------------------------------------------------------------

## Usage

``` r
library(Shiny.RAND12)
runRAND12App()
```

------------------------------------------------------------------------

## Features

### 📂 Data Upload & Preview

-   Built-in simulated RAND-12 data to play around with
-   Import your own data in SPSS (.sav), Stata (.dta), Excel (.xls/.xlsx), or CSV formats\
-   Automatic wide-to-long conversion and factor labeling

### 📊 Summary Scores (PSC-12 & MSC-12) & Subdomain Profiles

Visualize the PSC-12, MSC-12, and Subdomine Profiles with: - Histogram, density, boxplot, violin, jitter, raincloud, trend-line (observed, lm, loess) - Compare groups by any categorical variables - Add mean/median reference lines (grand and by group) - Coordinate flips, custom axis limits

### 🎨 Theming & Localization

-   Select from multiple ggplot2 themes and color palettes (qualitative & sequential)\
-   Toggle facet labels and response levels between English and Norwegian\
-   Add facet by group counts to caption
-   Set alpha (transparency), text sizes and more

### 📥 Download Publication-Quality Figures

-   One-click export to PDF or PNG at custom dimensions and resolution

------------------------------------------------------------------------

## Dependencies

The package imports these core R packages: shiny, shinythemes, shinyWidgets, ggplot2, dplyr, tidyr, colourpicker, RColorBrewer, ggsci, haven, readxl, readr, DT, ggrain

These are declared in DESCRIPTION under `Imports`: and loaded quietly in the app.

------------------------------------------------------------------------

## Citation 

If you use **Shiny.RAND12** in your work, please cite it as:

> Nilsen, S. A. (2025). *Shiny.RAND12: Interactive RAND-12 Data Visualizer* (Version 0.1.0) [Software]. 

### BibTeX

```bibtex
@software{Nilsen2025ShinyEQ5D,
  author       = {Nilsen, Sondre Aasen},
  title        = {{Shiny.RAND12: Interactive RAND-12 Data Visualizer}},
  version      = {0.1.0},
  year         = {2025},
  publisher    = {Zenodo},
  doi          = {...},
  url          = {...}
}
```
------------------------------------------------------------------------
## Issues & Feature Requests

If you encounter a bug or would like to request a new feature, please open an issue on GitHub:

1. Go to the [Issues tab of this repository](https://github.com/SondreNilsen/Shiny.RAND12/issues).  
2. Click **New issue**.  
3. Choose “Bug report” or “Feature request,” fill in the template, and submit.

------------------------------------------------------------------------


## License

This project is licensed under the **MIT License** – see the [LICENSE](LICENSE) file for details.

------------------------------------------------------------------------

*Created by Sondre Aasen Nilsen, on Behalf of Center for Patient-Reported Data, Haukeland University Hospital, Bergen, Norway*
*Questions or feedback? Email: sondre.aa.nilsen@gmail.com*
