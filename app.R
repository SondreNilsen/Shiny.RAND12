# load all of our package code without installing

if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".")
} else {
  stop("pkgload is required to load the app’s package code")
}

# call the function that launches the Shiny app

runRAND12App()
