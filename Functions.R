# FUNCTIONS

# key_packages_fn - These are packages I tend to work with often. We are installing and setting up collectivly.
key_packages_fn <- function() {
  # List of packages
key_packages <- c("tidyverse", "readr", "dplyr", "ggplot2", "rmarkdown", "xml2", "plotly", "shiny", "ggthemes", "janitor", "styler", "arrow", "ggsci", "rstudioapi", "ggridges", "patchwork", "lvplot")
  # Check if each package is installed, and install it if not
  for (pkg in key_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg)
    }
    # Load the package
    library(pkg, character.only = TRUE)
  }
}

# Call the function
key_packages_fn()

getwd()
