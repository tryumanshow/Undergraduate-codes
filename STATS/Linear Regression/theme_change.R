
# Install Packages
if ("remotes" %in% installed.packages()) {
  print("'remotes' package is already installed.")
} else (
  install.packages("remotes")
)

remotes::install_github("anthonynorth/rscodeio")

# Apply the theme
rscodeio::install_theme()
