packages <- c(
  "FedData",
  "terra",
  "sf",
  "tidyverse",
  "units",
  "landscapemetrics",
  "readxl",
  "truncnorm",
  "foreach",
  "suncalc",
  "igraph",
  "furrr"
)

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
library(parallel)
