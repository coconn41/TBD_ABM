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
if(cluster==F){
for (pkg in packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}
library(parallel)
}
if(cluster==T){
  .libPaths("/n/home08/collinoconnor/R/TBD_ABM/Library")
  
  cat("R version:", R.version.string, "\n")
  cat(".libPaths():\n")
  print(.libPaths())
  cat("rlang path:", system.file("libs", "rlang.so", package = "rlang"), "\n")
  for(pkg in packages){
    library(pkg, character.only = TRUE,lib.loc = paste0(getwd(),'/Library/'))
  }
}