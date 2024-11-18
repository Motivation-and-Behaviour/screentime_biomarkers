## Load packages
source("./packages.R")

## Load R files
lapply(list.files("./R", full.names = TRUE), source)

tar_plan(
  tar_render(manuscript, "doc/manuscript.Rmd")
)
