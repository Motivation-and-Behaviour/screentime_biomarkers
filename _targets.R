## Load packages
source("./packages.R")

## Load R files
tar_source()

# Use parallel processing where possible
tar_option_set(
  controller = crew_controller_local(workers = 3, seconds_idle = 15)
)

lsac_path <- Sys.getenv("LSAC_PATH")

tar_plan(
  tar_file_read(
    biomarkers_data,
    file.path(
      lsac_path,
      "Special Purpose Dataset - CHP (Biomarkers) General Release/SPSS/lsacgrcp.sav"
    ), # nolint
    read_biomarkers_data(!!.x)
  ),
  tar_files_input(
    waves,
    c(
      file.path(
        lsac_path,
        "9.1_C2 General Release/Survey data/SPSS/lsacgrb2.sav"
      ),
      file.path(
        lsac_path,
        "9.1_C2 General Release/Survey data/SPSS/lsacgrb4.sav"
      ),
      file.path(
        lsac_path,
        "9.1_C2 General Release/Survey data/SPSS/lsacgrb6.sav"
      ),
      file.path(
        lsac_path,
        "9.1_C2 General Release/Survey data/SPSS/lsacgrb8.sav"
      ),
      file.path(
        lsac_path,
        "9.1_C2 General Release/Survey data/SPSS/lsacgrb10.sav"
      )
    )
  ),
  tar_target(
    waves_data,
    read_waves_data(waves),
    pattern = map(waves), iteration = "list"
  ),
  tar_target(
    waves_joined,
    dplyr::bind_rows(waves_data),
    pattern = map(waves_data)
  ),
  tar_render(manuscript, "doc/manuscript.Rmd")
)
