## Load packages
source("./packages.R")

## Load R files
tar_source()

# Use parallel processing where possible
tar_option_set(
  controller = crew_controller_local(workers = 3, seconds_idle = 15)
)

lsac_path <- Sys.getenv("LSAC_PATH")

outcome_variables <- tribble(
  ~variable, ~bloods,
  # Main outcomes
  "cardio_index_w6.5", TRUE,
  "ApoBA1_ratio_w6.5", TRUE,
  "glycoprotein_w6.5", TRUE,
  "phospholipids_w6.5", TRUE,
  # Additional outcomes
  "vo2_w6.5", TRUE,
  "waistcm_w6.5", TRUE,
  "waist2height_w6.5", TRUE,
  "bmiz_w6.5", TRUE,
  "bodyfat_w6.5", TRUE,
  "bpsysamp_w6.5", TRUE,
  "pulsepressamp_w6.5", TRUE,
  "bpsysz_w6.5", TRUE,
  "bpdiaz_w6.5", TRUE,
  "trigly_w6.5", TRUE,
  "cholesttotal_w6.5", TRUE,
  "cholesttotalhdl_w6.5", TRUE,
  "cholestnonhdl_w6.5", TRUE,
  "glucose_w6.5", TRUE
)

model_vector <- paste0("model_", outcome_variables$variable) |>
  paste(collapse = ", ")
fit_measures_cmd <- glue::glue("fit_measures_table({model_vector})")

list(
  tar_file_read(
    biomarkers_data,
    file.path(
      lsac_path,
      "Special Purpose Dataset - CHP (Biomarkers) General Release/SPSS/lsacgrcp.sav" # nolint
    ),
    read_biomarkers_data(!!.x)
  ),
  tar_files_input(
    waves,
    c(
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
  # supporting data
  tar_file_read(
    bio_ref_data,
    file.path("sources/biomarker_reference.csv"),
    read_norm_data(!!.x)
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
  tar_target(
    df_clean,
    clean_data(waves_joined, biomarkers_data),
  ),
  tar_target(
    df_clean_alt, # This is the sensitivity dataset
    clean_data(
      waves_joined, biomarkers_data,
      checkpoint_only = FALSE, remove_outliers = FALSE
    ),
  ),
  tar_target(
    scored_data,
    score_data(df_clean)
  ),
  tar_target(
    transformed_data,
    transform_data(scored_data, bio_ref_data),
  ),
  tar_target(
    transformed_data_no_filter,
    transform_data(scored_data, bio_ref_data, filter_valid = FALSE)
  ),
  tar_map(
    values = outcome_variables,
    names = "variable",
    tar_target(model, fit_lgcm(transformed_data, variable, bloods))
  ),
tar_target(
  all_models,
  list(
      model_cardio_index_w6.5 = model_cardio_index_w6.5,
      glycoprotein_w6.5 = model_glycoprotein_w6.5,
      phospholipids_w6.5 = model_phospholipids_w6.5,
      vo2_w6.5 = model_vo2_w6.5,
      waistcm_w6.5 = model_waistcm_w6.5,
      waist2height_w6.5 = model_waist2height_w6.5,
      bmiz_w6.5 = model_bmiz_w6.5,
      bodyfat_w6.5 = model_bodyfat_w6.5,
      bpsysamp_w6.5 = model_bpsysamp_w6.5,
      pulsepressamp_w6.5 = model_pulsepressamp_w6.5,
      bpsysz_w6.5 = model_bpsysz_w6.5,
      bpdiaz_w6.5 = model_bpdiaz_w6.5,
      trigly_w6.5 = model_trigly_w6.5,
      cholesttotal_w6.5 = model_cholesttotal_w6.5,
      cholesttotalhdl_w6.5 = model_cholesttotalhdl_w6.5,
      cholestnonhdl_w6.5 = model_cholestnonhdl_w6.5,
      glucose_w6.5 = model_glucose_w6.5
    )
  ),
  tar_target(
    fit_measures,
    fit_measures_table(all_models)
  ),
  tar_render(manuscript, "doc/manuscript.Rmd")
)
