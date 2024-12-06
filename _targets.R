## Load packages
source("./packages.R")

## Load R files
tar_source()

# Use parallel processing where possible
tar_option_set(
  controller = crew_controller_local(
    workers = min(parallel::detectCores() - 2, 20), seconds_idle = 15
  )
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
  "vo2_w6.5", FALSE,
  "waistcm_w6.5", FALSE,
  "waist2height_w6.5", FALSE,
  "bmiz_w6.5", FALSE,
  "bodyfat_w6.5", FALSE,
  "bpsysamp_w6.5", FALSE,
  "pulsepressamp_w6.5", FALSE,
  "bpsysz_w6.5", FALSE,
  "bpdiaz_w6.5", FALSE,
  "trigly_w6.5", TRUE,
  "cholesttotal_w6.5", TRUE,
  "cholesttotalhdl_w6.5", TRUE,
  "cholestnonhdl_w6.5", TRUE,
  "glucose_w6.5", TRUE
)

model_builder <- tar_map(
  values = outcome_variables,
  names = "variable",
  unlist = FALSE,
  tar_target(model, fit_lgcm(transformed_data, variable, bloods)),
  tar_target(model_fit_measures, get_measures(model)),
  tar_target(model_df, make_model_dfs(model, model_fit_measures)),
  tar_target(model_table_gt, make_lgcm_gt(model, variable, model_fit_measures)),
  tar_target(
    model_table_gt_supps,
    make_lgcm_gt(model, variable, model_fit_measures, main = FALSE)
  ),
  tar_target(model_predictions, make_model_predictions(model, transformed_data))
)

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
      checkpoint_only = FALSE, no_outliers = FALSE
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
  model_builder,
  tar_combine(
    fit_measures,
    model_builder[["model_fit_measures"]],
    command = dplyr::bind_rows(!!!.x, .id = "variable") |>
      dplyr::mutate(
        model_name = stringr::str_remove(variable, "model_fit_measures_"),
        across(where(is.numeric), round, 2)
      ) |>
      dplyr::select(model_name, everything(), -variable)
  ),
  tar_combine(
    model_dfs,
    model_builder[["model_df"]]
  ),
  tar_target(
    diagnostic_table,
    make_diagnostic_table(model_dfs),
    format = "file"
  ),
  tar_target(table1, make_table1(scored_data)),
  tar_target(
    table1_file,
    save_table(table1, "doc/table1.docx"),
    format = "file"
  ),
  tar_combine(
    outcomes_table,
    model_builder[["model_table_gt"]],
    command = make_outcomes_table(!!!.x)
  ),
  tar_combine(
    model_predictions,
    model_builder[["model_predictions"]]
    ),
  tar_target(prediction_plot, plot_predictions(model_predictions)),
  tar_target(
    outcomes_table_file,
    save_table(outcomes_table, "doc/outcomes_table.docx"),
    format = "file"
  ),
  tar_render(manuscript, "doc/manuscript.Rmd")
)