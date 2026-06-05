## Load packages
source("./packages.R")

## Load R files
tar_source()

# Use parallel processing where possible
tar_option_set(
  controller = crew_controller_local(
    workers = min(parallel::detectCores() - 2, 28),
    seconds_idle = 15
  )
)
dotenv::load_dot_env()
lsac_path <- Sys.getenv("LSAC_PATH")

outcome_variables <- tribble(
  ~variable              , ~bloods ,
  # Main outcomes
  "cardio_index_w6.5"    , TRUE    ,
  "ApoBA1_ratio_w6.5"    , TRUE    ,
  "glycoprotein_w6.5"    , TRUE    ,
  "phospholipids_w6.5"   , TRUE    ,
  # Additional outcomes
  "vo2_w6.5"             , FALSE   ,
  "waistcm_w6.5"         , FALSE   ,
  "waist2height_w6.5"    , FALSE   ,
  "bmiz_w6.5"            , FALSE   ,
  "bodyfat_w6.5"         , FALSE   ,
  "bpsysamp_w6.5"        , FALSE   ,
  "pulsepressamp_w6.5"   , FALSE   ,
  "bpsysz_w6.5"          , FALSE   ,
  "bpdiaz_w6.5"          , FALSE   ,
  "trigly_w6.5"          , TRUE    ,
  "cholesttotal_w6.5"    , TRUE    ,
  "cholesttotalhdl_w6.5" , TRUE    ,
  "cholestnonhdl_w6.5"   , TRUE    ,
  "glucose_w6.5"         , TRUE
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
  tar_target(model_predictions, make_model_predictions(model, transformed_data)),
  # Sensitivity analysis 1: observed Wave 3 screen time as exposure
  tar_target(model_w3, fit_observed_w3(transformed_data, variable, bloods)),
  tar_target(
    model_w3_gt,
    make_lm_gt(model_w3, variable, c(st_totalz_w3 = "Observed Wave 3 Screen Time"))
  ),
  tar_target(model_w3_df, tidy_lm_pair(model_w3)),
  # Sensitivity analysis 2: mixed-model (lme4) trajectory, two-stage
  tar_target(
    model_lmm,
    fit_lmm_outcome(st_trajectory_lmm, transformed_data, variable, bloods)
  ),
  tar_target(
    model_lmm_gt,
    make_lm_gt(model_lmm, variable, c(
      blup_intercept = "Screen Time Intercept (mixed model)",
      blup_slope = "Screen Time Slope (mixed model)"
    ))
  ),
  tar_target(model_lmm_df, tidy_lm_pair(model_lmm)),
  # Sensitivity analysis 3a: definition-invariant trajectory (TV + games only)
  tar_target(
    model_consistent,
    fit_lgcm(transformed_data, variable, bloods, st_prefix = "st_consistentz")
  ),
  tar_target(
    model_consistent_gt,
    make_lgcm_gt(model_consistent, variable, model_fit_measures)
  ),
  tar_target(model_consistent_df, make_model_dfs(model_consistent, model_fit_measures)),
  # Sensitivity analysis 3b: period-specific exposures (Waves 3-4 vs 5-6)
  tar_target(
    model_early,
    fit_observed_w3(transformed_data, variable, bloods, exposure = "st_earlyz")
  ),
  tar_target(
    model_early_gt,
    make_lm_gt(model_early, variable, c(st_earlyz = "Waves 3-4 Mean Screen Time"))
  ),
  tar_target(model_early_df, tidy_lm_pair(model_early)),
  tar_target(
    model_late,
    fit_observed_w3(transformed_data, variable, bloods, exposure = "st_latez")
  ),
  tar_target(
    model_late_gt,
    make_lm_gt(model_late, variable, c(st_latez = "Waves 5-6 Mean Screen Time"))
  ),
  tar_target(model_late_df, tidy_lm_pair(model_late))
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
    pattern = map(waves),
    iteration = "list"
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
      waves_joined,
      biomarkers_data,
      checkpoint_only = FALSE,
      no_outliers = FALSE
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
  # Sensitivity analysis 2 (stage 1): fit the mixed-model trajectory once
  tar_target(st_trajectory_lmm, fit_st_trajectory_lmm(transformed_data)),
  # Sensitivity analysis 3: mean screen time across waves (visual check for a
  # discontinuity at the Wave 4-5 exposure-definition boundary)
  tar_target(st_wave_plot, plot_st_wave_trajectory(transformed_data)),
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
  # Sensitivity analysis outputs
  tar_combine(
    w3_table,
    model_builder[["model_w3_gt"]],
    command = make_sensitivity_table(
      !!!.x,
      caption = paste(
        "Sensitivity analysis 1. Associations between observed Wave 3",
        "screen time and health outcomes (observed exposure in place of the",
        "latent intercept)."
      )
    )
  ),
  tar_combine(
    lmm_table,
    model_builder[["model_lmm_gt"]],
    command = make_sensitivity_table(
      !!!.x,
      caption = paste(
        "Sensitivity analysis 2. Associations between mixed-model screen-time",
        "trajectories (subject-specific intercept and slope) and health outcomes."
      )
    )
  ),
  tar_combine(
    w3_dfs,
    model_builder[["model_w3_df"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_combine(
    lmm_dfs,
    model_builder[["model_lmm_df"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_target(
    w3_diagnostic_table,
    make_lm_diagnostic_table(w3_dfs, "outputs/sensitivity_w3_tables.csv"),
    format = "file"
  ),
  tar_target(
    lmm_diagnostic_table,
    make_lm_diagnostic_table(lmm_dfs, "outputs/sensitivity_lmm_tables.csv"),
    format = "file"
  ),
  # Sensitivity analysis 3: exposure definition change across waves
  tar_combine(
    consistent_table,
    model_builder[["model_consistent_gt"]],
    command = make_outcomes_table(
      !!!.x,
      caption = paste(
        "Sensitivity analysis 3a. Associations between screen-time trajectories",
        "and health outcomes using only the definition-invariant components",
        "(television and electronic games)."
      )
    )
  ),
  tar_combine(
    consistent_dfs,
    model_builder[["model_consistent_df"]]
  ),
  tar_target(
    consistent_diagnostic_table,
    make_diagnostic_table(consistent_dfs, "outputs/sensitivity_consistent_tables.csv"),
    format = "file"
  ),
  tar_combine(
    early_table,
    model_builder[["model_early_gt"]],
    command = make_sensitivity_table(
      !!!.x,
      caption = paste(
        "Sensitivity analysis 3b. Associations between Waves 3-4 average screen",
        "time (original computer-item definition) and health outcomes."
      )
    )
  ),
  tar_combine(
    late_table,
    model_builder[["model_late_gt"]],
    command = make_sensitivity_table(
      !!!.x,
      caption = paste(
        "Sensitivity analysis 3b. Associations between Waves 5-6 average screen",
        "time (revised computer-item definition) and health outcomes."
      )
    )
  ),
  tar_combine(
    early_dfs,
    model_builder[["model_early_df"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_combine(
    late_dfs,
    model_builder[["model_late_df"]],
    command = dplyr::bind_rows(!!!.x)
  ),
  tar_target(
    early_diagnostic_table,
    make_lm_diagnostic_table(early_dfs, "outputs/sensitivity_early_tables.csv"),
    format = "file"
  ),
  tar_target(
    late_diagnostic_table,
    make_lm_diagnostic_table(late_dfs, "outputs/sensitivity_late_tables.csv"),
    format = "file"
  ),
  tar_render(results_section, "doc/Results.Rmd"),
  tar_render(sensitivity_section, "doc/Sensitivity.Rmd")
)
