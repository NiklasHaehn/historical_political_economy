library(tidyverse)
library(fixest)
library(HonestDiD)
library(did)

out_dir <- "data/fmt"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Helper functions --------------------------------------------------------

run_event_study <- function(data, outcome, treatment, weight_var = NULL) {
  fml <- as.formula(glue::glue(
    "{outcome} ~ {treatment} + i(event_time, {treatment}, ref = -1) | event_id + event_time"
  ))
  wt  <- if (!is.null(weight_var)) as.formula(paste0("~", weight_var)) else NULL
  tryCatch(
    feols(fml, cluster = ~event_id, weights = wt, data = data),
    error = \(e) { message("  [event study error] ", conditionMessage(e)); NULL }
  )
}

extract_event_study_coefs <- function(mod, label) {
  if (is.null(mod)) return(tibble())
  ci <- confint(mod) |> as.data.frame() |> rownames_to_column("term") |>
    as_tibble() |> rename(ci_low = `2.5 %`, ci_high = `97.5 %`)
  tibble(term = names(coef(mod)), att = unname(coef(mod))) |>
    filter(str_detect(term, "^event_time::")) |>
    mutate(event_time = as.integer(str_extract(term, "-?\\d+"))) |>
    left_join(ci, by = "term") |>
    transmute(group = label, event_time, att, ci_low, ci_high) |>
    bind_rows(tibble(group = label, event_time = -1L, att = 0, ci_low = 0, ci_high = 0)) |>
    arrange(event_time)
}

run_pretrend_test <- function(mod, label) {
  if (is.null(mod)) return(tibble(group = label, f_stat = NA_real_, df1 = NA_integer_, df2 = NA_integer_, p_value = NA_real_))
  coef_names <- names(coef(mod))
  pre_names  <- coef_names[str_detect(coef_names, "event_time::-[0-9]+:")]
  if (length(pre_names) == 0)
    return(tibble(group = label, f_stat = NA_real_, df1 = NA_integer_, df2 = NA_integer_, p_value = NA_real_))
  wt <- wald(mod, keep = pre_names)
  tibble(group = label, f_stat = wt$stat, df1 = wt$df1, df2 = wt$df2, p_value = wt$p)
}

run_pooled_did <- function(data, outcome, treatment, weight_var = NULL) {
  d   <- data |> mutate(post = as.integer(event_time >= 0))
  fml <- as.formula(glue::glue("{outcome} ~ post * {treatment} | event_id + event_time"))
  wt  <- if (!is.null(weight_var)) as.formula(paste0("~", weight_var)) else NULL
  tryCatch(
    feols(fml, cluster = ~event_id, weights = wt, data = d),
    error = \(e) { message("  [pooled did error] ", conditionMessage(e)); NULL }
  )
}

extract_pooled_att <- function(mod, treatment, label) {
  empty <- tibble(group = label, att = NA_real_, se = NA_real_, ci_low = NA_real_, ci_high = NA_real_, p_value = NA_real_)
  if (is.null(mod)) return(empty)
  term_name <- glue::glue("post:{treatment}")
  coefs <- coef(mod)
  if (!term_name %in% names(coefs)) return(empty)
  b  <- coefs[[term_name]]
  se <- sqrt(diag(vcov(mod, "cluster"))[[term_name]])
  tibble(group = label, att = b, se = se,
         ci_low = b - 1.96 * se, ci_high = b + 1.96 * se,
         p_value = 2 * pnorm(-abs(b / se)))
}

extract_es_coefs <- function(mod, treatment_var) {
  coef_names <- names(coef(mod))
  es_names   <- coef_names[str_detect(coef_names,
                  glue::glue("^event_time::-?[0-9]+:{treatment_var}$"))]
  if (length(es_names) == 0) return(NULL)
  event_times <- as.integer(str_extract(es_names, "-?[0-9]+"))
  ord <- order(event_times)
  sg  <- vcov(mod, "cluster")[es_names[ord], es_names[ord]]
  list(
    betahat        = unname(coef(mod)[es_names[ord]]),
    sigma          = unname(as.matrix(sg)),
    event_times    = event_times[ord],
    numPrePeriods  = sum(event_times < -1),
    numPostPeriods = sum(event_times >= 0)
  )
}

run_honest_did <- function(mod, treatment_var, outcome_label, out_path) {
  if (is.null(mod)) return(invisible(NULL))
  es <- extract_es_coefs(mod, treatment_var)
  if (is.null(es) || es$numPrePeriods < 2 || es$numPostPeriods < 1) {
    cat(glue::glue("[{outcome_label}] Insufficient periods for HonestDiD — skipped.\n"))
    return(invisible(NULL))
  }
  sensitivity <- tryCatch(
    HonestDiD::createSensitivityResults_relativeMagnitudes(
      betahat        = es$betahat,
      sigma          = es$sigma,
      numPrePeriods  = es$numPrePeriods,
      numPostPeriods = es$numPostPeriods,
      Mbarvec        = seq(0.5, 2, by = 0.5),
      alpha          = 0.05
    ),
    error = \(e) {
      cat(glue::glue("[{outcome_label}] HonestDiD error: {conditionMessage(e)}\n"))
      NULL
    }
  )
  if (is.null(sensitivity)) return(invisible(NULL))
  original_cs <- tryCatch(
    HonestDiD::constructOriginalCS(
      betahat        = es$betahat,
      sigma          = es$sigma,
      numPrePeriods  = es$numPrePeriods,
      numPostPeriods = es$numPostPeriods,
      alpha          = 0.05
    ),
    error = \(e) NULL
  )
  if (is.null(original_cs)) return(invisible(NULL))
  p <- HonestDiD::createSensitivityPlot_relativeMagnitudes(sensitivity, original_cs) +
    ggplot2::labs(
      title    = glue::glue("HonestDiD Sensitivity: {outcome_label}"),
      subtitle = "Robustness to violations of parallel trends (relative magnitudes, Rambachan & Roth 2023)"
    ) +
    ggplot2::theme_minimal(base_size = 11)
  ggplot2::ggsave(out_path, p, width = 8, height = 5, dpi = 300)
  cat(glue::glue("[{outcome_label}] HonestDiD plot saved: {out_path}\n"))
  invisible(sensitivity)
}


# Load and prepare data ---------------------------------------------------

event_study_reg_data <- read_csv(
  file.path(out_dir, "09_event_study_reg_data.csv"), show_col_types = FALSE
) |>
  mutate(
    event_week        = ymd(event_week),
    match_year        = year(event_week),
    optimal_treatment = cause_of_death_category %in% c("illness_fast", "violence", "accident"),
    era               = if_else(match_year < 1970, "pre_1970", "post_1970"),
    decade            = paste0(floor(match_year / 10) * 10, "s")
  )

event_study_outlet_data <- read_csv(
  file.path(out_dir, "10_event_study_outlet_data.csv"), show_col_types = FALSE
) |>
  mutate(
    event_week        = ymd(event_week),
    match_year        = year(event_week),
    optimal_treatment = cause_of_death_category %in% c("illness_fast", "violence", "accident"),
    era               = if_else(match_year < 1970, "pre_1970", "post_1970"),
    decade            = paste0(floor(match_year / 10) * 10, "s")
  )

event_base_clean <- read_csv("data/fmt/event_base.csv", show_col_types = FALSE) |>
  filter(match_year >= 1920, !is.na(vacancy_days)) |>
  transmute(special_election_id, state, cause_of_death_category, vacancy_start = ymd(vacancy_start)) |>
  mutate(cause_of_death_category = case_when(
    is.na(cause_of_death_category) ~ "non_death_vacancy",
    cause_of_death_category %in% c("unknown", "unclear") ~ "unknown_unclear",
    TRUE ~ cause_of_death_category
  )) |>
  filter(!is.na(special_election_id), !is.na(state), !is.na(vacancy_start)) |>
  left_join(ccesMRPprep::states_key |> select(state = st, region), by = "state") |>
  filter(!is.na(region))


# CS-DiD Feasibility Assessment -------------------------------------------

region_vacancy_counts <- event_base_clean |>
  distinct(special_election_id, region) |>
  count(region, name = "n_vacancies")

cs_did_assessment <- region_vacancy_counts |>
  summarise(
    n_regions      = n(),
    n_single_event = sum(n_vacancies == 1),
    n_multi_event  = sum(n_vacancies > 1),
    share_multi    = mean(n_vacancies > 1),
    info_lost_pct  = 1 - sum(n_vacancies == 1) / sum(n_vacancies)
  )

cat("\n=== CS-DiD Feasibility Assessment ===\n")
cat(glue::glue(
  "Regions with >1 vacancy: {cs_did_assessment$n_multi_event} ({round(cs_did_assessment$share_multi*100,1)}%)\n",
  "Events discarded under first-vacancy-only definition: {round(cs_did_assessment$info_lost_pct*100,1)}%\n\n"
))
cat("=> CS-DiD (Callaway & Sant'Anna 2021) requires absorbing treatment.\n",
    "   Vacancies are temporary and repeatable — CS-DiD is NOT appropriate.\n",
    "   Specification: Stacked Event Study (Baker et al. 2022).\n\n")


# optimal_treatment summary -----------------------------------------------

cat("=== optimal_treatment Summary ===\n")
event_study_reg_data |>
  distinct(event_id, match_year, optimal_treatment, cause_of_death_category) |>
  count(optimal_treatment, name = "n_events") |>
  mutate(share = round(n_events / sum(n_events) * 100, 1)) |>
  print()

cat("\nBy cause_of_death_category:\n")
event_study_reg_data |>
  distinct(event_id, cause_of_death_category, optimal_treatment) |>
  count(cause_of_death_category, optimal_treatment, sort = TRUE) |>
  print()


# Subsample definitions ---------------------------------------------------

subs_reg <- list(
  pooled        = event_study_reg_data,
  optimal_true  = event_study_reg_data |> filter(optimal_treatment),
  optimal_false = event_study_reg_data |> filter(!optimal_treatment),
  illness_fast  = event_study_reg_data |> filter(cause_of_death_category == "illness_fast"),
  illness_slow  = event_study_reg_data |> filter(cause_of_death_category == "illness_slow"),
  non_death     = event_study_reg_data |> filter(cause_of_death_category == "non_death_vacancy"),
  pre_1970      = event_study_reg_data |> filter(match_year < 1970),
  post_1970     = event_study_reg_data |> filter(match_year >= 1970)
)

subs_tone <- list(
  pooled        = event_study_outlet_data |> filter(!is.na(slant)),
  optimal_true  = event_study_outlet_data |> filter(optimal_treatment, !is.na(slant)),
  optimal_false = event_study_outlet_data |> filter(!optimal_treatment, !is.na(slant)),
  illness_fast  = event_study_outlet_data |> filter(cause_of_death_category == "illness_fast", !is.na(slant)),
  illness_slow  = event_study_outlet_data |> filter(cause_of_death_category == "illness_slow", !is.na(slant)),
  non_death     = event_study_outlet_data |> filter(cause_of_death_category == "non_death_vacancy", !is.na(slant)),
  pre_1970      = event_study_outlet_data |> filter(match_year < 1970, !is.na(slant)),
  post_1970     = event_study_outlet_data |> filter(match_year >= 1970, !is.na(slant))
)


# HonestDiD Sensitivity — run BEFORE subgroup loops ----------------------
# NOTE: HonestDiD 0.2.6 has a bug where tryCatch cannot catch a condition
# raised inside purrr::map after fixest non-PSD VCOV fixes accumulate.
# Running it first (before any subgroup models) avoids this.

cat("\n=== HonestDiD Sensitivity Analysis ===\n")
mod_articles_pooled <- run_event_study(subs_reg$pooled, "n_articles", "is_treated_region")
mod_tone_pooled     <- run_event_study(subs_tone$pooled, "slant", "is_treated_outlet", "n_slant_articles")
run_honest_did(mod_articles_pooled, "is_treated_region", "n_articles",
               file.path(out_dir, "08_honest_did_articles.png"))
run_honest_did(mod_tone_pooled, "is_treated_outlet", "slant",
               file.path(out_dir, "08_honest_did_tone.png"))


# Event Study Models & Pre-Trend Tests ------------------------------------

cat("\n=== Event Study Models + Pre-Trend Tests ===\n\n")

cat("--- Articles outcome ---\n")
es_coefs_reg <- list()
pretrend_reg <- list()

for (nm in names(subs_reg)) {
  n_events <- n_distinct(subs_reg[[nm]]$event_id)
  cat(glue::glue("  [{nm}] n_events = {n_events}\n"))
  if (n_events < 5) { cat("    => too few events, skipped.\n"); next }
  mod <- run_event_study(subs_reg[[nm]], "n_articles", "is_treated_region")
  es_coefs_reg[[nm]] <- extract_event_study_coefs(mod, nm)
  pretrend_reg[[nm]] <- run_pretrend_test(mod, nm)
  if (!is.null(mod)) {
    pt <- pretrend_reg[[nm]]
    cat(glue::glue("    Pre-trend F({pt$df1}, {pt$df2}) = {round(pt$f_stat, 3)}, p = {round(pt$p_value, 4)}\n"))
  }
}

cat("\n--- Tone outcome ---\n")
es_coefs_tone <- list()
pretrend_tone <- list()

for (nm in names(subs_tone)) {
  n_events <- n_distinct(subs_tone[[nm]]$event_id)
  cat(glue::glue("  [{nm}] n_events = {n_events}\n"))
  if (n_events < 5) { cat("    => too few events, skipped.\n"); next }
  mod <- run_event_study(subs_tone[[nm]], "slant", "is_treated_outlet", weight_var = "n_slant_articles")
  es_coefs_tone[[nm]] <- extract_event_study_coefs(mod, nm)
  pretrend_tone[[nm]] <- run_pretrend_test(mod, nm)
  if (!is.null(mod)) {
    pt <- pretrend_tone[[nm]]
    cat(glue::glue("    Pre-trend F({pt$df1}, {pt$df2}) = {round(pt$f_stat, 3)}, p = {round(pt$p_value, 4)}\n"))
  }
}


# Pooled DiD (scalar ATT) ------------------------------------------------

cat("\n=== Pooled DiD (scalar ATT) ===\n\n")

pooled_did_results_reg <- map2_dfr(names(subs_reg), subs_reg, function(nm, d) {
  if (n_distinct(d$event_id) < 5) return(tibble())
  extract_pooled_att(run_pooled_did(d, "n_articles", "is_treated_region"), "is_treated_region", nm)
}) |> mutate(outcome = "n_articles") |> relocate(outcome, group)

pooled_did_results_tone <- map2_dfr(names(subs_tone), subs_tone, function(nm, d) {
  if (n_distinct(d$event_id) < 5) return(tibble())
  extract_pooled_att(run_pooled_did(d, "slant", "is_treated_outlet", "n_slant_articles"), "is_treated_outlet", nm)
}) |> mutate(outcome = "slant") |> relocate(outcome, group)

pooled_did_results <- bind_rows(pooled_did_results_reg, pooled_did_results_tone)
print(pooled_did_results |> mutate(across(where(is.numeric), \(x) round(x, 4))))
write_csv(pooled_did_results, file.path(out_dir, "08_pooled_did.csv"))


# Pre-trend results export -----------------------------------------------

pretrend_results <- bind_rows(
  bind_rows(pretrend_reg)  |> mutate(outcome = "n_articles"),
  bind_rows(pretrend_tone) |> mutate(outcome = "slant")
) |> relocate(outcome, group)

cat("\n=== Pre-trend Summary ===\n")
print(pretrend_results |> mutate(across(where(is.numeric), \(x) round(x, 4))))
write_csv(pretrend_results, file.path(out_dir, "08_pre_trend_tests.csv"))

es_coefs_all <- bind_rows(
  bind_rows(es_coefs_reg)  |> mutate(outcome = "n_articles"),
  bind_rows(es_coefs_tone) |> mutate(outcome = "slant")
) |> relocate(outcome, group)
write_csv(es_coefs_all, file.path(out_dir, "08_event_study_coefs.csv"))


# Temporal split by decade (articles) ------------------------------------

cat("\n=== Temporal Split: by Decade (Articles) ===\n")

decade_reg_results <- event_study_reg_data |>
  filter(!is.na(decade)) |>
  group_split(decade) |>
  map_dfr(possibly(
    ~ {
      d <- .x; dec <- d$decade[[1]]
      if (n_distinct(d$event_id) < 5) return(tibble())
      mod <- run_event_study(d, "n_articles", "is_treated_region")
      pt  <- run_pretrend_test(mod, dec)
      att <- extract_pooled_att(run_pooled_did(d, "n_articles", "is_treated_region"), "is_treated_region", dec)
      bind_cols(tibble(decade = dec, n_events = n_distinct(d$event_id)),
                pt  |> select(f_stat, df1, df2, p_value),
                att |> select(att, se, p_value) |> rename(att_p = p_value))
    },
    otherwise = tibble()
  ))

print(decade_reg_results |> mutate(across(where(is.numeric), \(x) round(x, 4))))
write_csv(decade_reg_results, file.path(out_dir, "08_temporal_decade_articles.csv"))


# Sun & Abraham robustness check ------------------------------------------

first_vacancy_week <- event_base_clean |>
  group_by(region) |>
  slice_min(vacancy_start, n = 1, with_ties = FALSE) |>
  transmute(region, cohort_week = as.integer(floor_date(vacancy_start, "week", week_start = 1)))

sa_panel <- event_study_reg_data |>
  filter(is_treated_region == 1) |>
  left_join(first_vacancy_week, by = c("treated_region" = "region")) |>
  filter(!is.na(cohort_week)) |>
  mutate(calendar_week = as.integer(floor_date(event_week, "week", week_start = 1)))

n_cohorts <- n_distinct(sa_panel$cohort_week)
cat(glue::glue("\n=== Sun & Abraham Robustness Check ({n_cohorts} cohorts) ===\n"))

if (n_cohorts >= 3) {
  mod_sa <- tryCatch(
    feols(n_articles ~ sunab(cohort_week, calendar_week) | treated_region + calendar_week,
          cluster = ~treated_region, data = sa_panel),
    error = \(e) { cat(glue::glue("[SA] Error: {conditionMessage(e)}\n")); NULL }
  )
  if (!is.null(mod_sa)) {
    sa_coefs   <- iplot(mod_sa, only.params = TRUE)
    stacked_cs <- extract_event_study_coefs(mod_articles_pooled, "Stacked TWFE") |>
      rename(att_val = att)
    plot_sa <- bind_rows(
      as_tibble(sa_coefs) |> mutate(group = "Sun & Abraham (2021)") |>
        rename(event_time = x, att_val = y),
      stacked_cs
    ) |>
      ggplot2::ggplot(ggplot2::aes(x = event_time, y = att_val, color = group,
                                    ymin = ci_low, ymax = ci_high)) +
      ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
      ggplot2::geom_vline(xintercept = -0.5, linetype = "dotted", color = "grey50") +
      ggplot2::geom_ribbon(ggplot2::aes(fill = group), alpha = 0.12, color = NA) +
      ggplot2::geom_line(linewidth = 0.7) +
      ggplot2::geom_point(size = 1.5) +
      ggplot2::scale_color_manual(
        values = c("Stacked TWFE" = "#2c7fb8", "Sun & Abraham (2021)" = "#d95f02")) +
      ggplot2::scale_fill_manual(
        values = c("Stacked TWFE" = "#2c7fb8", "Sun & Abraham (2021)" = "#d95f02")) +
      ggplot2::labs(
        title    = "Robustness: Sun & Abraham (2021) vs. Stacked TWFE",
        subtitle = "SA uses first-vacancy cohort per region; Stacked TWFE uses all vacancies",
        x = "Event time (weeks relative to vacancy start)", y = "ATT (n_articles)",
        color = NULL, fill = NULL
      ) +
      ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(legend.position = "bottom")
    ggplot2::ggsave(file.path(out_dir, "08_sun_abraham_comparison.png"),
                    plot_sa, width = 10, height = 6, dpi = 300)
    cat("[SA] Comparison plot saved.\n")
  }
} else {
  cat(glue::glue("[SA] Only {n_cohorts} cohorts — insufficient.\n"))
}

cat("\nDone.\n")
