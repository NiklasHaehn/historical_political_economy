library(tidyverse)
library(fixest)
library(glue)
library(lubridate)
source("paper_style.R")

out_dir <- "paper/figures/analysis"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)


# Data loading ------------------------------------------------------------

articles_raw <- read_csv(
  "data/fmt/10_tdm_event_articles_individual_6w_tone_llama3.csv",
  show_col_types = FALSE
) |>
  filter(!is.na(headline_slant)) |>
  mutate(
    publication_id = str_squish(str_remove(publication_title, "[[:space:]]*[(].*")),
    week      = as.Date(article_week),
    abs_slant = abs(headline_slant)
  )

outlet_panel <- read_csv(
  "data/fmt/10_event_study_outlet_data.csv",
  show_col_types = FALSE
)


# Article-level stacked panel ---------------------------------------------
# Each article × event combination is one row.
# An article from week W at outlet O appears once per event that covers week W.
# is_treated_outlet varies across events for the same article (stacked design).

article_panel <- outlet_panel |>
  inner_join(
    articles_raw |> select(publication_id, week, headline_slant, abs_slant),
    by = c("publication_id", "week"),
    relationship = "many-to-many"
  ) |>
  mutate(match_year = year(event_week))

cat("=== Article-level panel ===\n")
cat("Rows:    ", nrow(article_panel), "\n")
cat("Events:  ", n_distinct(article_panel$event_id), "\n")
cat("Outlets: ", n_distinct(article_panel$publication_id), "\n")
cat("\nTreated/control split:\n")
print(table(article_panel$is_treated_outlet))
cat("\nArticles per outlet (treated vs. control obs):\n")
article_panel |>
  count(publication_id, is_treated_outlet) |>
  pivot_wider(names_from = is_treated_outlet, values_from = n,
              names_prefix = "n_treated_") |>
  print()


# Subsamples --------------------------------------------------------------

subs <- list(
  full     = article_panel,
  post1970 = article_panel |> filter(match_year >= 1970),
  post1990 = article_panel |> filter(match_year >= 1990)
)

cat("\nSubsample sizes:\n")
map_dfr(names(subs), \(nm) tibble(
  subsample   = nm,
  n_articles  = nrow(subs[[nm]]),
  n_events    = n_distinct(subs[[nm]]$event_id),
  n_outlets   = n_distinct(subs[[nm]]$publication_id),
  pct_treated = round(100 * mean(subs[[nm]]$is_treated_outlet), 1)
)) |> print()


# Helpers -----------------------------------------------------------------

run_es <- function(data, outcome, fe_spec, label) {
  fml <- as.formula(glue(
    "{outcome} ~ i(event_time, is_treated_outlet, ref = -1) | {fe_spec}"
  ))
  mod <- tryCatch(
    feols(fml, cluster = ~event_id, data = data),
    error = \(e) { cat("  [Error in", label, "]:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(mod)) return(NULL)

  pre_nm <- names(coef(mod))[str_detect(names(coef(mod)), "event_time::-[0-9]+")]
  if (length(pre_nm) > 0) {
    pt <- wald(mod, keep = pre_nm)
    cat(glue("  [{label}]  Pre-trend F({pt$df1},{pt$df2}) = {round(pt$stat, 3)},  p = {round(pt$p, 4)}\n"))
  } else {
    cat(glue("  [{label}]  No pre-period terms.\n"))
  }
  mod
}

extract_es <- function(mod, outcome_label, spec_label) {
  if (is.null(mod)) return(tibble())
  cn    <- names(coef(mod))
  es_nm <- cn[str_detect(cn, "^event_time::-?[0-9]+:is_treated_outlet$")]
  if (length(es_nm) == 0) return(tibble())
  et  <- as.integer(str_extract(es_nm, "-?[0-9]+"))
  ord <- order(et)
  ci  <- confint(mod)[es_nm[ord], ]
  tibble(
    outcome    = outcome_label,
    spec       = spec_label,
    event_time = et[ord],
    att        = coef(mod)[es_nm[ord]],
    ci_low     = ci[, 1],
    ci_high    = ci[, 2]
  )
}


# Models ------------------------------------------------------------------
# Main FE: event_id^publication_id absorbs outlet-specific baseline within
# each event. Identification = within-outlet × within-event temporal change,
# treated outlets vs. control outlets.

cat("\n=== Outcome: headline_slant (direction) ===\n")
models_slant <- list(
  full_oe     = run_es(subs$full,     "headline_slant",
                       "event_id^publication_id + event_time",
                       "Full, outlet×event FE"),
  full_basic  = run_es(subs$full,     "headline_slant",
                       "event_id + event_time + publication_id",
                       "Full, separate FEs"),
  p1970_oe    = run_es(subs$post1970, "headline_slant",
                       "event_id^publication_id + event_time",
                       "Post-1970, outlet×event FE"),
  p1990_oe    = run_es(subs$post1990, "headline_slant",
                       "event_id^publication_id + event_time",
                       "Post-1990, outlet×event FE"),
  p1990_basic = run_es(subs$post1990, "headline_slant",
                       "event_id + event_time + publication_id",
                       "Post-1990, separate FEs")
)

cat("\n=== Outcome: abs(headline_slant) = Extremeness ===\n")
models_abs <- list(
  full_oe     = run_es(subs$full,     "abs_slant",
                       "event_id^publication_id + event_time",
                       "Full, outlet×event FE"),
  full_basic  = run_es(subs$full,     "abs_slant",
                       "event_id + event_time + publication_id",
                       "Full, separate FEs"),
  p1970_oe    = run_es(subs$post1970, "abs_slant",
                       "event_id^publication_id + event_time",
                       "Post-1970, outlet×event FE"),
  p1990_oe    = run_es(subs$post1990, "abs_slant",
                       "event_id^publication_id + event_time",
                       "Post-1990, outlet×event FE"),
  p1990_basic = run_es(subs$post1990, "abs_slant",
                       "event_id + event_time + publication_id",
                       "Post-1990, separate FEs")
)


# Extract coefficients ----------------------------------------------------

spec_labels <- c(
  full_oe     = "Full: outlet:event FE",
  full_basic  = "Full: additive FE",
  p1970_oe    = "Post-1970: outlet:event FE",
  p1990_oe    = "Post-1990: outlet:event FE",
  p1990_basic = "Post-1990: additive FE"
)

es_slant <- imap_dfr(models_slant, \(m, nm) extract_es(m, "Slant direction", spec_labels[nm]))
es_abs   <- imap_dfr(models_abs,   \(m, nm) extract_es(m, "Extremeness (|slant|)", spec_labels[nm]))
es_all   <- bind_rows(es_slant, es_abs)

write_csv(es_all, file.path(out_dir, "12_es_pluralism_article_coefs.csv"))
cat("\nSaved: 12_es_pluralism_article_coefs.csv\n")


# Plots -------------------------------------------------------------------

spec_colors <- c(
  "Full: outlet:event FE"      = pal$main,
  "Full: additive FE"          = pal$null,
  "Post-1970: outlet:event FE" = pal$third,
  "Post-1990: outlet:event FE" = pal$second,
  "Post-1990: additive FE"     = "#7b2d8b"
)

make_es_fig <- function(data, y_label) {
  # Drop specs with degenerate CIs (collinearity at t=0 creates massive uncertainty)
  data <- data |>
    group_by(spec) |>
    filter(max(abs(ci_high - ci_low), na.rm = TRUE) < 5) |>
    ungroup()
  ggplot(data, aes(x = event_time, y = att, color = spec)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = -1.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.25, linewidth = 0.4) +
    geom_line(linewidth = 0.65) +
    geom_point(size = 1.4) +
    scale_color_manual(values = spec_colors) +
    scale_x_continuous(breaks = seq(-12, 12, by = 4)) +
    facet_wrap(vars(spec), ncol = 2, scales = "free_y") +
    labs(
      x     = "Weeks relative to vacancy start",
      y     = y_label,
      color = NULL
    ) +
    theme_paper +
    theme(legend.position = "none")
}

fig_slant <- make_es_fig(es_slant, "Effect on headline slant")
fig_abs   <- make_es_fig(es_abs,   "Effect on |slant| (extremeness)")

ggsave(file.path(out_dir, "12_fig_article_slant.png"), fig_slant,
       width = 12, height = 7, dpi = 320, bg = "white")
ggsave(file.path(out_dir, "12_fig_article_abs.png"), fig_abs,
       width = 12, height = 7, dpi = 320, bg = "white")
cat("Saved: 12_fig_article_slant.png\n")
cat("Saved: 12_fig_article_abs.png\n")


# Paper figure: main spec only --------------------------------------------

es_paper <- es_all |>
  filter(spec == "Full: outlet:event FE") |>
  mutate(outcome = factor(outcome,
    levels = c("Slant direction", "Extremeness (|slant|)")))

fig_paper <- ggplot(es_paper, aes(x = event_time, y = att)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -1.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                width = 0.25, color = pal$main, linewidth = 0.5) +
  geom_line(color = pal$main, linewidth = 0.7) +
  geom_point(color = pal$main, size = 1.6) +
  scale_x_continuous(breaks = seq(-12, 12, by = 4)) +
  facet_wrap(vars(outcome), scales = "free_y", ncol = 2) +
  labs(
    x = "Weeks relative to vacancy start",
    y = "Estimated effect"
  ) +
  theme_paper

ggsave(file.path(out_dir, "12_fig_pluralism_paper.png"), fig_paper,
       width = 11, height = 4.5, dpi = 320, bg = "white")
cat("Saved: 12_fig_pluralism_paper.png\n")

cat("\nDone.\n")
