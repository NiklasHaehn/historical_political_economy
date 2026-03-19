library(tidyverse)
library(glue)
library(HonestDiD)
source("paper_style.R")

out_dir     <- "paper/figures/analysis"
app_dir     <- "paper/figures/appendix"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

es_coefs   <- read_csv("data/fmt/08_event_study_coefs.csv",  show_col_types = FALSE)
pooled_att <- read_csv("data/fmt/08_pooled_did.csv",         show_col_types = FALSE)
pretrends  <- read_csv("data/fmt/08_pre_trend_tests.csv",    show_col_types = FALSE)

col_main   <- pal$main
col_second <- pal$second
col_third  <- pal$third
col_null   <- pal$null

theme_es <- theme_paper

# exogenous groups: illness_fast + accident + violence
exog_cats <- c("illness_fast", "accident", "violence")


# Helper: single event-study panel ----------------------------------------

plot_es_panel <- function(data, color = col_main, ref_time = -1L) {
  ggplot(data, aes(x = event_time, y = att)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = ref_time - 0.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, linewidth = 0.4, color = color) +
    geom_line(color = color, linewidth = 0.7) +
    geom_point(color = color, size = 1.5) +
    scale_x_continuous(breaks = seq(-12, 12, by = 4)) +
    labs(x = "Weeks relative to vacancy start", y = NULL) +
    theme_es
}


# =========================================================================
# Figure 1: n_articles event study (main results, first in paper)
#   Left panel:  pooled
#   Right panel: post-1970
# No overall title/subtitle (patchwork); panel labels identify each panel.
# =========================================================================

art_pooled   <- es_coefs |> filter(outcome == "n_articles", group == "pooled")
art_post1970 <- es_coefs |> filter(outcome == "n_articles", group == "post_1970")

p_art_pooled <- plot_es_panel(art_pooled, col_null) +
  labs(title = "Pooled (all years)", y = "Estimated effect on article count")

p_art_post1970 <- plot_es_panel(art_post1970, col_null) +
  labs(title = "Post-1970 subsample")

fig_art_es <- patchwork::wrap_plots(p_art_pooled, p_art_post1970, ncol = 2)

ggsave(
  file.path(out_dir, "08_fig_art_es.png"),
  fig_art_es, width = 11, height = 4.5, dpi = 320, bg = "white"
)
cat("Saved: 08_fig_art_es.png\n")


# =========================================================================
# Figure 2: n_articles ATT coefplot
# Groups: pooled, post_1970, optimal_true (exogenous deaths)
# =========================================================================

att_art <- pooled_att |>
  filter(outcome == "n_articles", group %in% c("pooled", "post_1970", "optimal_true")) |>
  left_join(
    pretrends |> select(outcome, group, pt_p = p_value),
    by = c("outcome", "group")
  ) |>
  mutate(
    spec_label = case_when(
      group == "pooled"       ~ "Pooled\n(all years)",
      group == "post_1970"    ~ "Post-1970\nsubsample",
      group == "optimal_true" ~ "Exogenous deaths\n(fast illness, accidents, violence)"
    ) |> factor(levels = c(
      "Pooled\n(all years)",
      "Post-1970\nsubsample",
      "Exogenous deaths\n(fast illness, accidents, violence)"
    )),
    sig = p_value < 0.05
  )

fig_art_att <- ggplot(att_art, aes(x = spec_label, y = att, shape = sig)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 0.9,
                color = col_null) +
  geom_point(size = 4, color = col_null) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21),
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "p \u2265 0.05"),
                     name   = NULL) +
  labs(x = NULL, y = "Estimated ATT (article count)") +
  theme_es +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "08_fig_art_att.png"),
  fig_art_att, width = 7, height = 4.5, dpi = 320, bg = "white"
)
cat("Saved: 08_fig_art_att.png\n")


# =========================================================================
# Figure 3: Slant multi-spec event study
# Groups: pooled, post_1970, optimal_true
# Single panel, distinguished by color/linetype — no title/subtitle.
# =========================================================================

spec_labels <- c(
  pooled       = "Pooled",
  post_1970    = "Post-1970",
  optimal_true = "Exogenous deaths"
)
spec_colors <- c(
  pooled       = col_main,
  post_1970    = col_main,
  optimal_true = col_second
)
spec_lty <- c(
  pooled       = "solid",
  post_1970    = "dashed",
  optimal_true = "solid"
)

slant_multi <- es_coefs |>
  filter(outcome == "slant", group %in% names(spec_labels)) |>
  mutate(
    spec = factor(group, levels = names(spec_labels), labels = spec_labels)
  )

fig_slant_multi <- ggplot(slant_multi, aes(x = event_time, y = att,
                                            color = spec, linetype = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = -1.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c(
    "Pooled"           = col_main,
    "Post-1970"        = col_main,
    "Exogenous deaths" = col_second
  )) +
  scale_linetype_manual(values = c(
    "Pooled"           = "solid",
    "Post-1970"        = "dashed",
    "Exogenous deaths" = "solid"
  )) +
  scale_x_continuous(breaks = seq(-12, 12, by = 4)) +
  labs(
    x        = "Weeks relative to vacancy start",
    y        = "Estimated effect on partisan slant",
    color    = NULL, linetype = NULL
  ) +
  theme_es +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "08_fig_slant_multi.png"),
  fig_slant_multi, width = 8, height = 5, dpi = 320, bg = "white"
)
cat("Saved: 08_fig_slant_multi.png\n")


# =========================================================================
# Figure 4: Slant ATT coefplot
# Groups: pooled, post_1970, optimal_true, illness_slow
# No title/subtitle.
# =========================================================================

att_slant <- pooled_att |>
  filter(outcome == "slant",
         group %in% c("pooled", "post_1970", "optimal_true", "illness_slow")) |>
  left_join(
    pretrends |> select(outcome, group, pt_p = p_value),
    by = c("outcome", "group")
  ) |>
  mutate(
    spec_label = case_when(
      group == "pooled"        ~ "Pooled\n(all years)",
      group == "post_1970"     ~ "Post-1970\nsubsample",
      group == "optimal_true"  ~ "Exogenous deaths\n(fast illness, accidents, violence)",
      group == "illness_slow"  ~ "Illness (slow)\ndeaths only"
    ) |> factor(levels = c(
      "Pooled\n(all years)",
      "Post-1970\nsubsample",
      "Exogenous deaths\n(fast illness, accidents, violence)",
      "Illness (slow)\ndeaths only"
    )),
    sig      = p_value < 0.05,
    exogenous = group %in% c("pooled", "post_1970", "optimal_true")
  )

fig_slant_att <- ggplot(att_slant, aes(x = spec_label, y = att, shape = sig,
                                        color = exogenous)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 0.9) +
  geom_point(size = 4) +
  scale_shape_manual(values = c(`TRUE` = 16, `FALSE` = 21),
                     labels = c(`TRUE` = "p < 0.05", `FALSE` = "p \u2265 0.05"),
                     name   = NULL) +
  scale_color_manual(values = c(`TRUE` = col_main, `FALSE` = col_null),
                     guide  = "none") +
  labs(x = NULL, y = "Estimated ATT (partisan slant)") +
  theme_es +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "08_fig_slant_att.png"),
  fig_slant_att, width = 7, height = 4.5, dpi = 320, bg = "white"
)
cat("Saved: 08_fig_slant_att.png\n")


# =========================================================================
# ATT summary table (CSV for paper)
# =========================================================================

att_table <- pooled_att |>
  filter(
    (outcome == "slant"      & group %in% c("pooled", "post_1970", "optimal_true",
                                             "illness_slow")) |
    (outcome == "n_articles" & group %in% c("pooled", "post_1970", "optimal_true"))
  ) |>
  left_join(
    pretrends |> select(outcome, group, pt_f = f_stat, pt_df1 = df1, pt_p = p_value),
    by = c("outcome", "group")
  ) |>
  transmute(
    Outcome       = if_else(outcome == "slant", "Partisan tone", "Article volume"),
    Specification = recode(group,
      pooled       = "Pooled",
      post_1970    = "Post-1970",
      optimal_true = "Exogenous deaths",
      illness_slow = "Illness (slow)"
    ),
    ATT        = round(att, 4),
    SE         = round(se, 4),
    CI_low     = round(ci_low, 4),
    CI_high    = round(ci_high, 4),
    p_value    = round(p_value, 4),
    PreTrend_F = round(pt_f, 3),
    PreTrend_p = round(pt_p, 4),
    PassesPT   = pt_p >= 0.05
  )

write_csv(att_table, file.path(out_dir, "08_att_table.csv"))
cat("Saved: 08_att_table.csv\n")
print(att_table)


# =========================================================================
# APPENDIX: Robustness — illness_fast event study
# Single panel (slant, illness_fast), no title/subtitle.
# =========================================================================

slant_ilfast <- es_coefs |> filter(outcome == "slant", group == "illness_fast")

app_robust <- plot_es_panel(slant_ilfast, col_second) +
  labs(y = "Estimated effect on partisan slant")

ggsave(
  file.path(app_dir, "app_fig_robust_ilfast.png"),
  app_robust, width = 7, height = 4.5, dpi = 320, bg = "white"
)
cat("Saved: app_fig_robust_ilfast.png\n")


# =========================================================================
# APPENDIX: HonestDiD sensitivity for each main DiD model
# Models: pooled, post_1970, optimal_true, illness_slow
# Faceted 2x2 layout — facet labels identify each model.
# =========================================================================

event_study_outlet_data <- read_csv(
  "data/fmt/10_event_study_outlet_data.csv", show_col_types = FALSE
) |> filter(!is.na(slant))

run_honest_did <- function(data, label) {
  mod <- tryCatch(
    fixest::feols(
      slant ~ is_treated_outlet + i(event_time, is_treated_outlet, ref = -1) |
        event_id + event_time,
      cluster = ~event_id,
      weights = ~n_slant_articles,
      data    = data
    ),
    error = function(e) { cat(" [HonestDiD skip:", label, "]:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(mod)) return(NULL)

  cn          <- names(coef(mod))
  es_nm       <- cn[str_detect(cn, "^event_time::-?[0-9]+:is_treated_outlet$")]
  event_times <- as.integer(str_extract(es_nm, "-?[0-9]+"))
  ord         <- order(event_times)
  betahat     <- unname(coef(mod)[es_nm[ord]])
  sigma_mat   <- unname(as.matrix(vcov(mod, "cluster")[es_nm[ord], es_nm[ord]]))
  nPre        <- sum(event_times[ord] < -1)
  nPost       <- sum(event_times[ord] >= 0)

  if (nPre < 2 || nPost < 1) {
    cat(" [HonestDiD skip:", label, "]: insufficient pre/post periods\n")
    return(NULL)
  }

  robust  <- HonestDiD::createSensitivityResults_relativeMagnitudes(
    betahat = betahat, sigma = sigma_mat,
    numPrePeriods = nPre, numPostPeriods = nPost,
    Mbarvec = seq(0, 2, by = 0.25), alpha = 0.05
  )
  orig_cs <- HonestDiD::constructOriginalCS(
    betahat = betahat, sigma = sigma_mat,
    numPrePeriods = nPre, numPostPeriods = nPost, alpha = 0.05
  )
  raw <- HonestDiD::createSensitivityPlot_relativeMagnitudes(robust, orig_cs)
  is_hline <- vapply(raw$layers, \(l) inherits(l$geom, "GeomHline"), logical(1))
  raw$layers <- raw$layers[!is_hline]

  raw$data$model <- label
  raw
}

hd_data <- list(
  "Pooled"            = event_study_outlet_data |>
                          filter(cause_of_death_category != "non_death_vacancy"),
  "Post-1970"         = event_study_outlet_data |>
                          filter(cause_of_death_category != "non_death_vacancy",
                                 year(event_week) >= 1970),
  "Exogenous deaths"  = event_study_outlet_data |>
                          filter(cause_of_death_category %in% exog_cats),
  "Illness (slow)"    = event_study_outlet_data |>
                          filter(cause_of_death_category == "illness_slow")
)

hd_plist <- imap(hd_data, \(dat, nm) {
  p <- run_honest_did(dat, nm)
  if (is.null(p)) return(NULL)
  # Remove HonestDiD's default red/blue color scale and restyle
  p$layers <- p$layers[!vapply(p$layers, \(l) inherits(l$geom, "GeomHline"), logical(1))]
  p +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    scale_color_manual(
      values = c("Original" = col_main, "C-LF" = col_second),
      guide  = guide_legend(title = NULL)
    ) +
    labs(
      title = nm,
      x     = expression(bar(M) ~ "(max pre-trend violation)"),
      y     = "ATT (partisan slant)"
    ) +
    theme_es +
    theme(legend.position = "bottom")
})
hd_plist <- compact(hd_plist)

if (length(hd_plist) >= 1) {
  app_honest <- patchwork::wrap_plots(hd_plist, ncol = 2)
  ggsave(
    file.path(app_dir, "app_fig_honest_did.png"),
    app_honest, width = 10, height = 8, dpi = 320, bg = "white"
  )
  cat("Saved: app_fig_honest_did.png\n")
} else {
  cat("Warning: no HonestDiD models converged, app_fig_honest_did.png not saved.\n")
}

cat("\nDone. Main figures in:", out_dir, "\nAppendix figures in:", app_dir, "\n")
