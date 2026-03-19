library(tidyverse)
library(fixest)
library(glue)
source("paper_style.R")

out_dir <- "paper/figures/analysis"
app_dir <- "paper/figures/appendix"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(app_dir, recursive = TRUE, showWarnings = FALSE)

reg_data    <- read_csv("data/fmt/09_event_study_reg_data.csv",  show_col_types = FALSE)
outlet_data <- read_csv("data/fmt/10_event_study_outlet_data.csv", show_col_types = FALSE) |>
  filter(!is.na(slant))

exog_cats <- c("illness_fast", "accident", "violence")

# Donut: remove event_time in (-2, -1, 0, 1, 2); reference = -3
DONUT_EXCL <- -2:2
REF_TIME   <- -3L


# Group filters -----------------------------------------------------------

reg_groups <- list(
  pooled       = reg_data    |> filter(cause_of_death_category != "non_death_vacancy",
                                       !event_time %in% DONUT_EXCL),
  post_1970    = reg_data    |> filter(cause_of_death_category != "non_death_vacancy",
                                       year(event_week) >= 1970,
                                       !event_time %in% DONUT_EXCL),
  optimal_true = reg_data    |> filter(cause_of_death_category %in% exog_cats,
                                       !event_time %in% DONUT_EXCL)
)

out_groups <- list(
  pooled       = outlet_data |> filter(cause_of_death_category != "non_death_vacancy",
                                       !event_time %in% DONUT_EXCL),
  post_1970    = outlet_data |> filter(cause_of_death_category != "non_death_vacancy",
                                       year(event_week) >= 1970,
                                       !event_time %in% DONUT_EXCL),
  optimal_true = outlet_data |> filter(cause_of_death_category %in% exog_cats,
                                       !event_time %in% DONUT_EXCL),
  illness_slow = outlet_data |> filter(cause_of_death_category == "illness_slow",
                                       !event_time %in% DONUT_EXCL)
)


# Estimation helpers ------------------------------------------------------

run_donut_es <- function(data, outcome, treat_var, fe_spec, label) {
  fml <- as.formula(glue(
    "{outcome} ~ i(event_time, {treat_var}, ref = {REF_TIME}) | {fe_spec}"
  ))
  mod <- tryCatch(
    feols(fml, cluster = ~event_id, data = data),
    error = \(e) { cat("  [Error:", label, "]:", conditionMessage(e), "\n"); NULL }
  )
  if (is.null(mod)) return(NULL)

  pre_nm <- names(coef(mod))[str_detect(names(coef(mod)), glue("event_time::-[4-9]|event_time::-1[0-9]|event_time::-3"))]
  pre_nm <- pre_nm[pre_nm != glue("event_time::{REF_TIME}:{treat_var}")]
  if (length(pre_nm) > 1) {
    pt <- wald(mod, keep = pre_nm)
    cat(glue("  [{label}]  Pre-trend F({pt$df1},{pt$df2}) = {round(pt$stat,3)},  p = {round(pt$p,4)}\n\n"))
  }
  mod
}

extract_es <- function(mod, treat_var, group_label, outcome_label) {
  if (is.null(mod)) return(tibble())
  cn    <- names(coef(mod))
  pat   <- glue("^event_time::-?[0-9]+:{treat_var}$")
  es_nm <- cn[str_detect(cn, pat)]
  if (length(es_nm) == 0) return(tibble())
  et  <- as.integer(str_extract(es_nm, "-?[0-9]+"))
  ord <- order(et)
  ci  <- confint(mod)[es_nm[ord], ]
  tibble(
    outcome    = outcome_label,
    group      = group_label,
    event_time = et[ord],
    att        = unname(coef(mod)[es_nm[ord]]),
    ci_low     = ci[, 1],
    ci_high    = ci[, 2]
  )
}

extract_att <- function(mod, treat_var, group_label, outcome_label) {
  if (is.null(mod)) return(tibble())
  cn   <- names(coef(mod))
  post <- cn[str_detect(cn, glue("^event_time::[0-9]+:{treat_var}$"))]
  if (length(post) == 0) return(tibble())
  b  <- coef(mod)[post]
  v  <- vcov(mod, "cluster")[post, post]
  # pooled post-period ATT: simple average of post coefficients
  w   <- rep(1 / length(b), length(b))
  att <- sum(w * b)
  se  <- sqrt(as.numeric(t(w) %*% as.matrix(v) %*% w))
  tibble(
    outcome  = outcome_label,
    group    = group_label,
    att      = att,
    se       = se,
    ci_low   = att - 1.96 * se,
    ci_high  = att + 1.96 * se,
    p_value  = 2 * pnorm(-abs(att / se))
  )
}


# Run models --------------------------------------------------------------

cat("=== n_articles (donut) ===\n")
mods_art <- imap(reg_groups, \(d, nm)
  run_donut_es(d, "n_articles", "is_treated_region", "event_id + event_time", nm)
)

cat("=== slant (donut) ===\n")
mods_slant <- imap(out_groups, \(d, nm)
  run_donut_es(d, "slant", "is_treated_outlet", "event_id + event_time", nm)
)


# Extract coefficients ----------------------------------------------------

es_art   <- imap_dfr(mods_art,   \(m, nm) extract_es(m, "is_treated_region", nm, "n_articles"))
es_slant <- imap_dfr(mods_slant, \(m, nm) extract_es(m, "is_treated_outlet",  nm, "slant"))
es_all   <- bind_rows(es_art, es_slant)

att_art   <- imap_dfr(mods_art,   \(m, nm) extract_att(m, "is_treated_region", nm, "n_articles"))
att_slant <- imap_dfr(mods_slant, \(m, nm) extract_att(m, "is_treated_outlet",  nm, "slant"))
att_all   <- bind_rows(att_art, att_slant)

write_csv(es_all,  file.path(out_dir, "13_donut_es_coefs.csv"))
write_csv(att_all, file.path(out_dir, "13_donut_att.csv"))
cat("\nSaved CSVs\n")


# Plotting helpers --------------------------------------------------------

group_labels <- c(
  pooled       = "Pooled",
  post_1970    = "Post-1970",
  optimal_true = "Exogenous deaths",
  illness_slow = "Illness (slow)"
)

group_colors <- c(
  pooled       = pal$main,
  post_1970    = pal$main,
  optimal_true = pal$second,
  illness_slow = pal$null
)

# Shaded donut-hole annotation
donut_rect <- annotate(
  "rect",
  xmin = min(DONUT_EXCL) - 0.5, xmax = max(DONUT_EXCL) + 0.5,
  ymin = -Inf, ymax = Inf,
  fill = "grey80", alpha = 0.35
)

plot_donut_es <- function(data, color = pal$main) {
  ggplot(data, aes(x = event_time, y = att)) +
    donut_rect +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
    geom_vline(xintercept = REF_TIME - 0.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.35, linewidth = 0.4, color = color) +
    geom_line(color = color, linewidth = 0.7) +
    geom_point(color = color, size = 1.5) +
    scale_x_continuous(breaks = seq(-12, 12, by = 4)) +
    labs(x = "Weeks relative to vacancy start", y = NULL) +
    theme_paper
}


# Figure 1: n_articles donut event study ----------------------------------

art_pooled <- es_all |> filter(outcome == "n_articles", group == "pooled")
art_post   <- es_all |> filter(outcome == "n_articles", group == "post_1970")

p_art_pooled <- plot_donut_es(art_pooled, pal$null) +
  labs(title = "Pooled (all years)", y = "Estimated effect on article count")
p_art_post   <- plot_donut_es(art_post,   pal$null) +
  labs(title = "Post-1970 subsample")

fig_donut_art_es <- patchwork::wrap_plots(p_art_pooled, p_art_post, ncol = 2)

ggsave(
  file.path(out_dir, "13_fig_donut_art_es.png"),
  fig_donut_art_es, width = 11, height = 4.5, dpi = 320, bg = "white"
)
cat("Saved: 13_fig_donut_art_es.png\n")


# Figure 2: slant donut event study (multi-spec) --------------------------

slant_multi <- es_all |>
  filter(outcome == "slant", group %in% c("pooled", "post_1970", "optimal_true")) |>
  mutate(spec = factor(group,
    levels = c("pooled", "post_1970", "optimal_true"),
    labels = c("Pooled", "Post-1970", "Exogenous deaths")
  ))

fig_donut_slant_es <- ggplot(slant_multi, aes(x = event_time, y = att,
                                               color = spec, linetype = spec)) +
  donut_rect +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_vline(xintercept = REF_TIME - 0.5, linetype = "dotted", color = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.35, linewidth = 0.4) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.4) +
  scale_color_manual(values = c(
    "Pooled"           = pal$main,
    "Post-1970"        = pal$main,
    "Exogenous deaths" = pal$second
  )) +
  scale_linetype_manual(values = c(
    "Pooled"           = "solid",
    "Post-1970"        = "dashed",
    "Exogenous deaths" = "solid"
  )) +
  scale_x_continuous(breaks = seq(-12, 12, by = 4)) +
  labs(
    x       = "Weeks relative to vacancy start",
    y       = "Estimated effect on partisan slant",
    color   = NULL, linetype = NULL
  ) +
  theme_paper +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "13_fig_donut_slant_es.png"),
  fig_donut_slant_es, width = 8, height = 5, dpi = 320, bg = "white"
)
cat("Saved: 13_fig_donut_slant_es.png\n")


# Figure 3: ATT coefplot (both outcomes) ----------------------------------

att_plot <- att_all |>
  filter(
    (outcome == "n_articles" & group %in% c("pooled", "post_1970", "optimal_true")) |
    (outcome == "slant"      & group %in% c("pooled", "post_1970", "optimal_true", "illness_slow"))
  ) |>
  mutate(
    spec_label = case_when(
      group == "pooled"       ~ "Pooled\n(all years)",
      group == "post_1970"    ~ "Post-1970\nsubsample",
      group == "optimal_true" ~ "Exogenous deaths\n(fast illness, accidents, violence)",
      group == "illness_slow" ~ "Illness (slow)\ndeaths only"
    ) |> factor(levels = c(
      "Pooled\n(all years)",
      "Post-1970\nsubsample",
      "Exogenous deaths\n(fast illness, accidents, violence)",
      "Illness (slow)\ndeaths only"
    )),
    outcome_label = if_else(outcome == "slant", "Partisan slant", "Article count"),
    sig           = p_value < 0.05,
    exogenous     = group %in% c("pooled", "post_1970", "optimal_true")
  )

fig_donut_att <- ggplot(att_plot, aes(x = spec_label, y = att,
                                       shape = sig, color = exogenous)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15, linewidth = 0.9) +
  geom_point(size = 4) +
  scale_shape_manual(
    values = c(`TRUE` = 16, `FALSE` = 21),
    labels = c(`TRUE` = "p < 0.05", `FALSE` = "p \u2265 0.05"),
    name   = NULL
  ) +
  scale_color_manual(
    values = c(`TRUE` = pal$main, `FALSE` = pal$null),
    guide  = "none"
  ) +
  facet_wrap(vars(outcome_label), scales = "free", ncol = 2) +
  labs(x = NULL, y = "Estimated ATT (donut design)") +
  theme_paper +
  theme(legend.position = "bottom")

ggsave(
  file.path(out_dir, "13_fig_donut_att.png"),
  fig_donut_att, width = 10, height = 5, dpi = 320, bg = "white"
)
cat("Saved: 13_fig_donut_att.png\n")

cat("\nDone. Figures in:", out_dir, "\n")
print(att_all)
