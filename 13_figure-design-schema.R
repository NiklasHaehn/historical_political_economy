library(tidyverse)
source("paper_style.R")

out_dir <- "paper/figures/descriptives"

regions <- tibble(
  region  = factor(
    c("Northeast (treated)", "North Central", "South", "West"),
    levels = rev(c("Northeast (treated)", "North Central", "South", "West"))
  ),
  treated = c(TRUE, FALSE, FALSE, FALSE)
) |> mutate(y = as.integer(region))

t_min <- -12; t_max <- 6; t_ref <- -1; t_event <- 0

col_t_pre  <- pal$pre;   col_t_post <- pal$main
col_c_pre  <- "#f0f0f0"; col_c_post <- pal$light
col_ref    <- pal$ref;   col_event  <- pal$third; col_elec <- pal$elec

rects <- bind_rows(
  regions |> transmute(region, y, treated, xmin = t_min, xmax = t_ref, period = "pre"),
  regions |> transmute(region, y, treated, xmin = t_ref, xmax = t_max, period = "post")
) |>
  mutate(fill_cat = case_when(
    treated & period == "pre"  ~ "Treated: pre-period",
    treated & period == "post" ~ "Treated: campaign period",
   !treated & period == "pre"  ~ "Control: pre-period",
   !treated & period == "post" ~ "Control: campaign period"
  ) |> factor(levels = c("Treated: pre-period","Treated: campaign period",
                          "Control: pre-period", "Control: campaign period")))

fill_vals <- c(
  "Treated: pre-period"      = col_t_pre,
  "Treated: campaign period" = col_t_post,
  "Control: pre-period"      = col_c_pre,
  "Control: campaign period" = col_c_post
)

  # theme_schema defined in paper_style.R

p <- ggplot() +
  geom_rect(data = rects,
    aes(xmin = xmin, xmax = xmax, ymin = y - 0.44, ymax = y + 0.44, fill = fill_cat),
    color = NA) +
  scale_fill_manual(values = fill_vals, breaks = names(fill_vals),
                    guide = guide_legend(nrow = 2)) +
  # reference dashed line
  annotate("segment", x = t_ref, xend = t_ref, y = 0.5, yend = 4.5,
           color = col_ref, linewidth = 0.9, linetype = "dashed") +
  annotate("text", x = t_ref - 0.25, y = 5.08,
           label = "Reference week (t = \u22121, omitted)", hjust = 1,
           color = col_ref, size = 2.7, fontface = "italic") +
  # vacancy solid line
  annotate("segment", x = t_event, xend = t_event, y = 0.5, yend = 4.5,
           color = col_event, linewidth = 1.1) +
  annotate("text", x = t_event + 0.25, y = 5.08,
           label = "Vacancy (death or resignation)", hjust = 0,
           color = col_event, size = 2.7, fontface = "italic") +
  # special election dotted line
  annotate("segment", x = t_max, xend = t_max, y = 0.5, yend = 4.5,
           color = col_elec, linewidth = 0.9, linetype = "dotted") +
  annotate("text", x = t_max + 0.2, y = 2.5,
           label = "Special\nElection", color = col_elec, size = 2.7,
           hjust = 0, fontface = "italic") +
  # brackets
  annotate("segment", x = t_min, xend = t_ref - 0.15, y = 0.28, yend = 0.28,
           color = "grey40", linewidth = 0.5,
           arrow = arrow(ends = "both", length = unit(0.13, "cm"), type = "open")) +
  annotate("text", x = (t_min + t_ref) / 2, y = 0.09,
           label = "Pre-period (12 weeks)", size = 2.8, color = "grey30") +
  annotate("segment", x = t_ref + 0.15, xend = t_max, y = 0.28, yend = 0.28,
           color = "grey40", linewidth = 0.5,
           arrow = arrow(ends = "both", length = unit(0.13, "cm"), type = "open")) +
  annotate("text", x = (t_ref + t_max) / 2, y = 0.09,
           label = "Campaign period (\u2264 6 weeks)", size = 2.8, color = "grey30") +
  scale_x_continuous(breaks = seq(t_min, t_max, by = 2),
                     expand = expansion(add = c(0.3, 1.8))) +
  scale_y_continuous(breaks = regions$y, labels = as.character(regions$region),
                     expand = expansion(add = c(0.55, 1.15))) +
  labs(
    title    = "Stacked Event Study Design",
    subtitle = paste0("Each vacancy defines one event. Treated region = the region of the vacancy. ",
                      "Control regions = all other US regions in the same calendar weeks."),
    x = "Weeks relative to vacancy start"
  ) +
  theme_schema

ggsave(file.path(out_dir, "00_design_schema.png"),
       p, width = 9.5, height = 5.0, dpi = 320, bg = "white")
cat("Saved.\n")
