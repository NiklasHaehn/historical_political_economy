# Unified visual style for all paper figures
# source("paper_style.R") at the top of each figure script

# Color palette -----------------------------------------------------------

pal <- list(
  main   = "#2c7fb8",   # primary blue  — main results, treated
  second = "#d95f02",   # orange        — robustness / secondary
  third  = "#31a354",   # green         — tertiary
  null   = "#636363",   # medium grey   — null results / control
  light  = "#bdbdbd",   # light grey    — background / control pre-period
  pre    = "#deebf7",   # pale blue     — treated pre-period
  bg     = "#f7f7f7",   # near-white    — area backgrounds
  ref    = "#e34a33",   # red           — reference lines
  elec   = "#756bb1"    # purple        — special election markers
)

# Categorical palette for multi-group descriptive plots (6 colours)
pal_cat <- c("#2c7fb8", "#d95f02", "#31a354", "#756bb1", "#636363", "#e34a33")

# Main paper theme --------------------------------------------------------
# Use for all analysis and descriptive plots (non-map, non-schema)

theme_paper <- theme_minimal(base_size = 11) +
  theme(
    # Titles
    plot.title       = element_text(face = "bold", size = 12, hjust = 0,
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9, color = "grey40", hjust = 0,
                                    margin = margin(b = 8)),
    plot.caption     = element_text(size = 7.5, color = "grey40", hjust = 0,
                                    margin = margin(t = 6)),
    plot.margin      = margin(10, 12, 10, 12),
    # Axes
    axis.title       = element_text(size = 9, color = "grey30"),
    axis.text        = element_text(size = 9, color = "grey30"),
    axis.ticks       = element_line(color = "grey75", linewidth = 0.3),
    # Grid: y-axis only
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.35),
    # Facets
    strip.text         = element_text(face = "bold", size = 9),
    strip.background   = element_blank(),
    # Legend
    legend.position      = "bottom",
    legend.title         = element_blank(),
    legend.text          = element_text(size = 9),
    legend.key.size      = unit(0.42, "cm"),
    legend.key.spacing.x = unit(0.4, "cm"),
    legend.margin        = margin(t = 2)
  )

# Map theme ---------------------------------------------------------------
# Use for choropleth and geographic plots

theme_map <- theme_void(base_size = 11) +
  theme(
    plot.title    = element_text(face = "bold", size = 12, hjust = 0.5,
                                 margin = margin(b = 4)),
    plot.subtitle = element_text(size = 9, color = "grey40", hjust = 0.5,
                                 margin = margin(b = 8)),
    plot.caption  = element_text(size = 7.5, color = "grey40", hjust = 0),
    plot.margin   = margin(10, 12, 10, 12),
    legend.position      = "bottom",
    legend.title         = element_text(size = 9, face = "bold"),
    legend.text          = element_text(size = 9),
    legend.key.size      = unit(0.42, "cm"),
    legend.key.spacing.x = unit(0.3, "cm"),
    strip.text    = element_text(face = "bold", size = 9)
  )

# Schema / diagram theme --------------------------------------------------
# Use for design diagrams and schematic figures

theme_schema <- theme_void(base_size = 11) +
  theme(
    plot.title         = element_text(face = "bold", size = 13, hjust = 0.5,
                                      margin = margin(b = 5)),
    plot.subtitle      = element_text(size = 9, color = "grey40", hjust = 0.5,
                                      margin = margin(b = 10)),
    plot.margin        = margin(12, 16, 12, 16),
    axis.text.y        = element_text(hjust = 1, size = 10, color = "grey20",
                                      margin = margin(r = 6)),
    axis.text.x        = element_text(size = 9, color = "grey30",
                                      margin = margin(t = 4)),
    axis.title.x       = element_text(size = 9, color = "grey30",
                                      margin = margin(t = 6)),
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
    legend.position      = "bottom",
    legend.title         = element_blank(),
    legend.text          = element_text(size = 9.5),
    legend.key.size      = unit(0.42, "cm"),
    legend.key.spacing.x = unit(0.5, "cm")
  )

# Event study helper ------------------------------------------------------
# Standard elements for event study plots; add with +
#
# CI convention: always use geom_errorbar(), never geom_ribbon().
#   geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.3, linewidth = 0.4)
# Place geom_errorbar() before geom_line() and geom_point() so caps sit behind the line.

es_reference_lines <- list(
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50", linewidth = 0.4),
  geom_vline(xintercept = -1.5, linetype = "dotted", color = "grey50", linewidth = 0.4),
  scale_x_continuous(breaks = seq(-12, 12, by = 4))
)
