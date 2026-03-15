library(tidyverse)
library(glue)

in_file <- "data/fmt/event_base.csv"
out_dir <- "paper/figures/descriptives"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
state_key <- tibble(state = state.abb, region = state.region)
congress_lookup <- tibble(
  congress_num = 1:200,
  congress_start_year = 1789 + (congress_num - 1) * 2,
  congress_start = lubridate::ymd(
    glue("{congress_start_year}-{if_else(congress_num <= 73, '03-04', '01-03')}")
  )
)

cause_labels <- c(
  accident = "Accident",
  illness_fast = "Illness (fast)",
  illness_slow = "Illness (slow)",
  unclear_unknown = "Unclear / Unknown",
  violence = "Violence",
  unknown = "Unknown"
)
brew <- list(
  set1 = RColorBrewer::brewer.pal(8, "Set1"),
  set2 = RColorBrewer::brewer.pal(8, "Set2"),
  blues = RColorBrewer::brewer.pal(9, "Blues"),
  greys = RColorBrewer::brewer.pal(9, "Greys"),
  dark2 = RColorBrewer::brewer.pal(8, "Dark2"),
  beige = RColorBrewer::brewer.pal(9, "YlOrBr")[1]
)

theme_descriptive <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

events <- read_csv(in_file, show_col_types = FALSE) |>
  distinct(special_election_id, .keep_all = TRUE) |>
  left_join(state_key, by = "state") |>
  mutate(
    year = lubridate::year(special_election_date),
    decade = floor(year / 10) * 10,
    congress_index = findInterval(special_election_date, congress_lookup$congress_start),
    congress_num = congress_lookup$congress_num[pmax(congress_index, 1)],
    congress_start = congress_lookup$congress_start[pmax(congress_index, 1)],
    day_in_congress = as.integer(special_election_date - congress_start) + 1L,
    vacancy_days_plot = as.integer(special_election_date - vacancy_start),
    vacancy_days_plot = if_else(vacancy_days_plot < 0, NA_integer_, vacancy_days_plot)
  ) |> 
  filter(vacancy_days_plot < 365)

deaths <- events |>
  filter(cause_of_vacancy_fmt == "Died") |>
  mutate(
    cause = case_when(
      is.na(cause_of_death_category) ~ "unclear_unknown",
      cause_of_death_category %in% c("unknown", "unclear") ~ "unclear_unknown",
      TRUE ~ cause_of_death_category
    ) |>
      recode(!!!cause_labels, .default = "Other")
  )

# 1) Histogram: deaths by cause of death
plot_01_deaths_cause <- deaths |>
  count(cause, name = "n") |>
  mutate(cause = fct_reorder(cause, n)) |>
  ggplot(aes(cause, n, fill = cause)) +
  geom_col(width = 0.72) +
  geom_text(aes(label = n), hjust = -0.1, size = 3.2) +
  coord_flip() +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.08))) +
  labs(
    title = "Deaths by Cause of Death",
    x = NULL,
    y = "Number of Special Elections"
  ) +
  theme_descriptive

# 2) Frequency over total period
plot_02_special_elections_over_time <- events |>
  count(decade, name = "n") |>
  ggplot(aes(decade, n)) +
  geom_col(fill = brew$greys[7], width = 8) +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 12)) +
  labs(
    title = "Special Elections Over Time (Decade Aggregation)",
    x = "Decade",
    y = "Count"
  ) +
  theme_descriptive

# 3) Frequency within each congressional term
plot_03_special_elections_within_term <- events |>
  filter(between(day_in_congress, 1L, 731L)) |>
  mutate(month_in_term = pmin(((day_in_congress - 1L) %/% 30L) + 1L, 24L)) |>
  count(month_in_term, name = "n") |>
  ggplot(aes(month_in_term, n)) +
  geom_col(fill = brew$blues[7], width = 0.88) +
  scale_x_continuous(breaks = seq(1, 24, 2)) +
  labs(
    title = "Special Elections Within a Congressional Term",
    x = "Month in Term",
    y = "Count"
  ) +
  theme_descriptive

# 4) Vacancy duration over time (boxplot)
plot_04_vacancy_boxplot_over_time <- events |>
  filter(!is.na(vacancy_days_plot)) |>
  mutate(decade = factor(decade)) |>
  ggplot(aes(decade, vacancy_days_plot)) +
  geom_boxplot(fill = brew$blues[4], color = brew$blues[9], outlier.alpha = 0.25, linewidth = 0.3) +
  labs(
    title = "Vacancy Duration Over Time",
    x = "Decade",
    y = "Vacancy Duration (Days)"
  ) +
  theme_descriptive +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

vacancy_region_year <- events |>
  filter(!is.na(region), !is.na(vacancy_days_plot)) |>
  group_by(region, year) |>
  summarise(
    vacancy_days_median = median(vacancy_days_plot, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

# 5b) Vacancy duration over time by region (faceted)
plot_05b_vacancy_region_facets <- vacancy_region_year |>
  ggplot(aes(year, vacancy_days_median)) +
  geom_point(color = brew$dark2[3], alpha = 0.3) +
  geom_smooth(se = FALSE, span = 0.22, color = brew$set1[1], linewidth = 0.9) +
  facet_wrap(vars(region), ncol = 2, scales = "free_y") +
  scale_x_continuous(breaks = scales::breaks_pretty(n = 8)) +
  labs(
    title = "Vacancy Duration Over Time by Region (Faceted)",
    x = "Year",
    y = "Median Vacancy Duration (Days)"
  ) +
  theme_descriptive

region_levels <- c("Northeast", "North Central", "South", "West")
region_axis <- tibble(region = factor(region_levels, levels = region_levels), y = seq_along(region_levels))
state_axis <- events |>
  distinct(state, region) |>
  filter(!is.na(state), !is.na(region)) |>
  mutate(region = factor(region, levels = region_levels)) |>
  arrange(region, state) |>
  mutate(state = factor(state, levels = rev(unique(state))))
x_1920 <- lubridate::ymd("1920-01-01")
x_2020 <- lubridate::ymd("2020-12-31")
events_1920_2020 <- events |> filter(between(special_election_date, x_1920, x_2020))
timeline_bounds <- events |>
  summarise(
    x_min = min(vacancy_start, na.rm = TRUE),
    x_max = max(special_election_date, na.rm = TRUE)
  )
region_base <- region_axis |>
  mutate(
    x_min = timeline_bounds$x_min[[1]],
    x_max = timeline_bounds$x_max[[1]]
  )
region_base_1920_2020 <- region_axis |> mutate(x_min = x_1920, x_max = x_2020)
state_base_1920_2020 <- state_axis |> mutate(x_min = x_1920, x_max = x_2020)

collapse_spans <- function(df, group_var, x_min = as.Date("1000-01-01"), x_max = as.Date("9999-12-31")) {
  group_var <- rlang::ensym(group_var)
  df |>
    filter(!is.na(!!group_var), !is.na(vacancy_start), !is.na(special_election_date), vacancy_start <= special_election_date) |>
    transmute(
      group = !!group_var,
      start = pmax(vacancy_start, x_min),
      end = pmin(special_election_date, x_max)
    ) |>
    filter(start <= end) |>
    arrange(group, start, end) |>
    group_by(group) |>
    mutate(
      start_num = as.integer(start),
      end_num = as.integer(end),
      end_lag = lag(cummax(end_num)),
      span_id = cumsum(if_else(is.na(end_lag) | start_num > end_lag, 1L, 0L))
    ) |>
    group_by(group, span_id) |>
    summarise(start = min(start), end = max(end), .groups = "drop")
}

vacancy_region_spans <- collapse_spans(events, region) |>
  mutate(region = factor(group, levels = region_levels)) |>
  select(-group) |>
  left_join(region_axis, by = "region") |>
  filter(!is.na(y))

special_election_ticks <- events |>
  filter(!is.na(region), !is.na(special_election_date)) |>
  transmute(region = factor(region, levels = region_levels), special_election_date) |>
  left_join(region_axis, by = "region") |>
  filter(!is.na(y))

vacancy_region_spans_1920_2020 <- collapse_spans(events_1920_2020, region, x_1920, x_2020) |>
  mutate(region = factor(group, levels = region_levels)) |>
  select(-group) |>
  left_join(region_axis, by = "region") |>
  filter(!is.na(y))

special_election_ticks_1920_2020 <- events_1920_2020 |>
  filter(!is.na(region), !is.na(special_election_date)) |>
  transmute(region = factor(region, levels = region_levels), special_election_date) |>
  left_join(region_axis, by = "region") |>
  filter(!is.na(y))

vacancy_state_spans_1920_2020 <- collapse_spans(events_1920_2020, state, x_1920, x_2020) |>
  mutate(state = factor(as.character(group), levels = levels(state_axis$state))) |>
  select(-group) |>
  left_join(state_axis, by = "state") |>
  filter(!is.na(region))

special_election_ticks_state_1920_2020 <- events_1920_2020 |>
  filter(!is.na(state), !is.na(special_election_date)) |>
  transmute(state = factor(as.character(state), levels = levels(state_axis$state)), special_election_date) |>
  left_join(state_axis, by = "state") |>
  filter(!is.na(region))

# 6) Regional vacancy timeline with special-election ticks
plot_06_region_timeline <- ggplot() +
  geom_segment(
    data = region_base,
    aes(x = x_min, xend = x_max, y = y, yend = y),
    linewidth = 8.8,
    color = brew$beige,
    lineend = "butt"
  ) +
  geom_segment(
    data = vacancy_region_spans,
    aes(x = start, xend = end, y = y, yend = y),
    linewidth = 8,
    color = brew$blues[6],
    alpha = 0.85,
    lineend = "butt"
  ) +
  geom_segment(
    data = special_election_ticks,
    aes(x = special_election_date, xend = special_election_date, y = y - 0.3, yend = y + 0.3),
    linewidth = 0.25,
    color = brew$set1[1],
    alpha = 0.95
  ) +
  scale_y_continuous(
    breaks = region_axis$y,
    labels = region_axis$region,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_x_date(date_breaks = "20 years", date_labels = "%Y") +
  labs(
    title = "Regional Vacancy Timeline with Special Elections",
    x = "Year",
    y = NULL,
    caption = "Beige bars = full timeline, blue bars = vacancy periods, red ticks = special elections"
  ) +
  theme_descriptive

# 6a) Regional vacancy timeline, 1920-2020
plot_06a_region_timeline_1920_2020 <- ggplot() +
  geom_segment(
    data = region_base_1920_2020,
    aes(x = x_min, xend = x_max, y = y, yend = y),
    linewidth = 8.8,
    color = brew$beige,
    lineend = "butt"
  ) +
  geom_segment(
    data = vacancy_region_spans_1920_2020,
    aes(x = start, xend = end, y = y, yend = y),
    linewidth = 8,
    color = brew$blues[6],
    alpha = 0.85,
    lineend = "butt"
  ) +
  geom_segment(
    data = special_election_ticks_1920_2020,
    aes(x = special_election_date, xend = special_election_date, y = y - 0.3, yend = y + 0.3),
    linewidth = 0.25,
    color = brew$set1[1],
    alpha = 0.95
  ) +
  scale_y_continuous(
    breaks = region_axis$y,
    labels = region_axis$region,
    expand = expansion(add = c(0.5, 0.5))
  ) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", limits = c(x_1920, x_2020)) +
  labs(
    title = "Regional Vacancy Timeline with Special Elections (1920-2020)",
    x = "Year",
    y = NULL,
    caption = "Beige bars = full timeline, blue bars = vacancy periods, red ticks = special elections"
  ) +
  theme_descriptive

# 6b) State vacancy timeline, 1920-2020
plot_06b_state_timeline_1920_2020 <- ggplot() +
  geom_segment(
    data = state_base_1920_2020,
    aes(x = x_min, xend = x_max, y = state, yend = state),
    linewidth = 2.9,
    color = brew$beige,
    lineend = "butt"
  ) +
  geom_segment(
    data = vacancy_state_spans_1920_2020,
    aes(x = start, xend = end, y = state, yend = state),
    linewidth = 2.5,
    color = brew$blues[6],
    alpha = 0.85,
    lineend = "butt"
  ) +
  geom_point(
    data = special_election_ticks_state_1920_2020,
    aes(x = special_election_date, y = state),
    shape = 124,
    size = 1.8,
    color = brew$set1[1],
    alpha = 0.95
  ) +
  facet_wrap(vars(region), ncol = 2, scales = "free_y") +
  scale_y_discrete(expand = expansion(add = c(0.6, 0.6))) +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y", limits = c(x_1920, x_2020)) +
  labs(
    title = "State Vacancy Timeline by Region with Special Elections (1920-2020)",
    x = "Year",
    y = NULL,
    caption = "Beige bars = full timeline, blue bars = vacancy periods, red ticks = special elections"
  ) +
  theme_descriptive +
  theme(axis.text.y = element_text(size = 5))

plots <- list(
  plot_01_deaths_cause = plot_01_deaths_cause,
  plot_02_special_elections_over_time = plot_02_special_elections_over_time,
  plot_03_special_elections_within_term = plot_03_special_elections_within_term,
  plot_04_vacancy_boxplot_over_time = plot_04_vacancy_boxplot_over_time,
  plot_05b_vacancy_region_facets = plot_05b_vacancy_region_facets,
  plot_06_region_timeline = plot_06_region_timeline,
  plot_06a_region_timeline_1920_2020 = plot_06a_region_timeline_1920_2020,
  plot_06b_state_timeline_1920_2020 = plot_06b_state_timeline_1920_2020
)

plots |>
  imap(
    \(plot_obj, name) ggsave(
      filename = glue("{out_dir}/06_{name}.png"),
      plot = plot_obj,
      width = 12,
      height = case_when(
        str_detect(name, "state_timeline") ~ 15,
        str_detect(name, "boxplot|facets") ~ 7,
        str_detect(name, "timeline") ~ 5.8,
        TRUE ~ 6
      ),
      dpi = 320,
      bg = "white"
    )
  )

plots
