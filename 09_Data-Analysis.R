library(tidyverse)
library(glue)

files <- list.files(
  path = "data/raw/TDM_Studio",
  pattern = "\\.(csv|cvs)$",
  recursive = TRUE,
  full.names = TRUE
) |>
  keep(~ str_detect(.x, "TDM_Studio/[^/]+/"))

if (length(files) == 0) stop(glue("No .csv/.cvs files found in {normalizePath('data/raw/TDM_Studio')}."))

data_tdm <- files |>
  map(~ read_csv(.x, show_col_types = FALSE) |> mutate(source_file = .x)) |>
  bind_rows() |> 
  janitor::clean_names() |> 
  rename(state = publisher_province) |> 
  left_join(
    ccesMRPprep::states_key |> 
      select(state = st, region),
    by = "state"
  )

publication_weekly_articles <- data_tdm |>
  transmute(
    publication_id,
    date = ymd(date),
    region
  ) |>
  filter(!is.na(publication_id), !is.na(date)) |>
  mutate(week = floor_date(date, unit = "week", week_start = 1)) |>
  count(publication_id, region, week, name = "n_articles") |>
  group_by(publication_id, region) |>
  complete(week = seq(min(week), max(week), by = "week"), fill = list(n_articles = 0L)) |>
  ungroup()

articles_weekly_total <- publication_weekly_articles |>
  group_by(week) |>
  summarise(n_articles = sum(n_articles), .groups = "drop") |>
  mutate(panel = "Total newspaper articles", region = "Total")

articles_weekly_region <- publication_weekly_articles |>
  group_by(week, region) |>
  summarise(n_articles = sum(n_articles), .groups = "drop") |>
  complete(week, region, fill = list(n_articles = 0L)) |>
  mutate(panel = "Newspaper articles by region")

plot_articles_descriptives <- bind_rows(articles_weekly_total, articles_weekly_region) |>
  ggplot(aes(x = week, y = n_articles, color = region, group = region)) +
  geom_line(linewidth = .5, alpha = .9) +
  facet_wrap(~panel, ncol = 2, scales = "free_y") +
  scale_x_date(date_breaks = "10 years", date_labels = "%Y") +
  labs(x = NULL, y = "Number of newspaper articles", color = "Region") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave(
  "data/fmt/TDM_Studio_articles_descriptives.png",
  plot_articles_descriptives,
  width = 12,
  height = 5,
  dpi = 300
)

event_base_clean <- read_csv("data/fmt/event_base.csv", show_col_types = FALSE) |>
  filter(match_year >= 1920) |> 
  filter(!is.na(vacancy_days)) |> 
  transmute(
    special_election_id,
    state,
    cause_of_death,
    cause_of_death_category,
    vacancy_start = ymd(vacancy_start),
    vacancy_end = ymd(vacancy_end)
  ) |>
  mutate(
    cause_of_death_category = case_when(
      is.na(cause_of_death) ~ "non_death_vacancy",
      cause_of_death_category %in% c("unknown", "unclear") | is.na(cause_of_death_category) ~ "unknown_unclear",
      TRUE ~ cause_of_death_category
    )
  ) |>
  filter(!is.na(special_election_id), !is.na(state), !is.na(vacancy_start)) |>
  mutate(
    vacancy_end = coalesce(vacancy_end, vacancy_start),
    week_start = floor_date(pmin(vacancy_start, vacancy_end), unit = "week", week_start = 1),
    week_end = floor_date(pmax(vacancy_start, vacancy_end), unit = "week", week_start = 1)
  ) |>
  distinct(special_election_id, state, cause_of_death_category, week_start, week_end) |>
  left_join(
    ccesMRPprep::states_key |>
      select(state = st, region),
    by = "state"
  ) |>
  filter(!is.na(region))

events_weekly_region <- event_base_clean |>
  mutate(week = map2(week_start, week_end, ~ seq(.x, .y, by = "week"))) |>
  unnest(week) |>
  group_by(week, region) |>
  summarise(in_vacancy = 1L, .groups = "drop")

newspaper_events_weekly_region <- full_join(
  articles_weekly_region |>
    select(week, region, n_articles),
  events_weekly_region,
  by = c("week", "region")
) |>
  mutate(
    n_articles = replace_na(n_articles, 0L),
    in_vacancy = replace_na(in_vacancy, 0L)
  ) |>
  arrange(region, week)

region_week_panel <- newspaper_events_weekly_region |>
  complete(
    region,
    week = seq(min(week), max(week), by = "week"),
    fill = list(n_articles = 0L, in_vacancy = 0L)
  ) |>
  arrange(region, week)

vacancy_events <- event_base_clean |>
  distinct(special_election_id, region, week_start, cause_of_death_category) |>
  arrange(week_start, region) |>
  transmute(
    event_id = row_number(),
    special_election_id,
    treated_region = region,
    event_week = week_start,
    cause_of_death_category
  )

event_window <- 12L

event_weeks <- vacancy_events |>
  mutate(week = map(event_week, ~ seq(.x - weeks(event_window), .x + weeks(event_window), by = "week"))) |>
  unnest(week) |>
  mutate(event_time = as.integer(difftime(week, event_week, units = "weeks"))) |>
  distinct(event_id, special_election_id, treated_region, cause_of_death_category, event_week, week, event_time)

event_study_reg_data <- event_weeks |>
  left_join(
    region_week_panel |>
      select(region, week, n_articles, in_vacancy),
    by = "week",
    relationship = "many-to-many"
  ) |>
  mutate(is_treated_region = as.integer(region == treated_region)) |>
  filter(is_treated_region == 1L | in_vacancy == 0L)

event_study_region_mod <- fixest::feols(
  n_articles ~ is_treated_region + i(event_time, is_treated_region, ref = -1) | event_id + event_time,
  cluster = ~event_id,
  data = event_study_reg_data
)

event_study_region <- tibble(
  term = names(coef(event_study_region_mod)),
  att = unname(coef(event_study_region_mod))
) |>
  filter(str_detect(term, "^event_time::")) |>
  mutate(event_time = as.integer(str_extract(term, "-?\\d+"))) |>
  left_join(
    confint(event_study_region_mod) |>
      as.data.frame() |>
      rownames_to_column("term") |>
      as_tibble() |>
      rename(ci_low = `2.5 %`, ci_high = `97.5 %`),
    by = "term"
  ) |>
  transmute(event_time, interaction_effect = att, ci_low, ci_high) |>
  bind_rows(tibble(event_time = -1L, interaction_effect = 0, ci_low = 0, ci_high = 0)) |>
  arrange(event_time)

event_study_region_by_cause <- event_study_reg_data |>
  group_split(cause_of_death_category) |>
  map(
    possibly(
      ~ {
        cause_i <- .x$cause_of_death_category[[1]]
        mod_i <- fixest::feols(
          n_articles ~ is_treated_region + i(event_time, is_treated_region, ref = -1) | event_id + event_time,
          cluster = ~event_id,
          data = .x
        )
        tibble(
          term = names(coef(mod_i)),
          att = unname(coef(mod_i))
        ) |>
          filter(str_detect(term, "^event_time::")) |>
          mutate(event_time = as.integer(str_extract(term, "-?\\d+"))) |>
          left_join(
            confint(mod_i) |>
              as.data.frame() |>
              rownames_to_column("term") |>
              as_tibble() |>
              rename(ci_low = `2.5 %`, ci_high = `97.5 %`),
            by = "term"
          ) |>
          transmute(cause_of_death_category = cause_i, event_time, interaction_effect = att, ci_low, ci_high) |>
          bind_rows(
            tibble(
              cause_of_death_category = cause_i,
              event_time = -1L,
              interaction_effect = 0,
              ci_low = 0,
              ci_high = 0
            )
          ) |>
          arrange(event_time)
      },
      otherwise = tibble()
    )
  ) |>
  bind_rows()

event_study_interaction_effect <- event_study_region

plot_event_study_interaction_effect <- event_study_interaction_effect |>
  ggplot(aes(x = event_time, y = interaction_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha = .2, , color =  "#2c7fb8")+
  geom_line(linewidth = .7, color = "#2c7fb8") +
  geom_point(size = 1.2, color = "#2c7fb8") +
  labs(
    x = "Event time (weeks relative to vacancy start)",
    y = "Coefficient",
    title = "Region-level event-study interaction effects"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  "data/fmt/TDM_Studio_event_study_interaction_effect.png",
  plot_event_study_interaction_effect,
  width = 9,
  height = 5,
  dpi = 300
)

plot_event_study_region_by_cause <- event_study_region_by_cause |>
  ggplot(aes(x = event_time, y = interaction_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha = .2, , color =  "#2c7fb8")+
  geom_line(linewidth = .6, color = "#2c7fb8") +
  geom_point(size = 1, color = "#2c7fb8") +
  facet_wrap(~cause_of_death_category, scales = "free_y") +
  labs(
    x = "Event time (weeks relative to vacancy start)",
    y = "Coefficient",
    title = "Region-level event-study interaction effects by cause of death"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  "data/fmt/TDM_Studio_event_study_region_by_cause.png",
  plot_event_study_region_by_cause,
  width = 12,
  height = 7,
  dpi = 300
)
