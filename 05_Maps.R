library(tidyverse)
library(glue)

in_file <- "data/fmt/event_base.csv"
out_dir <- "paper/figures/maps"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

events <- read_csv(in_file, show_col_types = FALSE) |>
  distinct(special_election_id, .keep_all = TRUE) |> 
  left_join(ccesMRPprep::states_key |> 
              select(state = st, region),
            by = "state")

state_key <- tibble(
  state = state.abb,
  state_name = str_to_lower(state.name),
  region = state.region
)

us_map <- ggplot2::map_data("state") |> as_tibble()
state_centers <- us_map |>
  group_by(region) |>
  summarise(
    long = (min(long) + max(long)) / 2,
    lat = (min(lat) + max(lat)) / 2,
    .groups = "drop"
  ) |>
  rename(state_name = region)
region_centers <- state_centers |>
  left_join(state_key |> select(state_name, region), by = "state_name") |>
  filter(!is.na(region)) |>
  group_by(region) |>
  summarise(long = mean(long), lat = mean(lat), .groups = "drop")

map_theme <- theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold")
  )

# 1) Number of Special Elections by state
state_counts <- state_key |>
  left_join(events |> count(state, name = "n_special"), by = "state") |>
  mutate(n_special = replace_na(n_special, 0L))

plot_state <- us_map |>
  left_join(state_counts |> select(state_name, n_special), by = c("region" = "state_name")) |>
  ggplot(aes(long, lat, group = group, fill = n_special)) +
  geom_polygon(color = "white", linewidth = 0.15) +
  geom_text(
    data = state_centers |> left_join(state_counts, by = "state_name") |> filter(!is.na(n_special)),
    aes(long, lat, label = n_special),
    size = 2,
    check_overlap = TRUE,
    color = "black",
    inherit.aes = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Count") +
  labs(title = "Special Elections by State") +
  map_theme

# 2) Number of Special Elections by region
region_counts <- events |>
  count(region, name = "n_special")

plot_region <- us_map |>
  left_join(
    state_key |>
      left_join(region_counts, by = "region") |>
      mutate(n_special = replace_na(n_special, 0L)) |>
      select(state_name, n_special),
    by = c("region" = "state_name")
  ) |>
  ggplot(aes(long, lat, group = group, fill = n_special)) +
  geom_polygon(color = "white", linewidth = 0.15) +
  geom_text(
    data = region_centers |> left_join(region_counts, by = "region"),
    aes(long, lat, label = n_special),
    size = 4.6,
    fontface = "bold",
    color = "black",
    inherit.aes = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Count") +
  labs(title = "Special Elections by Region") +
  map_theme

# 3) Number of Special Elections by state and cause of death
deaths <- events |>
  filter(cause_of_vacancy_fmt == "Died") |>
  mutate(
    cause_of_death_category = case_when(
      is.na(cause_of_death_category) ~ "unclear_unknown",
      cause_of_death_category %in% c("unknown", "unclear") ~ "unclear_unknown",
      TRUE ~ cause_of_death_category
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

cause_levels <- deaths |> 
  count(cause_of_death_category, sort = TRUE) |>
  pull(cause_of_death_category)

state_cause_counts <- deaths |>
  count(state, cause_of_death_category, name = "n_special") |>
  left_join(state_key, by = "state") |>
  mutate(
      cause_of_death_category = factor(
      cause_of_death_category,
      levels = cause_levels,
      labels = cause_levels |> purrr::map_chr(\(x) cause_labels[[x]] %||% x)
    )
  )

plot_state_cause <- us_map |>
  left_join(
    state_cause_counts |> select(state_name, cause_of_death_category, n_special),
    by = c("region" = "state_name"),
    relationship = "many-to-many"
  ) |>
  ggplot(aes(long, lat, group = group, fill = n_special)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  geom_text(
    data = state_centers |>
      left_join(
        state_cause_counts |> select(state_name, cause_of_death_category, n_special),
        by = "state_name",
        relationship = "many-to-many"
      ) |>
      filter(n_special > 0),
    aes(long, lat, label = n_special),
    size = 1.5,
    check_overlap = TRUE,
    color = "black",
    inherit.aes = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  facet_wrap(vars(cause_of_death_category), ncol = 3) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Count") +
  labs(title = "Special Elections by State and Cause of Death") +
  map_theme

# 4) Number of Special Elections by region and cause of death
region_cause_counts <- deaths |>
  count(region, cause_of_death_category, name = "n_special") |>
  complete(region = unique(state.region), cause_of_death_category = cause_levels, fill = list(n_special = 0L)) |>
  mutate(
      cause_of_death_category = factor(
      cause_of_death_category,
      levels = cause_levels,
      labels = cause_levels |> purrr::map_chr(\(x) cause_labels[[x]] %||% x)
    )
  )

plot_region_cause <- us_map |>
  left_join(
    state_key |>
      select(state_name, region) |>
      left_join(region_cause_counts, by = "region", relationship = "many-to-many"),
    by = c("region" = "state_name"),
    relationship = "many-to-many"
  ) |>
  ggplot(aes(long, lat, group = group, fill = n_special)) +
  geom_polygon(color = "white", linewidth = 0.1) +
  geom_text(
    data = region_centers |>
      left_join(region_cause_counts, by = "region", relationship = "many-to-many") |>
      filter(n_special > 0),
    aes(long, lat, label = n_special),
    size = 4.2,
    fontface = "bold",
    color = "black",
    inherit.aes = FALSE
  ) +
  coord_map("albers", lat0 = 39, lat1 = 45) +
  facet_wrap(vars(cause_of_death_category), ncol = 3) +
  scale_fill_viridis_c(option = "C", direction = -1, name = "Count") +
  labs(title = "Special Elections by Region and Cause of Death") +
  map_theme

plots <- list(
  map_state = plot_state,
  map_region = plot_region,
  map_state_cause = plot_state_cause,
  map_region_cause = plot_region_cause
)

plots |>
  purrr::imap(
    \(plot_obj, name) ggsave(
      filename = glue("{out_dir}/05_{name}.png"),
      plot = plot_obj,
      width = 12,
      height = if_else(str_detect(name, "cause"), 8, 6),
      dpi = 320,
      bg = "white"
    )
  )

plots
