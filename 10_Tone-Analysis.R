library(tidyverse)
library(lubridate)
library(glue)
library(ollamar)

script_start <- Sys.time()

set.seed(1234)

model_name <- Sys.getenv("HEADLINE_TONE_MODEL", "llama3.2:latest")
reduced_out <- Sys.getenv("TONE_REDUCED_OUT", "data/fmt/TDM_Studio_event_articles_individual_6w.csv")
labels_out <- Sys.getenv("HEADLINE_TONE_LABELS_OUT", "data/fmt/10_tdm_headline_tone_labels_llama3.csv")
articles_tone_out <- Sys.getenv("HEADLINE_TONE_ARTICLES_OUT", "data/fmt/10_tdm_event_articles_individual_6w_tone_llama3.csv")
event_study_out <- Sys.getenv("TONE_EVENT_STUDY_OUT", "data/fmt/TDM_Studio_event_study_tone_interaction_effect.csv")
event_study_cause_out <- Sys.getenv("TONE_EVENT_STUDY_CAUSE_OUT", "data/fmt/TDM_Studio_event_study_tone_by_cause.csv")
plot_out <- Sys.getenv("TONE_EVENT_STUDY_PLOT_OUT", "data/fmt/TDM_Studio_event_study_tone_interaction_effect.png")
plot_cause_out <- Sys.getenv("TONE_EVENT_STUDY_CAUSE_PLOT_OUT", "data/fmt/TDM_Studio_event_study_tone_by_cause.png")
n_headlines_max <- Sys.getenv("HEADLINE_TONE_N_MAX", "") |>
  na_if("") |>
  as.integer() |>
  (\(x) ifelse(is.na(x), Inf, x))()
event_window <- Sys.getenv("TONE_EVENT_WINDOW_WEEKS", "6") |>
  as.integer() |>
  (\(x) ifelse(is.na(x), 6L, x))() |>
  as.integer()

files <- list.files(
  path = "data/raw/TDM_Studio",
  pattern = "\\.(csv|cvs)$",
  recursive = TRUE,
  full.names = TRUE
) |>
  keep(~ str_detect(.x, "TDM_Studio/[^/]+/"))

if (length(files) == 0) stop(glue("No .csv/.cvs files found in {normalizePath('data/raw/TDM_Studio')}."))

articles <- files |>
  map(~ read_csv(.x, show_col_types = FALSE) |> mutate(source_file = .x)) |>
  bind_rows() |>
  janitor::clean_names() |>
  rename(state_article = publisher_province) |>
  left_join(
    ccesMRPprep::states_key |>
      select(state = st, region),
    by = c("state_article" = "state")
  ) |>
  mutate(
    article_date = ymd(date),
    article_week = floor_date(article_date, unit = "week", week_start = 1),
    article_id = coalesce(as.character(goid), glue("article_{row_number()}"))
  ) |>
  filter(!is.na(article_date), !is.na(region))

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
  distinct(special_election_id, state, cause_of_death_category, vacancy_start, week_start, week_end) |>
  left_join(
    ccesMRPprep::states_key |>
      select(state = st, region),
    by = "state"
  ) |>
  filter(!is.na(region))

event_articles_individual <- articles |>
  inner_join(
    event_base_clean |>
      transmute(
        special_election_id,
        state_event = state,
        region,
        cause_of_death_category,
        vacancy_start,
        event_week = week_start,
        window_start = vacancy_start - weeks(6),
        window_end = vacancy_start + weeks(6)
      ),
    by = "region",
    relationship = "many-to-many"
  ) |>
  filter(between(article_date, window_start, window_end)) |>
  mutate(
    event_time_day = as.integer(article_date - vacancy_start),
    event_time_week = as.integer(difftime(article_week, event_week, units = "weeks"))
  ) |>
  filter(between(event_time_week, -6L, 6L)) |>
  relocate(
    special_election_id,
    state_event,
    region,
    vacancy_start,
    cause_of_death_category,
    event_week,
    window_start,
    window_end,
    event_time_week,
    event_time_day,
    article_id,
    article_date,
    article_week
  ) |>
  arrange(special_election_id, article_date, publication_id, article_id)

prompt_headline_tone <- function(headline) {
  glue(
    "You are a political text annotation assistant.

Task:
Classify the headline based on tone toward U.S. partisan actors.

Return:
- 0 = balanced or neutral political headline
- 1 = biased political headline
- NA = not a U.S. political headline or insufficient political context

Definition of U.S. partisan actors:
Democrats, Republicans, Democratic Party, Republican Party, GOP, liberals, conservatives, the left, the right, or similar explicitly political sides in U.S. politics.

Definitions:
- 0: The headline is about U.S. politics and uses neutral, factual, or symmetric language. It does not clearly favor or disfavor one partisan side.
- 1: The headline is about U.S. politics and uses asymmetric or evaluative language that favors or disfavors one partisan side.
- NA: The headline is not about U.S. politics, is not political news, or is too vague to determine partisan tone.

Code as 1 if the headline:
- praises one side,
- blames one side,
- portrays one side as more competent, moral, extreme, dangerous, dishonest, or responsible,
- uses loaded or judgmental language toward one side.

Code as 0 if the headline:
- is descriptive,
- reports conflict symmetrically,
- states a political development without clear evaluative slant.

Code as NA if:
- the headline is non-political,
- the headline is not about U.S. politics,
- no clear political context is present,
- the text is too incomplete or vague to evaluate.

Rules:
1. Use only the headline text.
2. Do not use outside knowledge.
3. Only evaluate explicit wording in the headline.
4. If only one partisan side is mentioned:
   - return 1 if the wording is evaluative or loaded,
   - return 0 if the wording is neutral and factual.
5. Return only one value: 0, 1, or NA.

Headline: {headline}
"
  )
}

normalize_label <- function(x) {
  x |>
    coalesce("") |>
    str_to_upper() |>
    str_squish() |>
    str_extract_all("\\b(?:NA|N/A|0|1)\\b") |>
    map_chr(
      ~ .x |>
        first() |>
        coalesce("NA") |>
        str_replace("N/A", "NA")
    )
}

classify_headline <- function(headline) {
  if (headline |> coalesce("") |> str_squish() == "") return("NA")
  generate(model_name, prompt_headline_tone(headline), temperature = 0) |>
    resp_process("text") |>
    normalize_label()
}

safe_classify_headline <- possibly(classify_headline, otherwise = "NA")

headline_pool <- event_articles_individual |>
  distinct(title) |>
  filter(!(is.na(title) | str_squish(title) == "")) |>
  arrange(title)

headline_labels_existing <- if (file.exists(labels_out)) {
  labels_raw <- read_csv(labels_out, show_col_types = FALSE)
  if ("llm_completed" %in% names(labels_raw)) {
    labels_raw |>
      transmute(
        title = as.character(title),
        headline_tone_label = as.character(headline_tone_label),
        llm_completed = as.integer(llm_completed)
      ) |>
      filter(!(is.na(title) | str_squish(title) == "")) |>
      distinct(title, .keep_all = TRUE)
  } else {
    labels_raw |>
      transmute(
        title = as.character(title),
        headline_tone_label = NA_character_,
        llm_completed = 0L
      ) |>
      filter(!(is.na(title) | str_squish(title) == "")) |>
      distinct(title, .keep_all = TRUE)
  }
} else {
  tibble(title = character(), headline_tone_label = character(), llm_completed = integer())
}

headline_labels <- headline_pool |>
  left_join(headline_labels_existing, by = "title")

headline_missing <- headline_labels |>
  filter(coalesce(llm_completed, 0L) == 0L) |>
  slice_head(n = n_headlines_max)

if (nrow(headline_missing) > 0) {
  ollama_ready <- tryCatch(
    {
      generate(model_name, "Return exactly 0.", temperature = 0) |>
        resp_process("text")
      TRUE
    },
    error = \(e) FALSE
  )

  if (!ollama_ready) {
    stop(
      glue(
        "Ollama is not reachable on 127.0.0.1:11434. Start Ollama (`ollama serve`) and ensure model `{model_name}` is available (`ollama pull {model_name}`)."
      )
    )
  }

  headline_labels_new <- headline_missing |>
    mutate(headline_tone_label = map_chr(title, safe_classify_headline), llm_completed = 1L) |>
    select(title, headline_tone_label, llm_completed)

  headline_labels <- headline_labels |>
    select(title, headline_tone_label, llm_completed) |>
    filter(!title %in% headline_labels_new$title) |>
    bind_rows(headline_labels_new)
}

headline_labels <- headline_labels |>
  mutate(
    llm_completed = coalesce(llm_completed, 0L),
    headline_tone_label = if_else(llm_completed == 1L, normalize_label(headline_tone_label), NA_character_)
  ) |>
  distinct(title, .keep_all = TRUE) |>
  arrange(title)

event_articles_tone <- event_articles_individual |>
  left_join(headline_labels, by = "title") |>
  mutate(
    headline_tone_label = if_else(is.na(headline_tone_label), "NA", normalize_label(headline_tone_label)),
    headline_tone = case_when(
      headline_tone_label == "1" ~ 1L,
      headline_tone_label == "0" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

c(reduced_out, labels_out, articles_tone_out) |>
  dirname() |>
  unique() |>
  walk(~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

write_csv(event_articles_individual, reduced_out)
write_csv(headline_labels, labels_out, na = "")
write_csv(event_articles_tone, articles_tone_out, na = "")

articles_tone_unique <- event_articles_tone |>
  distinct(article_id, publication_id, region, article_week, headline_tone)

publication_weekly_tone <- articles_tone_unique |>
  transmute(publication_id = as.character(publication_id), region, week = article_week, headline_tone) |>
  filter(!is.na(publication_id), !is.na(region), !is.na(week)) |>
  group_by(publication_id, region, week) |>
  summarise(
    tone = mean(headline_tone, na.rm = TRUE),
    n_tone_articles = sum(!is.na(headline_tone)),
    .groups = "drop"
  ) |>
  mutate(tone = if_else(is.nan(tone), NA_real_, tone))

events_weekly_region <- event_base_clean |>
  mutate(week = map2(week_start, week_end, ~ seq(.x, .y, by = "week"))) |>
  unnest(week) |>
  group_by(week, region) |>
  summarise(in_vacancy = 1L, .groups = "drop")

outlet_week_panel <- publication_weekly_tone |>
  left_join(events_weekly_region, by = c("week", "region")) |>
  mutate(
    tone = replace_na(tone, 0),
    in_vacancy = replace_na(in_vacancy, 0L)
  ) |>
  group_by(publication_id, region) |>
  complete(
    week = seq(min(week), max(week), by = "week"),
    fill = list(tone = 0, n_tone_articles = 0L, in_vacancy = 0L)
  ) |>
  ungroup() |>
  arrange(publication_id, week)

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

event_weeks <- vacancy_events |>
  mutate(week = map(event_week, ~ seq(.x - weeks(event_window), .x + weeks(event_window), by = "week"))) |>
  unnest(week) |>
  mutate(event_time = as.integer(difftime(week, event_week, units = "weeks"))) |>
  distinct(event_id, special_election_id, treated_region, cause_of_death_category, event_week, week, event_time)

event_study_outlet_data <- event_weeks |>
  left_join(
    outlet_week_panel |>
      select(publication_id, region, week, tone, in_vacancy),
    by = "week",
    relationship = "many-to-many"
  ) |>
  filter(!is.na(publication_id)) |>
  mutate(is_treated_outlet = as.integer(region == treated_region)) |>
  filter(is_treated_outlet == 1L | (is_treated_outlet == 0L & in_vacancy == 0L))

if (event_study_outlet_data |> summarise(n_tone_values = n_distinct(tone)) |> dplyr::pull(n_tone_values) <= 1) {
  stop(
    glue(
      "Weekly tone has no variation after aggregation. LLM results were saved ({labels_out}, {articles_tone_out}), but event-study estimation is not possible. Ensure headlines are classified with 0/1 (not only NA)."
    )
  )
}

event_study_outlet_mod <- fixest::feols(
  tone ~ is_treated_outlet + i(event_time, is_treated_outlet, ref = -1) | event_id + event_time,
  cluster = ~event_id,
  data = event_study_outlet_data
)

event_study_outlet <- tibble(
  term = names(coef(event_study_outlet_mod)),
  att = unname(coef(event_study_outlet_mod))
) |>
  filter(str_detect(term, "^event_time::")) |>
  mutate(event_time = as.integer(str_extract(term, "-?\\d+"))) |>
  left_join(
    confint(event_study_outlet_mod) |>
      as.data.frame() |>
      rownames_to_column("term") |>
      as_tibble() |>
      rename(ci_low = `2.5 %`, ci_high = `97.5 %`),
    by = "term"
  ) |>
  transmute(event_time, interaction_effect = att, ci_low, ci_high) |>
  bind_rows(tibble(event_time = -1L, interaction_effect = 0, ci_low = 0, ci_high = 0)) |>
  arrange(event_time)

event_study_outlet_by_cause <- event_study_outlet_data |>
  group_split(cause_of_death_category) |>
  map(
    possibly(
      ~ {
        cause_i <- .x$cause_of_death_category[[1]]
        mod_i <- fixest::feols(
          tone ~ is_treated_outlet + i(event_time, is_treated_outlet, ref = -1) | event_id + event_time,
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

plot_event_study_tone_interaction_effect <- event_study_outlet |>
  ggplot(aes(x = event_time, y = interaction_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha = .2, color = "#2c7fb8") +
  geom_line(linewidth = .7, color = "#2c7fb8") +
  geom_point(size = 1.2, color = "#2c7fb8") +
  labs(
    x = "Event time (weeks relative to vacancy start)",
    y = "Coefficient",
    title = "Outlet-level event-study interaction effects (headline tone)"
  ) +
  theme_minimal(base_size = 11)

plot_event_study_tone_by_cause <- event_study_outlet_by_cause |>
  ggplot(aes(x = event_time, y = interaction_effect)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey40") +
  geom_vline(xintercept = 0, linetype = "dotted", color = "grey40") +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), alpha = .2, color = "#2c7fb8") +
  geom_line(linewidth = .6, color = "#2c7fb8") +
  geom_point(size = 1, color = "#2c7fb8") +
  facet_wrap(~cause_of_death_category, scales = "free_y") +
  labs(
    x = "Event time (weeks relative to vacancy start)",
    y = "Coefficient",
    title = "Outlet-level event-study interaction effects by cause of death (headline tone)"
  ) +
  theme_minimal(base_size = 11)

c(event_study_out, event_study_cause_out, plot_out, plot_cause_out) |>
  dirname() |>
  unique() |>
  walk(~ dir.create(.x, recursive = TRUE, showWarnings = FALSE))

write_csv(event_study_outlet, event_study_out)
write_csv(event_study_outlet_by_cause, event_study_cause_out)

ggsave(plot_out, plot_event_study_tone_interaction_effect, width = 9, height = 5, dpi = 300)
ggsave(plot_cause_out, plot_event_study_tone_by_cause, width = 12, height = 7, dpi = 300)

script_end <- Sys.time()
script_secs <- as.numeric(difftime(script_end, script_start, units = "secs"))
