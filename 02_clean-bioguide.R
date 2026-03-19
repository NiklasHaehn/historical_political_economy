library(tidyverse)
library(stringi)
library(lubridate)
library(glue)

norm_text <- function(x) {
  x |>
    stri_trans_general("Latin-ASCII") |>
    str_remove_all(",") |>
    str_squish() |>
    str_to_upper() |>
    na_if("")
}

bio_guide_duplicates <- function(data){
  data |>
  distinct() |> 
    arrange(term_congress, id_bioguide, term_end) |>
    group_by(term_congress, id_bioguide) |>
    summarise(
      across(-term_end, \(x) reduce(as.list(x), coalesce)),
      term_end = first(term_end),
      .groups = "drop"
    )
}

# Load Data ---------------------------------------------------------------

bio_guides <- read_csv("data/fmt/bio_guides.csv", show_col_types = FALSE) 
mp_keys <- read_csv("data/fmt/congress_member.csv", show_col_types = FALSE)

# Data Management ---------------------------------------------------------

bio_guides_fmt <-
  bio_guides |> 
  filter(term_type == "Representative") |> 
  bio_guide_duplicates()

house_member <-
  mp_keys |> 
  filter(term_type == "rep") |> 
  select(
    id_bioguide,
    id_icpsr,
    bio_gender,
    term_district,
    term_start,
    term_end,
    name_official_full,
    name_nickname
  ) |> 
  mutate(
    term_start = ymd(term_start, quiet = TRUE),
    term_end = ymd(term_end, quiet = TRUE),
    row_id = row_number()
  ) |>
  tidylog::left_join(
    bio_guides_fmt |>
      select(- term_district) |> 
      mutate(term_start = ymd(term_start, quiet = TRUE), term_end = ymd(term_end, quiet = TRUE)) |>
      rename(bio_term_start = term_start, bio_term_end = term_end),
    by = join_by(id_bioguide, between(term_start, bio_term_start, bio_term_end))
  ) |>
  mutate(match_gap_days = abs(as.numeric(term_start - bio_term_start))) |>
  slice_min(match_gap_days, n = 1, with_ties = FALSE, by = "row_id") |>
  tidylog::filter(!is.na(term_job_type)) |> 
  select(-row_id, -match_gap_days) |>
  mutate(
    cd = if_else(is.na(term_district), term_state, glue::glue("{term_state}-{term_district}")),
    year = lubridate::year(term_start),
    election_year = year - 1L,
    name_official_full = if_else(
      is.na(name_official_full),
      glue(
        "{coalesce(name_first, '')} {coalesce(name_middle, '')} {coalesce(name_last, '')} {coalesce(name_suffix, '')}"
      ) |>
        str_squish() |>
        na_if(""),
      name_official_full
    ),
    name_full_norm = norm_text(name_official_full),
    name_first_last_norm = norm_text(glue("{name_first} {name_last}")),
    name_first_norm = norm_text(name_first),
    name_last_norm = norm_text(name_last),
    candidate_first_initial = str_sub(name_first_norm, 1, 1)
  ) |> 
  relocate(contains("date")) |> 
  arrange(term_state, year) 
  

# Save Data Set -----------------------------------------------------------

write_csv(house_member, "data/fmt/MP_data.csv", na = "")

  