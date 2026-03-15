library(tidyverse)
library(stringi)
library(lubridate)
library(glue)

norm_text <- function(x) {
  x |>
    coalesce("") |>
    stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_remove_all("\\[[^\\]]*\\]") |>
    str_remove_all("\\([^\\)]*\\)") |>
    str_remove_all("[^A-Z0-9 ]") |>
    str_squish() |>
    na_if("")
}

norm_party <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_replace_all("[^A-Z ]", " ") |>
    str_squish() |>
    na_if("") |>
    (\(p) case_when(
      is.na(p) ~ NA_character_,
      str_detect(p, "DEMOCRAT") ~ "DEMOCRAT",
      str_detect(p, "REPUBLICAN") ~ "REPUBLICAN",
      str_detect(p, "WHIG") ~ "WHIG",
      str_detect(p, "FEDERALIST") ~ "FEDERALIST",
      str_detect(p, "JACKSON") ~ "JACKSON",
      str_detect(p, "INDEPENDENT|\\bIND\\b") ~ "INDEPENDENT",
      TRUE ~ p
    ))()
}

num_or_na <- function(x) suppressWarnings(as.numeric(as.character(x)))

safe_adist <- function(a, b) {
  if (is.na(a) || is.na(b)) Inf else as.numeric(adist(tolower(trimws(a)), tolower(trimws(b)))[1])
}

district_key_from_member <- function(x) {
  out <- x |> as.character() |> str_squish() |> str_to_upper() |> na_if("")
  out <- case_when(
    out %in% c("AT-LARGE", "AT LARGE", "AL", "0", "-1") ~ "0",
    out == "SENATOR" ~ NA_character_,
    TRUE ~ out
  )
  out_num <- suppressWarnings(as.integer(out))
  if_else(!is.na(out_num), as.character(out_num), out)
}


# Load Data ---------------------------------------------------------------
mp_data <- read_csv("data/fmt/MP_data.csv", show_col_types = FALSE)
members_raw <- read_csv("data/fmt/MP_data_llama3.csv", show_col_types = FALSE) |> 
  select(id_bioguide, Special_Election_llama3) |> 
  distinct() |> 
  left_join(mp_data, by = "id_bioguide")

elections_raw <- read_csv("data/fmt/election_data.csv", show_col_types = FALSE) |> 
  distinct() 

# Add Nominate Scores and better district codes
Hall_members <- read_csv("data/raw/Local_Roots/Hall_members.csv") |> 
  filter(chamber == "House") |> 
  mutate(district_code = if_else(district_code == 98, NA, district_code)) |> 
  distinct()

member_fmt <- members_raw |> 
  tidylog::left_join(Hall_members |> 
                       select(- c(bioname, chamber)),
                       by = c("term_congress" = "congress", "id_bioguide" = "bioguide_id", "id_icpsr" = "icpsr")) |> 
  mutate(id_election_bio = glue::glue("{id_bioguide}_{year}_{id_icpsr}")) |> 
  distinct() |> 
  arrange(term_congress, id_bioguide, year, id_icpsr) |> 
  group_by(id_election_bio) |> 
  slice(1) |>
  ungroup()

# Data Management ---------------------------------------------------------

elections_ranked <- elections_raw |>
  filter(office == "US HOUSE", stage == "GEN") |>
  mutate(
    election_id = row_number(),
    election_year = year,
    term_state = state_po,
    cd = district_key_from_member(district),
    candidatevotes_num = num_or_na(candidatevotes),
    totalvotes_num = num_or_na(totalvotes),
    vote_share = if_else(totalvotes_num > 0, candidatevotes_num / totalvotes_num, NA_real_),
    candidate_norm = norm_text(candidate),
    candidate_first_norm = str_extract(candidate_norm, "^[^ ]+"),
    candidate_last_norm = str_extract(candidate_norm, "[^ ]+$"),
    candidate_first_initial = str_sub(candidate_norm, 1, 1),
    candidate_first_last_norm = norm_text(glue("{candidate_first_norm} {candidate_last_norm}"))
  ) |>
  arrange(year, term_state, cd, desc(candidatevotes_num), candidate_norm) |>
  group_by(year, cd, term_state) |>
  mutate(
    election_rank = row_number(),
    n_candidates = n(),
    runnerup_candidate = candidate[election_rank == 2] |> first(),
    runnerup_party = party[election_rank == 2] |> first(),
    runnerup_candidatevotes = candidatevotes_num[election_rank == 2] |> first(),
    runnerup_vote_share = vote_share[election_rank == 2] |> first()
  ) |>
  ungroup()

winners <- elections_ranked |>
  filter(election_rank == 1)

member_winner_key <- member_fmt |>
  tidylog::left_join(winners, by = c("election_year", "term_state"), relationship = "many-to-many", suffix = c("", ".y")) |>
  mutate(
    lev_dist_full_name = map2_dbl(candidate_norm, name_full_norm, safe_adist),
    lev_dist_last_name = map2_dbl(candidate_last_norm, name_last_norm, safe_adist),
    lev_dist_first_name = map2_dbl(candidate_first_norm, name_first_norm, safe_adist)
    ) |>
  arrange(election_id, lev_dist_last_name, lev_dist_first_name, lev_dist_full_name) |>
  group_by(election_id) |>
  slice(1) |>
  ungroup() |> 
  select(election_id, id_election_bio)

member_fmt_winner <-
  member_fmt |> 
  left_join(member_winner_key, by = "id_election_bio", suffix = c("", ".y")) |> 
  left_join(winners, by = "election_id", suffix = c("", ".y")) |> 
  select(-contains(".y"))

all_election_candidates <- elections_ranked |> 
  select(year, state, district, candidate, candidate_first_norm, candidate_last_norm, party, election_id)
  
write_csv(member_fmt_winner, "MP_election_data.csv")
write_csv(all_election_candidates, "All_candidates.csv")
