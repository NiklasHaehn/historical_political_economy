library(tidyverse)
library(stringi)
library(lubridate)
library(glue)

safe_adist <- function(a, b) {
  if (is.na(a) || is.na(b)) Inf else as.numeric(adist(tolower(trimws(a)), tolower(trimws(b)))[1])
}

parse_special_date <- function(x) {
  month_token <- "jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:t(?:ember)?)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?"
  month_day_year_regex <- glue("(?i)\\b(?:{month_token})\\b\\s+\\d{{1,2}}\\s*,\\s*\\d{{4}}\\b")
  month_day_regex <- glue("(?i)\\b({month_token})\\b\\s+(\\d{{1,2}})\\b")
  month_year_regex <- glue("(?i)\\b({month_token})\\b\\s+\\d{{4}}\\b")
  month_year_numeric_regex <- "\\b(\\d{1,2})\\s*/\\s*(\\d{4})\\b"
  parse_one <- function(value) {
    clean <- value |>
      as.character() |>
      str_remove_all("\\[[^\\]]*\\]") |>
      str_replace_all("[–—−]", "-") |>
      str_squish()
    if (is.na(value) || is.na(clean) || clean == "") return(as.Date(NA))
    year <- str_extract(clean, "(?<!\\d)\\d{4}(?!\\d)(?!.*\\d{4})")
    if (is.na(year)) return(as.Date(NA))
    md_matches <- str_match_all(clean, month_day_regex)[[1]]
    inferred_mdy <- if (nrow(md_matches) == 0) character(0) else {
      glue("{md_matches[, 2]} {str_pad(md_matches[, 3], 2, side = 'left', pad = '0')}, {year}")
    }
    candidates <- c(
      str_extract_all(clean, month_day_year_regex)[[1]],
      str_extract_all(clean, "\\b\\d{1,2}/\\d{1,2}/\\d{4}\\b")[[1]],
      inferred_mdy
    ) |>
      unique() |>
      discard(\(v) is.na(v) || v == "")
    parsed <- if (length(candidates) == 0) as.Date(character(0)) else {
      parse_date_time(candidates, orders = c("B d, Y", "b d, Y", "m/d/Y"), quiet = TRUE) |>
        as_date() |>
        discard(is.na)
    }
    if (length(parsed) > 0) return(min(parsed))
    fallback_month <- str_match(clean, month_year_regex)[, 2]
    fallback_numeric <- str_match(clean, month_year_numeric_regex)
    fallback_date <- if (!is.na(fallback_month)) {
      parse_date_time(glue("{fallback_month} 1, {year}"), orders = c("B d, Y", "b d, Y"), quiet = TRUE) |>
        as_date()
    } else if (!is.na(fallback_numeric[, 1])) {
      ymd(glue("{fallback_numeric[, 3]}-{str_pad(fallback_numeric[, 2], 2, side = 'left', pad = '0')}-01"), quiet = TRUE)
    } else {
      as.Date(NA)
    }
    if_else(is.na(fallback_date), as.Date(NA), ceiling_date(fallback_date, "month") - days(1))
  }
  x |>
    map_chr(\(value) {
      parsed <- parse_one(value)
      if_else(is.na(parsed), NA_character_, format(parsed, "%Y-%m-%d"))
    }) |>
    ymd(quiet = TRUE)
}

extract_term_note_date <- function(x) {
  month_token <- "jan(?:uary)?|feb(?:ruary)?|mar(?:ch)?|apr(?:il)?|may|jun(?:e)?|jul(?:y)?|aug(?:ust)?|sep(?:t(?:ember)?)?|oct(?:ober)?|nov(?:ember)?|dec(?:ember)?"
  x |>
    as.character() |>
    str_extract(glue("(?i)\\b(?:{month_token})\\b\\s+\\d{{1,2}}\\s*,\\s*\\d{{4}}\\b")) |>
    parse_special_date()
}

clean_candidate_name <- function(x) {
  out <- x |>
    str_replace_all("\\[[^\\]]*\\]", "") |>
    str_remove("\\(.*") |>
    str_remove_all(",") |>
    str_remove_all('"') |>
    str_squish() |>
    na_if("")
  if_else(str_to_upper(coalesce(out, "")) %in% c("NONE", "VACANT"), NA_character_, out)
}

normalize_name <- function(x, rm_suffix = FALSE) {
  out <- x |>
    stri_trans_general("Latin-ASCII") |>
    str_replace_all("[^A-Za-z0-9\\s']", " ") |>
    str_replace_all("[:punct:]|[:symbol:]", "") |>
    str_squish() |>
    str_to_upper() |>
    na_if("")
  if (rm_suffix) {
    out <- out |>
      str_remove("\\s+(JR|SR|II|III|IV|V|VI|VII|VIII|IX|X)$") |>
      str_squish() |>
      na_if("")
  }
  out
}

clean_cause_of_death <- function(x) {
  x |>
    str_replace_all("\\[[^\\]]*\\]", "") |>
    str_remove_all('"') |>
    str_replace_all("[–—−]", "-") |>
    str_squish() |>
    na_if("")
}

categorize_cause_of_death <- function(x) {
  x_clean <- x |>
    clean_cause_of_death() |>
    str_to_lower()
  
  case_when(
    is.na(x_clean) ~ NA_character_,
    str_detect(x_clean, "suicid|committed suicide|shot himself|stabbed himself|self-inflicted|murder|assassin|assassinat|shot and killed|shot to death|stabbed to death|battle|war|gunshot|gun shot|homicide") ~ "violence",
    str_detect(x_clean, "accident|accidental|crash|plane|aircraft|airplane|automobile|car accident|car crash|motor|train|rail|drown|drowning|fall|fell|fire|burn|explosion|struck by|hit by|run over|thrown from|wreck") ~ "accident",
    str_detect(x_clean, "heart attack|acute heart|cardiovascular collapse|cardiac arrest|coronary|thrombosis|stroke|apoplexy|cerebral hemorrhage|intracerebral hemorrhage|hemorrhage|haemorrhage|aneurysm|ruptured artery|embol|convulsion|heart failure|cholera|typhoid|yellow fever|fever|influenza|grippe|pneumonia|bronchitis|hepatitis|peritonitis|erysipelas|infection|sepsis|acute gastritis|acute indigestion|catarrh|pleurisy|congestion|operation|surgery|postoperative|complications") ~ "illness_fast",
    str_detect(x_clean, "cancer|tumou?r|leukemia|lymphoma|consumption|tuberculosis|bright's disease|brights disease|nephritis|kidney|renal|liver|cirrho|diabetes|parkinson|alzheimer|dementia|huntington|als|arteriosclerosis|heart disease|heart ailment|heart trouble|heart issues|organic disease|long-standing health|long illness|general breakdown|paralysis|dropsy|gout|goiter|rheumatism|neuralgia|aids|abscess") ~ "illness_slow",
    str_detect(x_clean, "unknown|undetermined|unavailable|yet to be researched|not publicly disclosed|not announced|not reported") ~ "unclear",
    TRUE ~ "unclear"
  )
}

format_cause_of_vacancy <- function(x) {
  x_clean <- x |>
    str_replace_all("\\[[^\\]]*\\]", "") |>
    str_squish() |>
    str_to_lower()
  case_when(
    str_detect(x_clean, "\\bdied\\b|\\bdeath\\b") ~ "Died",
    str_detect(x_clean, "\\bresign") ~ "Resigned",
    str_detect(x_clean, "\\bdeclin") ~ "Declined",
    TRUE ~ NA_character_
  )
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

district_key_from_special <- function(x) {
  x |>
    str_remove("^[A-Z]{2}[[:space:]]*") |>
    str_squish() |>
    district_key_from_member()
}

safe_adist <- function(a, b) {
  if (is.na(a) || is.na(b)) {
    Inf
  } else {
    a2 <- tolower(trimws(a))
    b2 <- tolower(trimws(b))
    as.numeric(adist(a2, b2)[1])
  }
}

match_member_role <- function(events, members, role = c("original", "winner")) {
  role <- match.arg(role)
  events |>
    select(special_election_id, state, match_year, special_election_date, candidate_name_norm, candidate_last_norm, candidate_first_norm) |>
    left_join(
      members |>
        select(
          state, year, id_bioguide, id_election_bio, Special_Election_llama3,
          name_first, name_middle, name_last, name_nickname,
          name_full_norm, name_first_norm, name_last_norm,
          term_start, term_end, term_note
        ) |>
        mutate(
          year_low = year - 2,
          year_high = year + 2
        ),
      by = join_by(state, between(match_year, year_low, year_high)),
      relationship = "many-to-many"
    ) |>
    mutate(
      match_gap_days = if (role == "original") as.numeric(special_election_date - term_end) else as.numeric(term_start - special_election_date)
    ) |>
    mutate(
      candidate_window = is.na(match_gap_days) | between(match_gap_days, - 100, 365),
      match_gap_nonneg = !is.na(match_gap_days) & match_gap_days >= 0,
        lev_dist_full_name = map2_dbl(candidate_name_norm, name_full_norm, safe_adist),
        lev_dist_last_name = map2_dbl(candidate_last_norm, name_last_norm, safe_adist),
        lev_dist_first_name = map2_dbl(candidate_first_norm, name_first_norm, safe_adist)
      ) |>
      arrange(special_election_id, lev_dist_last_name, lev_dist_first_name, lev_dist_full_name, match_gap_nonneg, desc(match_gap_days)) |>
      group_by(special_election_id) |>
      slice(1) |>
      ungroup() |>
    # mutate(across(all_of(keep_cols), \(x) replace(x, !match_ok, NA))) |>
    select(
      special_election_id, id_bioguide, id_election_bio,
      state, candidate_name_norm, name_full_norm,
      term_start, term_end, term_note, Special_Election_llama3
    )
}

match_cause_of_death <- function(events, cause_data) {
  if (!"candidate_role" %in% names(events)) events <- events |> mutate(candidate_role = "original")
  if (!"special_election_id" %in% names(events)) events <- events |> mutate(special_election_id = row_number())
  if (!"id_bioguide_original" %in% names(events)) {
    events <- if ("id_bioguide" %in% names(events)) {
      events |> mutate(id_bioguide_original = id_bioguide)
    } else if ("id_bioguide_winner" %in% names(events)) {
      events |> mutate(id_bioguide_original = id_bioguide_winner)
    } else {
      events |> mutate(id_bioguide_original = NA_character_)
    }
  }
  if (!"id_election_bio" %in% names(events)) {
    events <- if ("id_election_bio_original" %in% names(events)) {
      events |> mutate(id_election_bio = id_election_bio_original)
    } else if ("id_election_bio_winner" %in% names(events)) {
      events |> mutate(id_election_bio = id_election_bio_winner)
    } else {
      events |> mutate(id_election_bio = NA_character_)
    }
  }
  if (!"term_end_original" %in% names(events)) {
    events <- if ("term_end" %in% names(events)) {
      events |> mutate(term_end_original = term_end)
    } else if ("term_end_winner" %in% names(events)) {
      events |> mutate(term_end_original = term_end_winner)
    } else {
      events |> mutate(term_end_original = as.Date(NA))
    }
  }
  if (!"name_full_norm_original" %in% names(events)) {
    events <- if ("name_full_norm" %in% names(events)) {
      events |> mutate(name_full_norm_original = name_full_norm)
    } else if ("name_full_norm_winner" %in% names(events)) {
      events |> mutate(name_full_norm_original = name_full_norm_winner)
    } else {
      events |> mutate(name_full_norm_original = NA_character_)
    }
  }
  if (!"candidate_name_norm" %in% names(events)) events <- events |> mutate(candidate_name_norm = name_full_norm_original)
  if (!"cause_of_vacancy" %in% names(events)) events <- events |> mutate(cause_of_vacancy = NA_character_)

  events |>
    filter(candidate_role == "original") |>
    transmute(
      special_election_id, id_bioguide_original, id_election_bio,
      cause_of_death_date = coalesce(parse_special_date(cause_of_vacancy), term_end_original),
      original_name_norm = coalesce(name_full_norm_original, candidate_name_norm),
      original_last_norm = str_extract(original_name_norm, "[^ ]+$"),
      original_first_norm = str_extract(original_name_norm, "^[^ ]+")
    ) |>
    left_join(
      cause_data |>
        select(
          cause_of_death_date, cause_of_death_member, cause_of_death, cause_of_death_category,
          cause_of_death_name_norm, cause_of_death_last_norm, cause_of_death_id
        ) |>
        rename(cause_of_death_date_match = cause_of_death_date) |>
        mutate(
          cause_date_low = cause_of_death_date_match - days(31),
          cause_date_high = cause_of_death_date_match + days(31)
        ),
      by = join_by(between(cause_of_death_date, cause_date_low, cause_date_high)),
      relationship = "many-to-many"
    ) |>
    mutate(
      date_gap_days = abs(as.numeric(cause_of_death_date - cause_of_death_date_match)),
      lev_dist_full_name = map2_dbl(original_name_norm, cause_of_death_name_norm, safe_adist),
      lev_dist_last_name = map2_dbl(original_last_norm, cause_of_death_last_norm, safe_adist),
      lev_dist_first_name = map2_dbl(original_first_norm, cause_of_death_name_norm |> str_extract("^[^ ]+"), safe_adist)
    ) |>
    arrange(special_election_id, lev_dist_last_name, lev_dist_first_name, lev_dist_full_name, date_gap_days) |>
    group_by(special_election_id) |>
    slice(1) |>
    ungroup() |>
    transmute(
      special_election_id, id_bioguide_original, cause_of_death_id, id_election_bio,
      cause_of_death_date = cause_of_death_date_match,
      cause_of_death_member,
      cause_of_death,
      cause_of_death_category,
      cause_of_death_match_type = "distance_date_window",
      cause_of_death_match_dist = lev_dist_full_name
    ) |>
    filter(!is.na(cause_of_death))
}


# Load Data ---------------------------------------------------------------

special_raw <- read_csv("data/fmt/special_elections.csv", show_col_types = FALSE)
member_elections <- read_csv("MP_election_data.csv")
cause_of_death_raw <- read_csv("data/fmt/cause_of_death.csv", show_col_types = FALSE)

# Data Management ---------------------------------------------------------

member_elections <- member_elections |> 
  select(-c(term_start, term_end)) |> 
  rename(
    term_start = bio_term_start,
    term_end = bio_term_end
  ) |> 
  mutate(across(
    c(term_start, term_end),
    ~lubridate::ymd(.x)
  )) |>
  mutate(state = coalesce(state_po, term_state, state_abbrev, state))

cause_of_death <- cause_of_death_raw |>
  filter(!is.na(member), !is.na(died)) |>
  transmute(
    cause_of_death_id = row_number(),
    cause_of_death_member = member,
    cause_of_death_date = parse_special_date(died),
    cause_of_death = clean_cause_of_death(cause_of_death),
    cause_of_death_category = categorize_cause_of_death(cause_of_death),
    cause_of_death_name_norm = normalize_name(member, rm_suffix = TRUE),
    cause_of_death_last_norm = str_extract(cause_of_death_name_norm, "[^ ]+$")
  ) |>
  filter(!is.na(cause_of_death_date), !is.na(cause_of_death_name_norm))

special_events_full <- special_raw |>
  transmute(
    special_election_id = row_number(),
  #  congress = con_gress_a,
    district_raw = district,
    state = str_extract(district, "^[A-Z]{2}"),
    district_key = district_key_from_special(district),
    cause_of_vacancy,
    cause_of_vacancy_fmt = format_cause_of_vacancy(cause_of_vacancy),
    special_election_date_raw = date_b_linked_to_election_article,
    special_election_date = parse_special_date(date_b_linked_to_election_article),
    original,
    winner,
    match_year = year(special_election_date)
  ) |>
  pivot_longer(
    cols = c(original, winner),
    names_to = "candidate_role",
    values_to = "candidate_text"
  ) |>
  mutate(
    candidate_name_raw = clean_candidate_name(candidate_text),
    candidate_name_norm = normalize_name(candidate_name_raw, rm_suffix = TRUE),
    candidate_last_norm = str_extract(candidate_name_norm, "[^ ]+$"),
    candidate_first_norm = str_extract(candidate_name_norm, "^[^ ]+")
  )

special_events_filtered <- 
  special_events_full |> 
  tidylog::filter(
    cause_of_vacancy != "Declined to serve", 
    cause_of_vacancy != "Disqualified",
    cause_of_vacancy != "Unseated after disqualification",
    cause_of_vacancy != "(See note)",
    !str_detect(str_to_lower(cause_of_vacancy), "vacant"),
    !str_detect(str_to_lower(cause_of_vacancy), "previous"),
    !str_detect(str_to_lower(cause_of_vacancy), "failure")
  ) |> 
  filter(
    special_election_id != 1, # Benjamin West never took his seat
    special_election_id != 3, # Edward, Pierpont was elected to the Second Congress in 1790 but declined his seat;
    special_election_id != 4, # James Townsend is not in the bio guide data
  )

original_match <- match_member_role(special_events_filtered |> filter(candidate_role == "original"), member_elections, role = "original")
winner_match <- match_member_role(special_events_filtered |> filter(candidate_role == "winner"), member_elections, role = "winner")

event_base <- special_events_filtered |>
  tidylog::left_join(original_match |>  select(-c(state, candidate_name_norm)), by = "special_election_id") |>
  tidylog::left_join(winner_match  |>  select(-c(state, candidate_name_norm)), by = "special_election_id", suffix = c("_original", "_winner")) |>
  mutate(
    vacancy_start = term_end_original,
    term_note_winner_date = extract_term_note_date(term_note_winner),
    term_start_winner = pmax(term_note_winner_date, term_start_winner, na.rm = TRUE),
    vacancy_end = term_start_winner,
    vacancy_days = as.integer(vacancy_end - vacancy_start),
    vacancy_days = if_else(vacancy_days < 0, NA_integer_, vacancy_days),
    winner_term_end_year = year(term_end_winner)
  ) |> 
  relocate(term_note_winner_date, term_start_winner, term_end_original, special_election_date, vacancy_start, vacancy_end, vacancy_days,  candidate_text, name_full_norm_original, name_full_norm_winner)

# 12 can not be matched.
sensitiv_check <- event_base |>
  filter(is.na(vacancy_days)) |>
  relocate(term_note_winner_date, term_start_winner, term_end_original, special_election_date, vacancy_start, vacancy_end, vacancy_days, candidate_text, )

cause_of_death_match <- match_cause_of_death(member_elections, cause_of_death)
cause_of_death_match_event <- match_cause_of_death(event_base, cause_of_death)

event_base <-
cause_of_death_match_event |> 
  select(special_election_id, starts_with("cause_of_death")) |> 
  right_join(event_base, by = "special_election_id")
  
member_event_base <- member_elections |> 
  tidylog::left_join(cause_of_death_match, by = c("id_election_bio"), suffix = c("", ".x")) |> 
  tidylog::left_join(event_base, by = c("id_election_bio" = "id_election_bio_winner"),  suffix = c("", ".y")) |> 
  select(-contains(".y"), -contains(".x"))

member_event_reduced <- 
  member_event_base |> 
  select(id_election_bio, id_bioguide,
         candidate_first_norm, candidate_last_norm, candidate_first_last_norm, candidate_norm,
         candidate_role, term_start, term_end, term_congress, term_party) |> 
  distinct()

write_csv(member_event_base, file = "data/fmt/member_event_base.csv")
write_csv(event_base, file = "data/fmt/event_base.csv")
write_csv(member_event_reduced, file = "data/fmt/member_event_reduced.csv")


  


