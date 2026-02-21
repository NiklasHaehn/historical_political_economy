library(tidyverse)
library(stringi)
library(lubridate)
library(fastLink)

# - Custom Functions ----

# make_house_congress_calendar <- function(congress_max = 200L) {
#   stopifnot(congress_max >= 1L)
#   
#   # Pre-20th Amendment: Congress 1 starts 1789-03-04, every 2 years
#   pre <- tibble(congress = 1:72) |>
#     mutate(
#       congress_start = ymd("1789-03-04") %m+% years(2L * (congress - 1L)),
#       congress_end   = congress_start %m+% years(2L) - days(1L)
#     )
#   
#   # Transition Congress 73 (unique end date)
#   c73 <- tibble(
#     congress = 73L,
#     congress_start = ymd("1933-03-04"),
#     congress_end   = ymd("1935-01-03")
#   )
#   
#   # Post-20th Amendment: Congress 74 starts 1935-01-03, every 2 years
#   if (congress_max >= 74L) {
#     post <- tibble(congress = 74:congress_max) |>
#       mutate(
#         congress_start = ymd("1935-01-03") %m+% years(2L * (congress - 74L)),
#         congress_end   = congress_start %m+% years(2L) - days(1L)
#       )
#     bind_rows(pre, c73, post)
#   } else if (congress_max == 73L) {
#     bind_rows(pre, c73)
#   } else {
#     pre |> filter(congress <= congress_max)
#   }
# }

# Load Data ----
mc_data <- read_csv("data/fmt/congress_member.csv")
special_elections <- read_csv("data/fmt/special_elections_csv")
election_data <- read_csv("data/fmt/election_data.csv")

# Data Management ----

# Special elections
special_elections_fmt <-
  special_elections |> 
  pivot_longer(
    cols = c(original, winner),
    names_to = "candidate_type",
    values_to = "name"
  ) |> 
  mutate(
    term_district = district |> 
      str_extract("[:space:].*") |> 
      str_trim(),
    state = district |> 
      str_remove("[:space:].*"),
    term_district = if_else(term_district == "at-large", NA, term_district),
    cd = glue::glue("{state}-{term_district}"),
    term_party = name |> 
      str_extract("\\(([^()]*)\\)") |> 
      str_remove_all("^\\(|\\)$"),
    date_char = date |> 
      str_remove("\\[.*") |> 
      str_replace("\u2013|\u2014|-", "-") |>
      str_replace("^\\s*([A-Za-z]+\\s+\\d{1,2})\\s*-\\s*\\d{1,2}\\s*,\\s*(\\d{4})\\s*$",
                  "\\1, \\2") |>
      str_squish(),
    date = as.Date(date_char, format = "%B %d, %Y"),
    full_name = name |> 
      str_remove("\\[.*") |> 
      str_remove("\\(.*") |>
      str_remove_all("\\,") |> 
      str_to_upper() |> 
      str_trim(),
    name_suffix = full_name |> 
      str_extract("(?i)\\b(Jr\\.?|Sr\\.?|II|III|IV|V|PhD|M\\.D\\.?|MD|Esq\\.?)$"),
    name_no_suffix = full_name |> 
      str_remove("(?i)\\s*(Jr\\.?|Sr\\.?|II|III|IV|V|PhD|M\\.D\\.?|MD|Esq\\.?)$") |>
      str_squish(),
    name_first = name_no_suffix |> 
      str_extract( "^[^ ]+") |> 
      str_to_upper(),
    name_last = name_no_suffix |> 
      str_extract("[^ ]+$") |> 
      str_to_upper(),
    name_middle = name_no_suffix |>
      str_remove("^[^ ]+\\s*") |>
      str_remove("\\s*[^ ]+$") |>
      str_to_upper() |> 
      na_if(""),
    year = lubridate::year(date),
    year_char = as.character(year),
  ) |> 
  filter(date >= as.Date("1920-01-01"))

# House only
mc_data_fmt <- 
  mc_data |> 
  select(
    id_bioguide, id_house_history, # id
    name_first, name_last, name_middle, name_nickname, name_suffix, # name
    bio_birthday, bio_gender, # standard covariates
    term_type, term_start, term_end, term_class, 
    term_party,
    term_state, term_district,
  ) |>
  mutate(
    across(
      starts_with("name"),
      \(x) x |>
        stringi::stri_trans_general("Latin-ASCII") |>
        str_remove_all("\\,") |> 
        str_squish() |> 
        str_to_upper()
    ),
    term_start_date = lubridate::ymd(term_start), 
    term_end_date  = lubridate::ymd(term_end),
    cd = glue::glue("{term_state}-{term_district}")
  ) |>
  mutate(number_start_terms = n(), .by = term_start_date) |> 
  mutate(number_end_terms = n(), .by = term_end_date) |> 
  mutate(
    year = lubridate::year(term_start_date) -1,
    year_char = as.character(year),
    state = term_state
    ) |> 
  filter(
    term_type == "rep",
    term_start_date >= as.Date("1920-01-01") # Time Frame
  ) |> 
  arrange(term_start_date, id_bioguide) 

election_data_fmt <- 
  election_data |> 
  mutate(
    full_name = candidate |> 
      str_remove_all("\\,") |> 
      str_to_upper() |> 
      str_squish(),
    name_suffix = full_name |> 
      str_extract("(?i)\\b(Jr\\.?|Sr\\.?|II|III|IV|V|PhD|M\\.D\\.?|MD|Esq\\.?)$"),
    name_no_suffix = full_name |> 
      str_remove("(?i)\\s*(Jr\\.?|Sr\\.?|II|III|IV|V|PhD|M\\.D\\.?|MD|Esq\\.?)$") |>
      str_squish(),
    name_first = name_no_suffix |> 
      str_extract( "^[^ ]+"),
    name_last = name_no_suffix |> 
      str_extract("[^ ]+$"),
    name_middle = name_no_suffix |>
      str_remove("^[^ ]+\\s*") |>
      str_remove("\\s*[^ ]+$") |>
      na_if(""),
    cd = glue::glue("{state_po}-{district}"),
    year_char = as.character(year),
    term_party = party |> 
      str_extract("[:alpha:]{1}")
  ) |> 
  select(-c(state)) |> 
  rename(
    state = state_po
  )

# Combine Data ------------------------------------------------------------

match_with_fastLink <- function(
    df1,
    df2,
    fix_varnames,
    matching_vars,
    hard_code = TRUE,
    thresh = 0.90
) {
 # browser()
  dfA <- df1 |> dplyr::mutate(A_index = dplyr::row_number())
  dfB <- df2 |> dplyr::mutate(B_index = dplyr::row_number())
  
  fl <- fastLink::fastLink(
    dfA              = dfA,
    dfB              = dfB,
    varnames         = c(fix_varnames, matching_vars),
    stringdist.match = matching_vars,
    partial.match    = matching_vars,
    return.df        = FALSE
  )
  
  matches <- fastLink::getMatches(
    dfA             = dfA,
    dfB             = dfB,
    fl.out          = fl,
    threshold.match = thresh
  )
  
  matches |> 
    group_by(B_index) |> 
    filter(posterior == max(posterior)) |> 
    ungroup()
}


mc_special_election_data <-
  election_data_fmt |>
  match_with_fastLink(
    df2          = special_elections_fmt |> filter(candidate_type == "winner"),
    fix_varnames = c("state", "year", "term_party"),
    matching_vars = c("name_first", "name_last")
  ) |> 
  rename(posterior_1 = posterior) |> 
  match_with_fastLink(
    df2          = mc_data_fmt,
    fix_varnames = c("state", "year", "term_party"),
    matching_vars = c("name_first", "name_last")
  )

# Save data ----
write_csv(mc_data_fmt, file = "data/fmt/congress_member.csv")
write_csv(special_elections, file = "data/fmt/special_elections.csv")