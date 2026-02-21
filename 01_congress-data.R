library(tidyverse)
library(yaml)
library(wikiscrapeR)
library(rvest)


# Custom Functions --------------------------------------------------------

drop_nulls <- function(x) purrr::compact(x)

repeat_row <- function(df, n) {
  if (n == 0) df[0, , drop = FALSE] else df[rep(1, n), , drop = FALSE]
}

process_mp_data <- function(data_yaml){
out <-
  data_yaml |>
  purrr::map(\(leg) {
    
    id_list   <- leg$id   %||% list()
    name_list <- leg$name %||% list()
    bio_list  <- leg$bio  %||% list()
    terms     <- leg$terms %||% list()
    
    base_df <-
      dplyr::bind_cols(
        tibble::as_tibble(purrr::compact(id_list))   |> dplyr::rename_with(\(x) paste0("id_", x)),
        tibble::as_tibble(purrr::compact(name_list)) |> dplyr::rename_with(\(x) paste0("name_", x)),
        tibble::as_tibble(purrr::compact(bio_list))  |> dplyr::rename_with(\(x) paste0("bio_", x))
      )
    
    terms_df <-
      terms |>
      purrr::map(\(trm) tibble::as_tibble(purrr::compact(trm))) |>
      dplyr::bind_rows() |>
      dplyr::rename_with(\(x) paste0("term_", x))
    
    dplyr::bind_cols(repeat_row(base_df, nrow(terms_df)), terms_df)
  }) |>
  dplyr::bind_rows() |>
  dplyr::mutate(
    term_start   = as.Date(term_start),
    term_end     = as.Date(term_end),
    bio_birthday = as.Date(bio_birthday)
  )

return(out)
}


# Download Data  ----------------------------------------------------------
# scrape Wikipedia
# https://en.wikipedia.org/wiki/List_of_special_elections_to_the_United_States_House_of_Representatives

url <- "https://en.wikipedia.org/wiki/List_of_special_elections_to_the_United_States_House_of_Representatives"
# Table 1
wiki_tables <- get_wikipedia_tables(url) |> 
  janitor::clean_names() |> 
  rename(
    congress = con_gress_a,
    date = date_b_linked_to_election_article,
  )

links_all <- function(url) {
  page <- read_html(url)
  
  page |>
    html_elements("a") |>
    (\(a) tibble::tibble(
      date = a |> html_text2() |> str_squish(),
      href = a |> html_attr("href")
    ))() |>
    filter(!is.na(href), href != "")
}

# Get data from github https://github.com/unitedstates/congress-legislators
hist_congress_member <- read_yaml("https://unitedstates.github.io/congress-legislators/legislators-historical.yaml")
current_congress_member <- read_yaml("https://unitedstates.github.io/congress-legislators/legislators-current.yaml")

mc_hist <- process_mp_data(hist_congress_member)
mc_current <- process_mp_data(current_congress_member)

# MIT Election Data
elections_1920_1974 <- read_csv("https://raw.githubusercontent.com/SaiChrisZHANG/us-congress-1920-1974/refs/heads/main/1920-1974-house.csv")

# Combine Data ------------------------------------------------------------

lines <-
  read_lines("data/raw/MIT_election_data/1976-2024-house.tab")

  csv_txt <- c(
    lines[1],                              # header (already fine)
    str_replace(lines[-1], '^"(.*)"$', "\\1")  # strip outer quotes from each row
  ) |>
  paste(collapse = "\n")

elections_1974_2024 <- read_csv(I(csv_txt), show_col_types = FALSE) |> 
  mutate(
    candidate = candidate |>
      str_replace_all('\\\\\"', '"') |>
      str_replace_all('""', '"') |> 
      str_remove_all('\\"') |>
      str_remove_all('\\\\"') |> 
      str_remove_all('\\\\')
  )

mc_combined <- mc_current
  bind_rows(mc_hist)
  
election_data_combined <- elections_1920_1974 |> 
  mutate(
    writein = as.character(writein),
    candidatevotes = as.character(candidatevotes),
    unofficial = as.character(unofficial),
    fusion_ticket = as.character(fusion_ticket),
    version = as.character(version)
    ) |> 
  bind_rows(elections_1974_2024)

# Save Data
write_csv(mc_combined, file = "data/fmt/congress_member.csv")
write_csv(wiki_tables, file = "data/fmt/special_elections.csv")
write_csv(election_data_combined, file = "data/fmt/election_data.csv")

# Snyder_data <- read_csv("Dropbox/Yale-Predoc/Shiro/SAY_ticketsplit/data/by-district_hist-elec.csv")
# load("data/raw/ICPSR_00001/DS0200/00001-0200-Data.rda")
