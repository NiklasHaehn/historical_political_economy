library(tidyverse)
library(yaml)
library(rvest)
library(polite)


# Custom Functions --------------------------------------------------------

get_wiki_table <- function(url, which = NULL, agent = NULL){
  
  if(is.null(agent)){
    session <- bow(url = url,
                     user_agent = "Scraping Agent, 
               Niklas H., 
               Department of Political Science,
               UCSD")
  } else {
    session <- bow(url = url,
                   user_agent = agent)
  }
  
  if(!is.null(session)){
    message(glue::glue("{url}: Success!"))
  } else {
    message(glue::glue("{url}: Failed!"))
  }
  
  page <- scrape(session)
  page_html <- page |> html_table()
  
  if(is.null(which)){
    return(page_html)
  }
  
  page_html_sub  <-  map(
    .x = which,
    .f = 
      \(i) {
        page_html_sub <- page_html[[i]]
        page_html_sub
    }
    ) |> bind_rows()

  return(page_html_sub |> janitor::clean_names())
}

drop_nulls <- function(x) purrr::compact(x)

parse_member_date <- function(x) {
  x_chr <- x |>
    as.character() |>
    str_squish() |>
    na_if("")
  mm_yyyy <- str_match(x_chr, "^(\\d{1,2})[/-](\\d{4})$")
  yyyy_mm <- str_match(x_chr, "^(\\d{4})-(\\d{1,2})$")
  fallback <- case_when(
    !is.na(mm_yyyy[, 1]) ~ glue::glue("{mm_yyyy[, 3]}-{str_pad(mm_yyyy[, 2], 2, side = 'left', pad = '0')}-01"),
    !is.na(yyyy_mm[, 1]) ~ glue::glue("{yyyy_mm[, 2]}-{str_pad(yyyy_mm[, 3], 2, side = 'left', pad = '0')}-01"),
    TRUE ~ NA_character_
  )
  full_date <- lubridate::parse_date_time(x_chr, orders = c("Y-m-d", "Y/m/d", "m/d/Y", "d/m/Y"), quiet = TRUE) |>
    lubridate::as_date()
  fallback_date <- lubridate::ymd(fallback, quiet = TRUE) |>
    lubridate::ceiling_date("month") -
    lubridate::days(1)
  coalesce(full_date, fallback_date)
}

process_mp <- function(data_yaml){
  out <- purrr::map2(data_yaml, seq_along(data_yaml),\(leg, i) {
    id_list <- leg$id %||% list() 
    name_list <- leg$name %||% list() 
    bio_list <- leg$bio %||% list() 
    terms <- leg$terms %||% list() 
    base_df <- dplyr::bind_cols( 
      tibble::as_tibble(purrr::compact(id_list)) |>
        dplyr::rename_with(\(x) paste0("id_", x)),
      tibble::as_tibble(purrr::compact(name_list)) |>
        dplyr::rename_with(\(x) paste0("name_", x)),
      tibble::as_tibble(purrr::compact(bio_list)) |>
        dplyr::rename_with(\(x) paste0("bio_", x)) 
    ) 
    terms_df <- terms |> 
      purrr::map(\(trm) tibble::as_tibble(purrr::compact(trm))) |>
      dplyr::bind_rows() |>
      dplyr::rename_with(\(x) paste0("term_", x))
    dplyr::bind_cols(repeat_row(base_df, nrow(terms_df)), terms_df) |>
      dplyr::mutate(list_nr = i, .before = 1) }) |>
    dplyr::bind_rows() |>
    mutate(across(any_of(c("term_start", "term_end", "bio_birthday", "bio_deathday")), parse_member_date))
  return(out) 
}

repeat_row <- function(df, n) {
  if (n == 0) df[0, , drop = FALSE] else df[rep(1, n), , drop = FALSE]
}

# ---- 1) Fast "one legislator -> many term-rows" ----
process_mp_data <- function(data_yaml, flatten = FALSE) {
  
  prefix_names <- function(x, prefix) {
    x <- purrr::compact(x %||% list())
    if (length(x) == 0) return(list())
    nms <- names(x) %||% rep("", length(x))
    names(x) <- ifelse(nms == "", glue::glue("{prefix}{seq_along(x)}"), glue::glue("{prefix}{nms}"))
    x
  }
  
  maybe_flatten <- function(x) {
    if (!flatten || !is.data.frame(x)) return(x)
    if (length(x) == 0) return(x)
    jsonlite::flatten(x)
  }
  
  out <-
    map2(data_yaml, seq_along(data_yaml), \(leg, i) {
      
      id_list   <- leg$id   %||% list()
      name_list <- leg$name %||% list()
      bio_list  <- leg$bio  %||% list()
      terms     <- leg$terms %||% list()
      
      # Base info: 1 row
      base_list <-
        c(
          prefix_names(maybe_flatten(id_list),   "id_"),
          prefix_names(maybe_flatten(name_list), "name_"),
          prefix_names(maybe_flatten(bio_list),  "bio_")
        )
      
      base_df <-
        base_list |>
        tibble::as_tibble_row() |>
        mutate(across(everything(), as.character))
      
      # Terms: 1 row per term (FAST: bind_rows can bind named lists directly)
      terms_df <-
        terms |>
        map(\(trm) purrr::compact(maybe_flatten(trm %||% list()))) |>
        dplyr::bind_rows()
      
      if (ncol(terms_df) > 0) {
        terms_df <- terms_df |> mutate(across(everything(), as.character))
      }
      
      if (ncol(terms_df) > 0) {
        terms_df <- terms_df |> dplyr::rename_with(\(x) paste0("term_", x))
      }
      
      n_terms <- nrow(terms_df)
      
      if (n_terms == 0) {
        base_df |>
          mutate(list_nr = i, .before = 1)
      } else {
        base_rep <- base_df[rep.int(1L, n_terms), , drop = FALSE]
        bind_cols(base_rep, terms_df) |>
          mutate(list_nr = i, .before = 1)
      }
    }) |>
    bind_rows() |>
    mutate(
      across(any_of(c("term_start", "term_end", "bio_birthday", "bio_deathday")), parse_member_date)
    )
  
  out
}

# ---- 2) Read all JSON files, normalize to "list of legislators", then process ----
normalize_bioguide_leg <- function(x) {
  norm_term <- function(jp) {
    party <- jp |> purrr::pluck("congressAffiliation", "partyAffiliation", 1, "party", "name", .default = NA_character_)
    caucus <- jp |> purrr::pluck("congressAffiliation", "caucusAffiliation", 1, "party", "name", .default = NA_character_)
    
    list(
      type = jp |> purrr::pluck("job", "name", .default = NA_character_),
      start = jp$startDate %||% (jp |> purrr::pluck("congressAffiliation", "congress", "startDate", .default = NA_character_)),
      end = jp$endDate %||% (jp |> purrr::pluck("congressAffiliation", "congress", "endDate", .default = NA_character_)),
      note = jp |> purrr::pluck("congressAffiliation", "note", 1, "content", .default = NA_character_),
      party = ifelse(is.na(party), caucus, party),
      state = jp |> purrr::pluck("congressAffiliation", "represents", "regionCode", .default = NA_character_),
      district = jp |> purrr::pluck("congressAffiliation", "represents", "district", .default = NA_character_),
      congress = jp |> purrr::pluck("congressAffiliation", "congress", "congressNumber", .default = NA_integer_),
      departure_reason = jp$departureReason %||% (jp |> purrr::pluck("congressAffiliation", "departureReason", .default = NA_character_)),
      job_type = jp |> purrr::pluck("job", "jobType", .default = NA_character_)
    ) |>
      purrr::compact()
  }
  
  list(
    id = list(bioguide = x$usCongressBioId %||% NA_character_),
    name = list(
      first = x$givenName %||% NA_character_,
      middle = x$middleName %||% NA_character_,
      last = x$familyName %||% NA_character_,
      suffix = x$honorificSuffix %||% NA_character_
    ) |> purrr::compact(),
    bio = list(
      birthday = x$birthDate %||% NA_character_,
      deathday = x$deathDate %||% NA_character_,
      profile_text = x$profileText %||% NA_character_
    ) |> purrr::compact(),
    terms = (x$jobPositions %||% list()) |> purrr::map(norm_term)
  )
}

read_bioguide_file <- function(file) {
  x <- jsonlite::read_json(file, simplifyVector = FALSE)
  
  # Normalize two common formats:
  # (a) single legislator object (has $terms or $jobPositions)
  # (b) list/array of legislator objects
  if (is.list(x) && !is.null(x$terms)) {
    return(list(x))
  }
  
  if (is.list(x) && !is.null(x$jobPositions)) {
    return(list(normalize_bioguide_leg(x)))
  }
  
  if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && !is.null(x[[1]]$terms)) {
    return(x)
  }
  
  if (is.list(x) && length(x) > 0 && is.list(x[[1]]) && !is.null(x[[1]]$jobPositions)) {
    return(x |> purrr::map(normalize_bioguide_leg))
  }
  
  # Otherwise: unknown/empty
  list()
}

process_bio_guide <- function(path = "data/raw/bio_guides",
                              parallel = TRUE,
                              workers = max(1L, future::availableCores() - 1L),
                              flatten = FALSE) {
  
  files <- list.files(path, pattern = "\\.json$", recursive = TRUE, full.names = TRUE)
  
  if (parallel) {
    legs <- tryCatch(
      {
        suppressPackageStartupMessages({
          library(future)
          library(furrr)
        })
        oplan <- future::plan()
        on.exit(future::plan(oplan), add = TRUE)
        
        future::plan(future::multisession, workers = workers)
        
        files |>
          furrr::future_map(read_bioguide_file, .progress = TRUE) |>
          purrr::list_flatten()
      },
      error = \(e) {
        message("parallel read failed; fallback to sequential")
        files |>
          map(read_bioguide_file) |>
          purrr::list_flatten()
      }
    )
  } else {
    legs <-
      files |>
      map(read_bioguide_file) |>
      purrr::list_flatten()
  }
  
  # One row per (legislator x term)
  df <- process_mp_data(legs, flatten = flatten)
  
  # Optional: attach file metadata (if you want it, you can re-map with imap instead)
  df
}

# ---- usage ----
bg_df <- process_bio_guide(
  path     = "data/raw/bio_guides",
  parallel = TRUE,
  workers  = 6,       # pick something sensible for your machine
  flatten  = FALSE    # TRUE gives wider columns but can be slower
)

#files <- jsonlite::read_json("data/raw/bio_guides/BioguideProfiles/K000286.json")

# Download Data  ----------------------------------------------------------
# Scrape Wikipedia Special elections
# https://en.wikipedia.org/wiki/List_of_special_elections_to_the_United_States_House_of_Representatives
special_elctions_wiki <- get_wiki_table("https://en.wikipedia.org/wiki/List_of_special_elections_to_the_United_States_House_of_Representatives", 2)

# Table 1
cause_of_death_1790_1899 <- get_wiki_table("https://en.wikipedia.org/wiki/List_of_members_of_the_United_States_Congress_who_died_in_office_(1790%E2%80%931899)", c(2:3)) |> 
  select(1:15)
cause_of_death_1900_1949 <- get_wiki_table("https://en.wikipedia.org/wiki/List_of_members_of_the_United_States_Congress_who_died_in_office_(1900%E2%80%931949)", c(2:6)) |> 
  select(1:14)
cause_of_death_1950_1999 <- get_wiki_table("https://en.wikipedia.org/wiki/List_of_members_of_the_United_States_Congress_who_died_in_office_(1950%E2%80%931999)", c(2:6))
cause_of_death_2000_today <- get_wiki_table("https://en.wikipedia.org/wiki/List_of_members_of_the_United_States_Congress_who_died_in_office_(2000%E2%80%93present)", c(2:4))

harmonize_cause_of_death <- function(df){
  rx <- c(
    member = "^(member|name|representative|senator)(_\\d+)?$",
    party = "^(party|political_party)(_\\d+)?$",
    state = "^(state|state_or_territory|district_or_territory|district|territory)(_\\d+)?$",
    chamber = "^(chamber|office|house_senate|house_or_senate)(_\\d+)?$",
    congress = "^(congress|congress_no|congress_number|u_s_con_gress|vte_united_states_congress_1|vte_united_states_congress_2|u_s_congress)(_\\d+)?$",
    died = "^(died|date_of_death|death_date|date)(_\\d+)?$",
    cause_of_death = "^(cause_of_death|cause)(_\\d+)?$",
    notes = "^(notes?|comments?|circumstances)(_\\d+)?$"
  )
  cols <- rx |> map(\(p) names(df)[str_detect(names(df), p)]) |> keep(\(x) length(x) > 0)
  names(cols) |>
    reduce(
      .init = df |> mutate(across(everything(), as.character)),
      .f = \(acc, nm) acc |> mutate("{nm}" := coalesce(!!!acc[cols[[nm]]]))
    ) |>
    select(-any_of(unlist(cols) |> setdiff(names(cols))))
}

cause_of_death <-
  list(
    cause_of_death_1790_1899,
    cause_of_death_1900_1949,
    cause_of_death_1950_1999,
    cause_of_death_2000_today
  ) |>
  map(harmonize_cause_of_death) |>
  bind_rows()

# Get data from github https://github.com/unitedstates/congress-legislators
hist_congress_member <- read_yaml("https://unitedstates.github.io/congress-legislators/legislators-historical.yaml")
current_congress_member <- read_yaml("https://unitedstates.github.io/congress-legislators/legislators-current.yaml")

mc_hist <- process_mp(hist_congress_member)
mc_current <- process_mp(current_congress_member)

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

mc_combined <- mc_current |> 
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
write_csv(cause_of_death, file = "data/fmt/cause_of_death.csv")
write_csv(mc_combined, file = "data/fmt/congress_member.csv")
write_csv(bg_df, file = "data/fmt/bio_guides.csv")
write_csv(special_elctions_wiki, file = "data/fmt/special_elections.csv")
write_csv(election_data_combined, file = "data/fmt/election_data.csv")

# Snyder_data <- read_csv("Dropbox/Yale-Predoc/Shiro/SAY_ticketsplit/data/by-district_hist-elec.csv")
# load("data/raw/ICPSR_00001/DS0200/00001-0200-Data.rda")

data <- read_csv("data/fmt/bio_guides.csv")

