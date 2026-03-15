library(tidyverse)
library(lubridate)
library(stringi)
library(glue)

cfg <- list(
  articles_path = Sys.getenv("TDM_ARTICLES_PATH", "data/raw/tdm_articles.csv"),
  mps_path = Sys.getenv("TDM_MPS_PATH", "data/fmt/MP_data.csv"),
  article_out = Sys.getenv("TDM_ARTICLE_OUT", "data/fmt/tdm_article_party_slant.csv"),
  weekly_out = Sys.getenv("TDM_WEEKLY_OUT", "data/fmt/tdm_weekly_outlet_slant.csv"),
  weekly_wide_out = Sys.getenv("TDM_WEEKLY_WIDE_OUT", "data/fmt/tdm_weekly_outlet_slant_wide.csv"),
  id_col = Sys.getenv("TDM_ID_COL", "article_id"),
  outlet_col = Sys.getenv("TDM_OUTLET_COL", "outlet"),
  date_col = Sys.getenv("TDM_DATE_COL", "date"),
  title_col = Sys.getenv("TDM_TITLE_COL", "title"),
  text_col = Sys.getenv("TDM_TEXT_COL", "text")
)

norm_text <- function(x) {
  x |>
    coalesce("") |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_upper() |>
    str_remove_all("\\[[^\\]]*\\]") |>
    str_remove_all("\\([^\\)]*\\)") |>
    str_remove_all("[^A-Z0-9 ]") |>
    str_squish() |>
    na_if("")
}

parse_date <- function(x) {
  parse_date_time(
    x |> as.character(),
    orders = c("Y-m-d", "Y/m/d", "m/d/Y", "d/m/Y", "Ymd", "Y-m-d H:M:S", "m/d/Y H:M:S"),
    quiet = TRUE
  ) |>
    as_date()
}

escape_rx <- function(x) {
  specials <- c("\\", ".", "|", "(", ")", "[", "]", "{", "}", "^", "$", "*", "+", "?")
  x |>
    map_chr(
      \(s) {
        if (is.na(s)) return(NA_character_)
        s |>
          str_split("", simplify = FALSE) |>
          pluck(1) |>
          map_chr(\(ch) if (ch %in% specials) glue("\\\\{ch}") else ch) |>
          str_c(collapse = "")
      }
    )
}

articles_raw <- read_csv(cfg$articles_path, show_col_types = FALSE, progress = FALSE)

required_article_cols <- c(cfg$id_col, cfg$outlet_col, cfg$date_col, cfg$text_col)
missing_article_cols <- setdiff(required_article_cols, names(articles_raw))
if (length(missing_article_cols) > 0) {
  stop(glue("Missing article columns in {cfg$articles_path}: {str_c(missing_article_cols, collapse = ', ')}"))
}

title_vec <- if (cfg$title_col %in% names(articles_raw)) {
  articles_raw[[cfg$title_col]]
} else {
  rep(NA_character_, nrow(articles_raw))
}

articles <- tibble(
  article_id = articles_raw[[cfg$id_col]] |> as.character(),
  outlet = articles_raw[[cfg$outlet_col]] |> as.character(),
  article_date = parse_date(articles_raw[[cfg$date_col]]),
  title = title_vec |> as.character(),
  text = articles_raw[[cfg$text_col]] |> as.character()
) |>
  mutate(
    article_id = coalesce(article_id |> na_if(""), glue("article_{row_number()}")),
    outlet = coalesce(outlet |> na_if(""), "UNKNOWN_OUTLET"),
    text_norm = glue("{coalesce(title, '')} {coalesce(text, '')}") |> norm_text(),
    week = floor_date(article_date, unit = "week", week_start = 1)
  )

mps_raw <- read_csv(cfg$mps_path, show_col_types = FALSE, progress = FALSE)
if ("term_type" %in% names(mps_raw)) {
  mps_raw <- mps_raw |> filter(term_type == "Representative")
}

required_mps_cols <- c("term_congress", "term_start", "term_end", "term_party", "name_first", "name_last")
missing_mps_cols <- setdiff(required_mps_cols, names(mps_raw))
if (length(missing_mps_cols) > 0) {
  stop(glue("Missing MP columns in {cfg$mps_path}: {str_c(missing_mps_cols, collapse = ', ')}"))
}

mps_n <- nrow(mps_raw)
first_norm_col <- if ("name_first_norm" %in% names(mps_raw)) mps_raw$name_first_norm else rep(NA_character_, mps_n)
last_norm_col <- if ("name_last_norm" %in% names(mps_raw)) mps_raw$name_last_norm else rep(NA_character_, mps_n)
nick_norm_col <- if ("name_nickname_norm" %in% names(mps_raw)) mps_raw$name_nickname_norm else rep(NA_character_, mps_n)
first_last_norm_col <- if ("name_first_last_norm" %in% names(mps_raw)) mps_raw$name_first_last_norm else rep(NA_character_, mps_n)

mps <- mps_raw |>
  mutate(
    term_congress = as.integer(term_congress),
    term_start_date = ymd(term_start, quiet = TRUE),
    term_end_date = ymd(term_end, quiet = TRUE),
    term_party = coalesce(term_party |> as.character(), "Unknown"),
    first_norm = coalesce(first_norm_col, norm_text(name_first)),
    last_norm = coalesce(last_norm_col, norm_text(name_last)),
    nick_norm = coalesce(nick_norm_col, norm_text(name_nickname)),
    first_last_norm = coalesce(
      first_last_norm_col,
      str_squish(glue("{first_norm} {last_norm}")) |> na_if("")
    ),
    nick_last_norm = if_else(
      is.na(nick_norm),
      NA_character_,
      str_squish(glue("{nick_norm} {last_norm}")) |> na_if("")
    )
  ) |>
  filter(!is.na(term_congress), !is.na(term_start_date), !is.na(term_end_date), !is.na(last_norm))

max_end <- mps |> summarise(max_end = max(term_end_date, na.rm = TRUE)) |> pull(max_end)

congress_bounds <- mps |>
  group_by(term_congress) |>
  summarise(congress_start = min(term_start_date, na.rm = TRUE), .groups = "drop") |>
  arrange(term_congress) |>
  mutate(congress_end = coalesce(lead(congress_start) - days(1L), max_end))

articles <- articles |>
  left_join(
    congress_bounds,
    by = join_by(between(article_date, congress_start, congress_end))
  ) |>
  select(-congress_start, -congress_end)

weather_sport_rx <- "\\b(?:WEATHER|FORECAST|TEMPERATURE|RAIN|SNOW|STORM|HUMIDITY|WIND|BASEBALL|FOOTBALL|BASKETBALL|SOCCER|HOCKEY|TENNIS|GOLF|TOURNAMENT|PLAYOFF|SCORE|TEAM|COACH|LEAGUE)\\b"
politics_hint_rx <- "\\b(?:CONGRESS|HOUSE|CONGRESSMAN|CONGRESSWOMAN|REPRESENTATIVE|DEMOCRAT|REPUBLICAN|FEDERALIST|WHIG|ELECTION|VOTE|BILL|LEGISLATION|WASHINGTON)\\b"
title_rx <- "CONGRESSMAN|CONGRESSWOMAN|REPRESENTATIVE|REP|HONORABLE|HON|MR|MRS|MS"

articles <- articles |>
  mutate(
    skip_fast = is.na(text_norm) |
      text_norm == "" |
      (
        str_detect(text_norm, weather_sport_rx) &
          !str_detect(text_norm, politics_hint_rx)
      )
  )

mp_patterns <- mps |>
  transmute(
    term_congress,
    term_party,
    mp_key = glue("{id_bioguide}_{term_start}"),
    last_norm,
    first_last_norm,
    nick_last_norm
  ) |>
  distinct() |>
  mutate(
    name_pattern = pmap_chr(
      list(first_last_norm, nick_last_norm, last_norm),
      \(full, nick, last) c(
        if (!is.na(full)) glue("\\b{escape_rx(full)}\\b") else NA_character_,
        if (!is.na(nick) && nick != full) glue("\\b{escape_rx(nick)}\\b") else NA_character_,
        glue("\\b(?:{title_rx})\\s+{escape_rx(last)}\\b")
      ) |>
        discard(\(z) is.na(z) || z == "") |>
        str_c(collapse = "|")
    )
  )

mp_by_congress <- mp_patterns |>
  group_split(term_congress) |>
  (\(x) set_names(x, map_chr(x, \(tbl) as.character(tbl$term_congress[[1]]))))()

count_party_mentions <- function(text_norm, term_congress, skip_fast) {
  if (isTRUE(skip_fast) || is.na(term_congress) || is.na(text_norm) || text_norm == "") {
    return(tibble(term_party = character(), mentions = integer()))
  }
  mp_tbl <- mp_by_congress[[as.character(term_congress)]]
  if (is.null(mp_tbl) || nrow(mp_tbl) == 0) {
    return(tibble(term_party = character(), mentions = integer()))
  }
  tokens <- text_norm |>
    str_split("\\s+", simplify = FALSE) |>
    pluck(1) |>
    unique()
  candidate_mps <- mp_tbl |> filter(last_norm %in% tokens)
  if (nrow(candidate_mps) == 0) {
    return(tibble(term_party = character(), mentions = integer()))
  }
  candidate_mps |>
    mutate(mentions = map_int(name_pattern, \(rx) str_count(text_norm, regex(rx)))) |>
    filter(mentions > 0) |>
    summarise(mentions = sum(mentions), .by = term_party)
}

party_hits <- pmap(
  list(articles$text_norm, articles$term_congress, articles$skip_fast),
  count_party_mentions
) |>
  map2(articles$article_id, \(x, aid) x |> mutate(article_id = aid)) |>
  bind_rows()

article_base <- articles |>
  filter(!is.na(term_congress), !is.na(week), !is.na(article_date)) |>
  select(article_id, outlet, article_date, week, term_congress, skip_fast)

article_parties <- article_base |>
  inner_join(
    mp_patterns |> distinct(term_congress, term_party),
    by = "term_congress",
    relationship = "many-to-many"
  ) |>
  left_join(party_hits, by = c("article_id", "term_party")) |>
  mutate(mentions = coalesce(mentions, 0L)) |>
  group_by(article_id) |>
  mutate(
    total_mentions = sum(mentions),
    party_share = if_else(total_mentions > 0, mentions / total_mentions, 0)
  ) |>
  ungroup() |>
  arrange(article_date, outlet, article_id, desc(mentions), term_party)

weekly_totals <- article_parties |>
  distinct(article_id, outlet, week, term_congress, total_mentions) |>
  summarise(
    n_articles = n(),
    n_political_articles = sum(total_mentions > 0),
    total_mentions = sum(total_mentions),
    .by = c(outlet, week, term_congress)
  )

weekly_slant <- article_parties |>
  summarise(
    party_mentions = sum(mentions),
    avg_article_share = mean(party_share),
    .by = c(outlet, week, term_congress, term_party)
  ) |>
  left_join(weekly_totals, by = c("outlet", "week", "term_congress")) |>
  mutate(weekly_party_share = if_else(total_mentions > 0, party_mentions / total_mentions, 0)) |>
  arrange(week, outlet, term_congress, desc(weekly_party_share), term_party)

weekly_slant_wide <- weekly_slant |>
  select(outlet, week, term_congress, term_party, weekly_party_share) |>
  pivot_wider(
    names_from = term_party,
    values_from = weekly_party_share,
    names_prefix = "slant_",
    values_fill = 0
  )

dir.create(dirname(cfg$article_out), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(cfg$weekly_out), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(cfg$weekly_wide_out), recursive = TRUE, showWarnings = FALSE)

write_csv(article_parties, cfg$article_out)
write_csv(weekly_slant, cfg$weekly_out)
write_csv(weekly_slant_wide, cfg$weekly_wide_out)

message(glue("Article-level output: {cfg$article_out}"))
message(glue("Weekly long output: {cfg$weekly_out}"))
message(glue("Weekly wide output: {cfg$weekly_wide_out}"))
message(glue("Articles with valid congress/date: {n_distinct(article_parties$article_id)}"))
