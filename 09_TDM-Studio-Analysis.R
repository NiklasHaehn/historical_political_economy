library(tidyverse)
library(lubridate)
library(stringi)
library(glue)
library(xml2)
library(rvest)

cfg <- list(
  xml_paths = Sys.getenv("TDM_XML_PATHS", ""),
  xml_dir = Sys.getenv("TDM_XML_DIR", "data/raw"),
  articles_path = Sys.getenv("TDM_ARTICLES_PATH", "data/raw/tdm_studio_articles.csv"),
  mps_path = Sys.getenv("TDM_MPS_PATH", "data/fmt/member_event_reduced.csv"),
  party_lookup_path = Sys.getenv("TDM_PARTY_LOOKUP_PATH", "MP_election_data.csv"),
  article_out = Sys.getenv("TDM_ARTICLE_OUT", "data/fmt/09_tdm_article_party_slant.csv"),
  weekly_out = Sys.getenv("TDM_WEEKLY_OUT", "data/fmt/09_tdm_weekly_outlet_slant.csv"),
  weekly_wide_out = Sys.getenv("TDM_WEEKLY_WIDE_OUT", "data/fmt/09_tdm_weekly_outlet_slant_wide.csv")
)

clean_nm <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

pick_col <- function(data, candidates, required = TRUE, label = "column") {
  cols <- intersect(candidates, names(data))
  if (length(cols) == 0 && required) {
    stop(glue("Missing {label}. Tried: {str_c(candidates, collapse = ', ')}"))
  }
  if (length(cols) == 0) NA_character_ else cols[[1]]
}

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

strip_html_tags <- function(text) {
  if (is.na(text) || text == "") return(NA_character_)
  tryCatch(
    text |>
      read_html() |>
      html_text2() |>
      str_replace_all("\n", " ") |>
      str_replace_all("\\\\", " ") |>
      str_squish(),
    error = \(e) text |> str_replace_all("<[^>]+>", " ") |> str_squish()
  )
}

get_node_text <- function(doc, xpaths) {
  values <- xpaths |>
    map(
      \(xp) {
        node <- xml_find_first(doc, xp)
        if (inherits(node, "xml_missing")) NA_character_ else xml_text(node)
      }
    ) |>
    unlist(use.names = FALSE) |>
    str_squish() |>
    discard(\(z) is.na(z) || z == "")
  if (length(values) == 0) NA_character_ else values[[1]]
}

read_xml_article <- function(path) {
  tryCatch(
    {
      doc <- read_xml(path)
      raw_text <- get_node_text(doc, c(".//FullText", ".//HiddenText", ".//Text", ".//ArticleText", ".//Body"))
      tibble(
        file = path,
        article_id = get_node_text(doc, c(".//GOID", ".//DocumentID", ".//RecordID", ".//ID")),
        title = get_node_text(doc, c(".//Title", ".//Headline")),
        date_raw = get_node_text(doc, c(".//NumericDate", ".//PublicationDate", ".//Date", ".//PubDate")),
        outlet = get_node_text(doc, c(".//PublisherName", ".//SourceName", ".//PublicationTitle")),
        text = strip_html_tags(raw_text)
      )
    },
    error = \(e) {
      message(glue("XML parse error in {path}: {e$message}"))
      tibble(file = path, article_id = NA_character_, title = NA_character_, date_raw = NA_character_, outlet = NA_character_, text = NA_character_)
    }
  )
}

xml_files <- cfg$xml_paths |>
  str_split(",", simplify = FALSE) |>
  pluck(1) |>
  str_trim() |>
  discard(\(x) x == "")

if (length(xml_files) == 0 && dir.exists(cfg$xml_dir)) {
  xml_files <- list.files(cfg$xml_dir, pattern = "\\.xml$", full.names = TRUE, ignore.case = TRUE)
}

if (length(xml_files) > 0) {
  message(glue("Reading {length(xml_files)} XML files."))
  articles_raw <- xml_files |>
    map(read_xml_article) |>
    bind_rows()
} else {
  if (!file.exists(cfg$articles_path)) {
    stop("No XML files found and CSV path does not exist. Set TDM_XML_PATHS or TDM_ARTICLES_PATH.")
  }
  csv_raw <- read_csv(cfg$articles_path, show_col_types = FALSE, progress = FALSE)
  names(csv_raw) <- clean_nm(names(csv_raw))
  id_col <- pick_col(csv_raw, c("article_id", "id", "document_id", "doc_id", "record_id", "item_id"), label = "article id column")
  outlet_col <- pick_col(csv_raw, c("outlet", "source", "publication", "publication_title", "newspaper", "publisher"), required = FALSE)
  date_col <- pick_col(csv_raw, c("pub_date", "date", "publication_date", "published_date", "publish_date", "document_date"), label = "publication date column")
  title_col <- pick_col(csv_raw, c("title", "headline", "article_title", "document_title"), required = FALSE)
  text_col <- pick_col(csv_raw, c("text", "full_text", "article_text", "body", "content", "document_text", "fulltext"), label = "article text column")
  message(glue("Detected CSV columns: id={id_col}, date={date_col}, text={text_col}"))
  articles_raw <- tibble(
    file = NA_character_,
    article_id = csv_raw[[id_col]] |> as.character(),
    title = if (!is.na(title_col)) csv_raw[[title_col]] |> as.character() else NA_character_,
    date_raw = csv_raw[[date_col]] |> as.character(),
    outlet = if (!is.na(outlet_col)) csv_raw[[outlet_col]] |> as.character() else NA_character_,
    text = csv_raw[[text_col]] |> as.character()
  )
}

articles <- articles_raw |>
  mutate(
    article_id = coalesce(article_id |> na_if(""), file |> basename() |> na_if(""), glue("article_{row_number()}")),
    outlet = coalesce(outlet |> na_if(""), "UNKNOWN_OUTLET"),
    article_date = parse_date(date_raw),
    text_norm = glue("{coalesce(title, '')} {coalesce(text, '')}") |> norm_text(),
    article_year = year(article_date),
    week = floor_date(article_date, unit = "week", week_start = 1)
  ) |>
  filter(!is.na(article_date), !is.na(text_norm))

mps_raw <- read_csv(cfg$mps_path, show_col_types = FALSE, progress = FALSE)
names(mps_raw) <- clean_nm(names(mps_raw))
required_mps_cols <- c(
  "id_election_bio",
  "candidate_last_norm",
  "candidate_first_norm",
  "candidate_first_last_norm",
  "candidate_norm",
  "candidate_role",
  "term_start",
  "term_end"
)
missing_mps_cols <- setdiff(required_mps_cols, names(mps_raw))
if (length(missing_mps_cols) > 0) {
  stop(glue("Missing MP columns in {cfg$mps_path}: {str_c(missing_mps_cols, collapse = ', ')}"))
}

party_lookup <- read_csv(
  cfg$party_lookup_path,
  col_select = any_of(c("id_election_bio", "term_congress", "term_party")),
  show_col_types = FALSE,
  progress = FALSE
) |>
  rename(term_congress_lk = term_congress, term_party_lk = term_party) |>
  distinct(id_election_bio, .keep_all = TRUE)

mps_n <- nrow(mps_raw)
id_bioguide_col <- if ("id_bioguide" %in% names(mps_raw)) mps_raw$id_bioguide else rep(NA_character_, mps_n)
term_congress_col <- if ("term_congress" %in% names(mps_raw)) mps_raw$term_congress else rep(NA_integer_, mps_n)
term_party_col <- if ("term_party" %in% names(mps_raw)) mps_raw$term_party else rep(NA_character_, mps_n)

mps <- mps_raw |>
  left_join(party_lookup, by = "id_election_bio") |>
  mutate(
    id_bioguide = id_bioguide_col |> as.character(),
    term_start_date = ymd(term_start |> as.character(), quiet = TRUE),
    term_end_date = ymd(term_end |> as.character(), quiet = TRUE),
    term_congress = coalesce(term_congress_col |> as.integer(), term_congress_lk |> as.integer()),
    term_party = coalesce(term_party_col |> as.character(), term_party_lk |> as.character(), "Unknown"),
    candidate_role_clean = candidate_role |> str_to_lower(),
    last_norm = candidate_last_norm |> norm_text(),
    first_last_norm = candidate_first_last_norm |> norm_text(),
    full_norm = candidate_norm |> norm_text(),
    start_year = year(term_start_date),
    end_year = year(term_end_date)
  ) |>
  filter(!is.na(term_congress), !is.na(term_start_date), !is.na(term_end_date), !is.na(last_norm))

max_end <- mps |> summarise(max_end = max(term_end_date, na.rm = TRUE)) |> pull(max_end)

congress_bounds <- mps |>
  group_by(term_congress) |>
  summarise(congress_start = min(term_start_date, na.rm = TRUE), .groups = "drop") |>
  arrange(term_congress) |>
  mutate(congress_end = coalesce(lead(congress_start) - days(1L), max_end))

articles <- articles |>
  left_join(congress_bounds, by = join_by(between(article_date, congress_start, congress_end))) |>
  select(-congress_start, -congress_end)

weather_sport_rx <- "\\b(?:WEATHER|FORECAST|TEMPERATURE|RAIN|SNOW|STORM|HUMIDITY|WIND|BASEBALL|FOOTBALL|BASKETBALL|SOCCER|HOCKEY|TENNIS|GOLF|TOURNAMENT|PLAYOFF|SCORE|TEAM|COACH|LEAGUE)\\b"
politics_hint_rx <- "\\b(?:CONGRESS|HOUSE|CONGRESSMAN|CONGRESSWOMAN|REPRESENTATIVE|DEMOCRAT|REPUBLICAN|FEDERALIST|WHIG|ELECTION|VOTE|BILL|LEGISLATION|WASHINGTON)\\b"
title_rx <- "CONGRESSMAN|CONGRESSWOMAN|REPRESENTATIVE|REP|HONORABLE|HON|MR|MRS|MS"

articles <- articles |>
  mutate(
    skip_fast = is.na(text_norm) |
      text_norm == "" |
      (str_detect(text_norm, weather_sport_rx) & !str_detect(text_norm, politics_hint_rx))
  )

mp_patterns <- mps |>
  transmute(
    term_congress,
    term_party,
    mp_key = coalesce(id_bioguide, id_election_bio, glue("mp_{row_number()}")),
    last_norm,
    first_last_norm,
    full_norm,
    start_year,
    end_year,
    candidate_role_clean
  ) |>
  distinct() |>
  mutate(
    name_pattern = pmap_chr(
      list(first_last_norm, full_norm, last_norm),
      \(first_last, full, last) c(
        if (!is.na(first_last)) glue("\\b{escape_rx(first_last)}\\b") else NA_character_,
        if (!is.na(full) && full != first_last) glue("\\b{escape_rx(full)}\\b") else NA_character_,
        glue("\\b(?:{title_rx})\\s+{escape_rx(last)}\\b")
      ) |>
        discard(\(z) is.na(z) || z == "") |>
        str_c(collapse = "|")
    )
  )

mp_by_congress <- mp_patterns |>
  group_split(term_congress) |>
  (\(x) set_names(x, map_chr(x, \(tbl) as.character(tbl$term_congress[[1]]))))()

count_party_mentions <- function(text_norm, term_congress, article_year, skip_fast) {
  if (isTRUE(skip_fast) || is.na(term_congress) || is.na(article_year) || is.na(text_norm) || text_norm == "") {
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
  candidate_mps <- mp_tbl |>
    filter(
      last_norm %in% tokens,
      !is.na(start_year),
      !is.na(end_year),
      article_year >= start_year,
      article_year <= end_year,
      is.na(candidate_role_clean) | candidate_role_clean != "original"
    )
  if (nrow(candidate_mps) == 0) {
    return(tibble(term_party = character(), mentions = integer()))
  }
  candidate_mps |>
    mutate(mentions = map_int(name_pattern, \(rx) str_count(text_norm, regex(rx)))) |>
    filter(mentions > 0) |>
    summarise(mentions = sum(mentions), .by = term_party)
}

party_hits <- pmap(
  list(articles$text_norm, articles$term_congress, articles$article_year, articles$skip_fast),
  count_party_mentions
) |>
  map2(articles$article_id, \(x, aid) x |> mutate(article_id = aid)) |>
  bind_rows()

article_base <- articles |>
  filter(!is.na(term_congress), !is.na(week), !is.na(article_date)) |>
  select(article_id, outlet, article_date, week, term_congress, skip_fast)

article_parties <- article_base |>
  inner_join(mp_patterns |> distinct(term_congress, term_party), by = "term_congress", relationship = "many-to-many") |>
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
