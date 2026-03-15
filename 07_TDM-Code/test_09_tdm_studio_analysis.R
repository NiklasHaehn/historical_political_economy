library(tidyverse)
library(glue)

project_dir <- getwd()
tmp_dir <- file.path(tempdir(), glue("tdm_xml_test_{format(Sys.time(), '%Y%m%d_%H%M%S')}"))
dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

xml_paths <- file.path(tmp_dir, c("a1.xml", "a2.xml", "a3.xml", "a4.xml"))
member_event_path <- file.path(tmp_dir, "member_event_reduced_test.csv")
party_lookup_path <- file.path(tmp_dir, "party_lookup_test.csv")
article_out <- file.path(tmp_dir, "article_party_slant.csv")
weekly_out <- file.path(tmp_dir, "weekly_slant.csv")
weekly_wide_out <- file.path(tmp_dir, "weekly_slant_wide.csv")

xml_docs <- c(
  "<Document><GOID>A1</GOID><Title>House report</Title><NumericDate>20220610</NumericDate><PublisherName>Daily Post</PublisherName><FullText>Representative John Smith met Congresswoman Jane Doe in Washington.</FullText></Document>",
  "<Document><GOID>A2</GOID><Title>Sports weather</Title><NumericDate>20220611</NumericDate><PublisherName>Daily Post</PublisherName><FullText>Baseball score and weather forecast with rain and wind.</FullText></Document>",
  "<Document><GOID>A3</GOID><Title>Congress update</Title><NumericDate>20220612</NumericDate><PublisherName>Tribune</PublisherName><FullText>Mr Bill Johnson and Rep Doe discussed a new bill in the House.</FullText></Document>",
  "<Document><GOID>A4</GOID><Title>Local note</Title><NumericDate>20220612</NumericDate><PublisherName>Tribune</PublisherName><FullText>A local event with no politics mentioned.</FullText></Document>"
)

walk2(xml_docs, xml_paths, write_lines)

mps_test <- tibble(
  id_election_bio = c("E1", "E2", "E3", "E4"),
  id_bioguide = c("S000001", "D000001", "D000001", "J000001"),
  candidate_first_norm = c("JOHN", "JANE", "JANE", "BILL"),
  candidate_last_norm = c("SMITH", "DOE", "DOE", "JOHNSON"),
  candidate_first_last_norm = c("JOHN SMITH", "JANE DOE", "JANE DOE", "BILL JOHNSON"),
  candidate_norm = c("JOHN SMITH", "JANE DOE", "JANE DOE", "BILL JOHNSON"),
  candidate_role = c(NA_character_, "winner", "original", NA_character_),
  term_start = c("2021-01-03", "2021-01-03", "2021-01-03", "2021-01-03"),
  term_end = c("2023-01-03", "2023-01-03", "2023-01-03", "2023-01-03")
)

party_lookup_test <- tibble(
  id_election_bio = c("E1", "E2", "E3", "E4"),
  term_congress = 117L,
  term_party = c("Republican", "Democrat", "Democrat", "Independent")
)

write_csv(mps_test, member_event_path)
write_csv(party_lookup_test, party_lookup_path)

env_keys <- c(
  "TDM_XML_PATHS",
  "TDM_MPS_PATH",
  "TDM_PARTY_LOOKUP_PATH",
  "TDM_ARTICLE_OUT",
  "TDM_WEEKLY_OUT",
  "TDM_WEEKLY_WIDE_OUT"
)
env_old <- Sys.getenv(env_keys, unset = NA_character_)

on.exit(
  walk2(
    env_keys,
    env_old,
    \(k, v) {
      if (is.na(v)) {
        Sys.unsetenv(k)
      } else {
        Sys.setenv(structure(v, names = k))
      }
    }
  ),
  add = TRUE
)

Sys.setenv(
  TDM_XML_PATHS = str_c(xml_paths, collapse = ","),
  TDM_MPS_PATH = member_event_path,
  TDM_PARTY_LOOKUP_PATH = party_lookup_path,
  TDM_ARTICLE_OUT = article_out,
  TDM_WEEKLY_OUT = weekly_out,
  TDM_WEEKLY_WIDE_OUT = weekly_wide_out
)

source(file.path(project_dir, "09_TDM-Studio-Analysis.R"), local = new.env())

article_res <- read_csv(article_out, show_col_types = FALSE)
weekly_res <- read_csv(weekly_out, show_col_types = FALSE)
weekly_wide_res <- read_csv(weekly_wide_out, show_col_types = FALSE)

expected_parties <- c("Republican", "Democrat", "Independent")

stopifnot(setequal(unique(article_res$term_party), expected_parties))
stopifnot(n_distinct(article_res$article_id) == 4L)
stopifnot(all(article_res |> count(article_id) |> pull(n) == length(expected_parties)))

weather_mentions <- article_res |>
  filter(article_id == "A2") |>
  distinct(total_mentions) |>
  pull(total_mentions)
stopifnot(length(weather_mentions) == 1L, weather_mentions == 0)

a1_dem_mentions <- article_res |>
  filter(article_id == "A1", term_party == "Democrat") |>
  pull(mentions)
stopifnot(length(a1_dem_mentions) == 1L, a1_dem_mentions == 1)

stopifnot(nrow(weekly_res) > 0, nrow(weekly_wide_res) > 0)
stopifnot(all(c("outlet", "week", "term_congress") %in% names(weekly_wide_res)))

message("Test passed: XML ingestion + mapping + original-role exclusion work.")
message(glue("Temporary test files: {tmp_dir}"))
print(article_res |> arrange(article_id, desc(mentions)) |> select(article_id, outlet, term_party, mentions, party_share))
