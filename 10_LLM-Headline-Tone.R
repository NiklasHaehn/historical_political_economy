library(tidyverse)
library(glue)
library(ollamar)

script_start <- Sys.time()

set.seed(1234)

model_name <- Sys.getenv("HEADLINE_TONE_MODEL", "llama3.2:latest")
input_path <- Sys.getenv("HEADLINE_TONE_INPUT", "data/fmt/TDM_Studio_event_articles_individual_6w.csv")
labels_out <- Sys.getenv("HEADLINE_TONE_LABELS_OUT", "data/fmt/10_tdm_headline_tone_labels_llama3.csv")
articles_out <- Sys.getenv("HEADLINE_TONE_ARTICLES_OUT", "data/fmt/10_tdm_event_articles_individual_6w_tone_llama3.csv")
n_headlines_max <- Sys.getenv("HEADLINE_TONE_N_MAX", "") |>
  na_if("") |>
  as.integer() |>
  (\(x) ifelse(is.na(x), Inf, x))()

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

articles <- read_csv(input_path, show_col_types = FALSE)

if (!"title" %in% names(articles)) stop(glue("Column `title` not found in {input_path}."))

headline_labels <- articles |>
  distinct(title) |>
  filter(!(is.na(title) | str_squish(title) == "")) |>
  slice_head(n = n_headlines_max) |>
  mutate(headline_tone_label = map_chr(title, safe_classify_headline)) |>
  arrange(title)

if (is.finite(n_headlines_max)) {
  message(glue("Processed only first {nrow(headline_labels)} unique headlines due to HEADLINE_TONE_N_MAX={n_headlines_max}."))
}

articles_tone <- articles |>
  left_join(headline_labels, by = "title") |>
  mutate(
    headline_tone_label = coalesce(headline_tone_label, "NA"),
    headline_tone = case_when(
      headline_tone_label == "1" ~ 1L,
      headline_tone_label == "0" ~ 0L,
      TRUE ~ NA_integer_
    )
  )

dir.create(dirname(labels_out), recursive = TRUE, showWarnings = FALSE)
dir.create(dirname(articles_out), recursive = TRUE, showWarnings = FALSE)

write_csv(headline_labels, labels_out, na = "")
write_csv(articles_tone, articles_out, na = "")

script_end <- Sys.time()
script_secs <- as.numeric(difftime(script_end, script_start, units = "secs"))

message(glue("Saved headline labels: {labels_out}"))
message(glue("Saved article-level tone data: {articles_out}"))
message(glue("Unique headlines labeled: {nrow(headline_labels)}"))
message(glue("Rows in article-level output: {nrow(articles_tone)}"))
message(glue("Total execution time: {round(script_secs, 2)} sec ({round(script_secs / 60, 2)} min)"))
