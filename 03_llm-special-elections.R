library(tidyverse)
library(glue)
library(ollamar)

script_start <- Sys.time()

set.seed(1234)

model_name <- "llama3.2:latest"

prompt_special_election <- function(txt) {
  glue(
    "You are classifying biographies of members of the U.S. House of Representatives.

TASK
Decide whether this biography contains evidence that the person FIRST ENTERED the U.S. House through a special election.

IMPORTANT SCOPE RULE
Only consider statements about entry into the U.S. House of Representatives.
Ignore all mentions of vacancies, unexpired terms, appointments, or elections for ANY OTHER OFFICE, including:
- governor
- secretary of state
- attorney general
- judge
- military office
- state legislature
- cabinet or administrative office
- senator

DECISION RULE
Return Yes only if the biography clearly indicates that the person first entered the U.S. House:
- by special election
- in a special election
- to fill a vacancy in the U.S. House
- to fill an unexpired term in the U.S. House
- to replace a deceased or resigned U.S. Representative mid-term

Return No if:
- there is no clear evidence of House special-election entry
- vacancy language refers to another office
- the biography describes normal election to Congress
- the case is ambiguous

Be conservative.
If unclear, return No.

EXAMPLES

Example 1
Biography: 'appointed secretary of state in 1875 to fill a vacancy; elected as a Republican to the Thirty-eighth Congress'
Answer: No

Example 2
Biography: 'appointed attorney general of Missouri to fill out the unexpired term; elected as a Democrat to the Forty-third Congress'
Answer: No

Example 3
Biography: 'upon the readmission of the State to representation was elected as a Unionist to the Thirty-ninth Congress and served from July 24, 1866, to March 3, 1867'
Answer: Yes

Example 4
Biography: 'elected as a Democrat to the Ninety-second Congress, by special election, to fill the vacancy caused by the death of Representative X'
Answer: Yes

OUTPUT
Return exactly one label and nothing else:
Yes
No

BIOGRAPHY 
\"\"\" 
{txt}
\"\"\" 
" 
  ) 
}



normalize_label <- function(x) {
  x |>
    coalesce("") |>
    str_to_lower() |>
    str_extract("yes|no") |>
    str_to_title() |>
    replace_na("No")
}

classify_special_election <- function(txt) {
  if (txt |> coalesce("") |> str_squish() == "") return("No")
  resp <- generate(model_name, prompt_special_election(txt), temperature = 0)
  resp |> resp_process("text") |> normalize_label()
}

safe_classify_special_election <- purrr::possibly(classify_special_election, otherwise = "No")

mp_data <- read_csv("data/fmt/MP_data.csv", show_col_types = FALSE)

mp_profiles <- mp_data |>
  distinct(id_bioguide, .keep_all = TRUE) |>
  transmute(id_bioguide, bio_profile_text)

special_election_labels <- mp_profiles |>
  mutate(Special_Election_llama3 = map_chr(bio_profile_text, safe_classify_special_election)) |>
  select(id_bioguide, Special_Election_llama3)

mp_data_llama3 <- mp_data |>
  left_join(special_election_labels, by = "id_bioguide")

write_csv(mp_data_llama3, "data/fmt/MP_data_llama3.csv", na = "")

special_election_labels
mp_profiles$bio_profile_text

script_end <- Sys.time()
script_secs <- as.numeric(difftime(script_end, script_start, units = "secs"))
message(glue("Total execution time: {round(script_secs, 2)} sec ({round(script_secs / 60, 2)} min)"))

