library(tidyverse)

event_data <- read_csv("data/fmt/event_base.csv")

event_data |> 
  filter(candidate_role == "original")

event_data |> 
  filter(candidate_role == "winner")