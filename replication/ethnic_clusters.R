library(readr)
library(dplyr)

df <- read_csv("replication//simulated_data.csv")

# share of group in location
new_df <- df |>
  mutate(grp_loc_count = n(), .by = c("location", "group")) |>
  mutate(grp_count = n(), .by = "group") |>
  mutate(share_grp_loc = grp_loc_count / grp_count) |>
  mutate(count_loc = n(), .by = "location") |>
  mutate(share_loc = count_loc / n()) |>
  mutate(LQ = share_grp_loc / share_loc)

