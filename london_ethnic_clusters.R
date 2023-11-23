#source functions ----
source("src//00_load_packages.R")
source("src//01_load_data.R")
source("src//02_lq_calculations.R")
source("src//04_cluster_oas_spat.R")


#import packages and data ----
load_packages(
  c("readr", "tidyr", "dplyr", "stringr", "sf", "ggplot2")
)

raw_data <- load_raw_data()


#data pre-processing ----
#TODO: they filter out tracts with low population and some other conditions
cleaned_census <- raw_data$census_harm |>
  filter(RGN21NM == "London") |>
  clean_census()

oas_with_lqs <- cleaned_census |>
  filter(year == 2001) |>
  calculate_lq() |>
  find_thresholds() |>
  filter(ethnicity == "CHNE.01") #to be removed hopefully


#identify ethnic OAs ----
#this can run for all ethnic groups fine in theory, just takes time
spatial <- raw_data$shapes_uk |>
  join_shapes(oas_with_lqs)

#this probably needs to be split into a list and the we use map
clustered <- cluster_neighbourhoods(spatial)

#this can go into a function and we also use map, but need to preserve ethnicity for joins later
duplicates_ref <- clustered |>
  group_by(OAXXCD) |>
  filter(n() > 1) |>
  mutate(id_fix = min(cluster_id)) |>
  ungroup()

clusters_fixed <- clustered |>
  left_join(duplicates_ref, by = c("OAXXCD", "cluster_id")) |>
  mutate(cluster_id = if_else(!is.na(id_fix), id_fix, TRUE)) |>
  distinct() |>
  select(-id_fix)


#prepare maps ----
plot_data <- spatial |>
  left_join(clusters_fixed, by = join_by(OAXXCD))

ggplot() +
  geom_sf(data = plot_data, aes(colour = ethnic_oa))
