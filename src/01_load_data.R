#TODO: Function description
load_raw_data <- function(
    census_path = "data//raw//harmonised_ethnicity_counts.csv",
    shapes_path = "data//raw//OAXXCD_Boundaries_Generalised_Clipped_EW.geojson") {
  
  #create a list with the raw data
  raw_data <- lst(
    census_harm = read_csv(census_path),
    shapes_uk = st_read(shapes_path)
  )
  
  return(raw_data)
}

#TODO: Function description
clean_census <- function(census_raw) {
  
  #reshape and transform the data
  census_clean <- census_raw |>
    select(OAXXCD, WBRI.01:WGYP.21) |>
    pivot_longer(!OAXXCD, names_to = "ethnicity", values_to = "count") |>
    mutate(
      year = as.numeric(str_c("20", str_sub(ethnicity, -2, -1)))
    ) |>
    filter(!is.na(count))
  
  return(census_clean)
}