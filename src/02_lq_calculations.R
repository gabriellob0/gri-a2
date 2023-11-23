#TODO: Function description
calculate_lq <- function(census_data) {
  
  #total population
  pop <- sum(census_data$count)
  
  #calculate the LQs
  lq_data <- census_data |>
    mutate(share_ethoa = count / sum(count), .by = ethnicity) |>
    mutate(share_oa = sum(count) / pop, .by = OAXXCD) |>
    mutate(loc_quo = share_ethoa / share_oa)
  
  #rank the data by LQ
  ranked_lqs <- lq_data |>
    group_by(ethnicity) |>
    arrange(ethnicity, loc_quo) |>
    mutate(rank = row_number()) |>
    ungroup()
  
  return(ranked_lqs)
}

#TODO: Function description
find_thresholds <- function(lq_data) {
  
  #find 99th percentile for white british
  white_british <- lq_data |>
    mutate(loc_count = n(), .by = ethnicity) |>
    filter(rank > 0.99*loc_count & str_detect(ethnicity, "WBRI")) |>
    slice_min(rank, by = ethnicity) |>
    select(year, loc_quo) |>
    rename(threshold = loc_quo)
  
  #join the threshold to the data
  with_thresholds <- lq_data |>
    left_join(white_british, by = join_by(year))
  
  return(with_thresholds)
}