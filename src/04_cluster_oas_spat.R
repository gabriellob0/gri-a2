join_shapes <- function(shapes, output_areas) {
  
  spatial <- raw_data$shapes_uk |>
    right_join(oas_with_lqs, by = join_by(OAXXCD)) |>
    mutate(ethnic_oa = if_else(loc_quo > threshold, 1, 0)) |>
    st_make_valid() #or sf_use_s2(TRUE) https://github.com/r-spatial/sf/issues/1762
  
  return(spatial)
}

cluster_neighbourhoods <- function(sp_df) {
  
  ethnic_oas <- sp_df |>
    filter(ethnic_oa == 1)
  
  clusters <- ethnic_oas |>
    st_union() |>
    st_cast("POLYGON") |>
    st_as_sf() |>
    mutate(cluster_id = row_number())
    
  merged_ids <- ethnic_oas  |>
    st_join(clusters) |>
    st_set_geometry(NULL) |>
    select(OAXXCD, cluster_id)
  
  return(merged_ids)
}
