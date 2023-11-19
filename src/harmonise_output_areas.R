#https://profrichharris.github.io/census/harmonised/
#packages ----
library(readr)
library(data.table)
library(dplyr)
library(purrr)
library(stringr)
library(RANN)
library(sf)
library(ggplot2)


#import data ----
import_lookup_data <- function(urls) {
  
  lookups_raw <- urls |>
    map(fread)
  
  lookup_merged <- lookups_raw$`2001_2011` |>
    select(1:4) |>
    full_join(lookups_raw$`2011_2021` |> select(1:5), by = "OA11CD") |>
    rename(CHG11 = CHGIND, CHG21 = CHNGIND) |>
    mutate(CHG21 = if_else(CHG21 == "x", "X", CHG21))
  
  return(lookup_merged)
}

#lookups for changes in Output Areas (OAs)
lookup_urls <- lst(
  `2001_2011` = "https://www.dropbox.com/s/7xb76wliw4qoy4q/Output_Area_%282001%29_to_Output_Area_%282011%29_to_Local_Authority_District_%282011%29_Lookup_in_England_and_Wales.csv?dl=1",
  `2011_2021` = "https://www.dropbox.com/s/wn6y5juef85nlgf/OA_%282011%29_to_OA_%282021%29_to_Local_Authority_District_%282022%29_for_England_and_Wales_Lookup.csv?dl=1"
)

lookup_merged <- import_lookup_data(lookup_urls)

#stage 1 ----
stage1 <- lookup_merged |>
  mutate(
    OAXXCD = if_else(CHG11 == "U" & CHG21 == "U", OA21CD, NA),
    OAXXCD = if_else(CHG11 == "M" & CHG21 == "U", OA21CD, OAXXCD),
    OAXXCD = if_else(CHG11 == "S" & CHG21 == "U", OA01CD, OAXXCD),
    OAXXCD = if_else(CHG11 == "U" & CHG21 == "M", OA21CD, OAXXCD),
    OAXXCD = if_else(CHG11 == "U" & CHG21 == "S", OA11CD, OAXXCD),
    OAXXCD = if_else(CHG11 == "S" & CHG21 == "S", OA01CD, OAXXCD),
    OAXXCD = if_else(CHG11 == "M" & CHG21 == "M", OA21CD, OAXXCD),
    OAXXCD = if_else(CHG11 == "M" & CHG21 == "S", OA11CD, OAXXCD)
  )

stage1 |>
  filter(is.na(OAXXCD)) |>
  select(CHG11, CHG21) |>
  group_by(CHG11, CHG21) |>
  summarise(n = n()) |>
  print(n = Inf)


#stage 2 ----
match_by_centroid <- function(lookup_df) {
  
  centroids_urls <- lst(
    `2001` = "https://www.dropbox.com/s/1fcm9ggpxudv72l/Output_Areas_%28December_2001%29_Population_Weighted_Centroids.csv?dl=1",
    `2011` = "https://www.dropbox.com/s/bsyg152nkcthvzc/Output_Areas_%28Dec_2011%29_PWC.csv?dl=1"
  )
  
  centroids_raw <- centroids_urls |>
    map(read_csv)
  
  pts01 <- centroids_raw$`2001` |>
    select(X, Y, oa01cd) |>
    rename(OA01CD = oa01cd)
  
  pts11 <- centroids_raw$`2011` |>
    select(X, Y, OA11CD) |>
    semi_join(lookup_df |> filter(is.na(OA01CD)), by = "OA11CD")
  
  nearest_neighbours <- nn2(
    data = select(pts01, -OA01CD),
    query = select(pts11, -OA11CD),
    k = 1
  )
  
  nn_index <- nearest_neighbours$nn.idx[, 1]
  
  matched_df <- lookup_df |>
    left_join(tibble(OA11CD = pts11$OA11CD, nearest = pts01$OA01CD[nn_index]),
              by = join_by(OA11CD)) |>
    mutate(OA01CD = if_else(!is.na(nearest), nearest, OA01CD)) |>
    select(-nearest)
  
  return(matched_df)
}

stage2 <- match_by_centroid(stage1)

#stage 3 ----
find_recursive_connections <- function(lookup_df) {
  
  df3 <- lookup_df
  
  while(anyNA(df3$OAXXCD)) {
    
    found <- df3 |>
      filter(is.na(OAXXCD)) |>
      slice(1)
    
    n <- nrow(found)
    n.previous <- 0
    
    while(n > n.previous) {
      
      n.previous <- n
      search <- found
      
      found <- df3 |>
        filter(OA01CD %in% search$OA01CD |
                 OA11CD %in% search$OA11CD |
                 OA21CD %in% search$OA21CD)
      
      n <- nrow(found)
      
    }
    
    df3 <- df3 |>
      mutate(OAXXCD = if_else(OA01CD %in% found$OA01CD |
                                OA11CD %in% found$OA11CD |
                                OA21CD %in% found$OA21CD,
                              found$OA21CD[1],
                              OAXXCD))
    
  }
  
  return(df3)
}

clean_lookups <- find_recursive_connections(stage2)



frc_dt_version <- function(lookup_df) {
  
  # Convert df3 to a data.table
  df3 <- setDT(lookup_df)
  
  while(anyNA(df3$OAXXCD)) {
    missing_rows <- df3[is.na(OAXXCD)]
    found <- missing_rows[1]
    n <- 1
    n.previous <- 0
    
    # Pre-calculate the filter condition once outside the loop
    filter_condition <- c("OA01CD", "OA11CD", "OA21CD")
    
    while(n > n.previous) {
      n.previous <- n
      search <- found
      
      # Use the filter condition obtained above
      found <- df3[Reduce(`|`, lapply(filter_condition, function(col) get(col) %in% search[[col]]))]
      
      n <- nrow(found)
    }
    
    df3[, OAXXCD := fifelse(OA01CD %in% found$OA01CD |
                              OA11CD %in% found$OA11CD |
                              OA21CD %in% found$OA21CD,
                            found$OA21CD[1],
                            OAXXCD)]
  }
  
  return(df3)
}

clean_fast <- frc_dt_version(stage2)
all.equal(clean_fast, clean_lookups)


#recoding ----
recode_oas <- function(lookup_df) {
  
  gps <- clean_lookups |>
    arrange(OA01CD) |>
    select(OAXXCD) |>
    filter(!duplicated(OAXXCD)) |>
    mutate(i = str_pad(row_number(), width = 7, pad = "0"))
  
  recoded <- clean_lookups|>
    left_join(gps, by = "OAXXCD") |>
    mutate(OAXXCD = str_c("X", i, str_sub(OAXXCD, 1, 1)),
           OA01CD = if_else(is.na(CHG11), NA, OA01CD)) |>
    select(-i)
  
  return(recoded)
}

recoded <- recode_oas(clean_lookups)


#higher-level geographies ----
import_geographies <- function(recoded_df) {
  
  geography_urls <- lst(
    MSOA_ref = "https://www.dropbox.com/s/vcaw0emrgz5efta/OA21_LSOA21_MSOA21_LAD22_EW_LU.csv?dl=1",
    MSOA_names = "https://www.dropbox.com/s/o3albmackgz0zao/MSOA-Names-2.2.csv?dl=1",
    regions = "https://www.dropbox.com/s/8um5h5quf9wc2vj/OA21_RGN22_LU.csv?dl=1",
    census_data = "https://www.dropbox.com/s/lw8r575noohhuu5/TS021-2021-1-filtered-2022-11-30T10_03_36Z.csv?dl=1"
  )
  
  geography_raw <- geography_urls |>
    map(read_csv)
  
  
  geography <- recoded_df |>
    select(OAXXCD, OA21CD, LAD22CD, LAD22NM) |>
    left_join(
      geography_raw$MSOA_ref |>
        select(1, 4) |>
        rename(OA21CD = 1, MSOA21CD = 2),
      by = join_by(OA21CD)
    ) |>
    left_join(
      geography_raw$MSOA_names |>
        select(1, 4) |>
        rename(MSOA21CD = 1, MSOA21NM = 2),
      by = join_by(MSOA21CD)
    ) |>
    left_join(
      geography_raw$regions |>
        select(1:3) |>
        rename(OA21CD = 1, RGN21CD = 2, RGN21NM = 3),
      by = join_by(OA21CD)
    ) |>
    left_join(
      geography_raw$census_data |>
        select(1, 5) |>
        rename(OA21CD = 1) |>
        summarise(popn = sum(Observation), .by = "OA21CD"),
      by = join_by(OA21CD)
    )
  
  return(geography)
}

raw_geographies <- import_geographies(recoded)

#harmonised OA fit ----
fit_harmonised_oas <- function(geog_df) {
  
  geog_uniq <- geog_df |>
    select(
      OAXXCD, MSOA21CD, MSOA21NM, LAD22CD, LAD22NM, RGN21CD, RGN21NM, popn
    ) |>
    unique()
  
  geography <- geog_uniq |>
    group_by(OAXXCD) |>
    summarise(popn = sum(popn), .groups = "keep") |>
    left_join(geog_uniq |>
                group_by(OAXXCD, MSOA21CD, MSOA21NM) |>
                summarise(popn = sum(popn), .groups = "keep") |>
                group_by(OAXXCD) |>
                arrange(OAXXCD, desc(popn)) |>
                select(-popn) |>
                slice(1) |>
                ungroup(), by = "OAXXCD") |>
    left_join(geog_uniq |>
                group_by(OAXXCD, LAD22CD, LAD22NM) |>
                summarise(popn = sum(popn), .groups = "keep") |>
                group_by(OAXXCD) |>
                arrange(OAXXCD, desc(popn)) |>
                select(-popn) |>
                slice(1) |>
                ungroup(), by = "OAXXCD") |>
    left_join(geog_uniq |>
                group_by(OAXXCD, RGN21CD, RGN21NM) |>
                summarise(popn = sum(popn), .groups = "keep") |>
                group_by(OAXXCD) |>
                arrange(OAXXCD, desc(popn)) |>
                select(-popn) |>
                slice(1) |>
                ungroup(), by = "OAXXCD") |>
    select(
      OAXXCD, popn, MSOA21CD, MSOA21NM, LAD22CD, LAD22NM, RGN21CD, RGN21NM
    )
  
  return(geography)
}

fitted_oas <- fit_harmonised_oas(raw_geographies)

#boundary files ----
map <- st_read("https://www.dropbox.com/s/2ffk5mwezyyjzre/Output_Areas_%28December_2021%29_Boundaries_Generalised_Clipped_EW_%28BGC%29.geojson?dl=1") |>
  left_join(
    recoded |>
      select(OAXXCD, OA21CD) |>
      unique(),
    by = join_by(OA21CD)
  )


map_agg <- map |>
  select(OAXXCD) |>
  aggregate(by = list(map$OAXXCD), FUN = length) |>
  rename(OAXXCD = 1, nOA21 = 2) |>
  left_join(fitted_oas, by = join_by(OAXXCD))


#maps ----
map_agg |>
  filter(RGN21NM == "London") |>
  ggplot() +
  geom_sf()
