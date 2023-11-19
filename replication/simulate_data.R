#libs ----
library(evd)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(stringr)

#fns ----
gen_houses <- function(H = 1000, G = 4, J = 10) {
  
  houses <- str_c("h", seq(1:H))
  locations <- str_c("loc", seq(1:J))
  groups <- str_c("g", seq(1:G))
  
  df_utils <- expand_grid(location = locations, group = groups) |>
    mutate(utils = rnorm(40, 10, 5)^2)
  
  df_houses = tibble(house = houses,
                     location = sample(locations, H, replace = T)) |>
    left_join(df_utils, by = "location", relationship = "many-to-many")
  
  return(df_houses)
}


#scripts ----
houses <- gen_houses() |>
  mutate(Hk = n(), .by = location) |>
  mutate(denom = sum(Hk*utils), .by = c("location", "group")) |>
  mutate(prob = utils / denom)


population <- tibble(person = str_c("ind", seq(1:1000)),
                     group = sample(str_c("g", seq(1:4)), 1000, replace = T))

test <- expand_grid(population$person, unique(houses$house)) |>
  mutate(match_error = rgumbel(1000000)) |>
  left_join(population, by = join_by("population$person" == "person")) |>
  left_join(houses, by = join_by("unique(houses$house)" == "house",
                                 "group" == "group")) |>
  mutate(ind_util = log(utils) + match_error) |>
  rename(population = `population$person`,
         house = `unique(houses$house)`)

find_house <- function(df, i) {
  
  house <- test |>
    filter(population == {{i}}) |>
    filter(ind_util == max(ind_util)) |>
    pull(house)
  
  return(house)
}

optimal_h <- character(1000)

for (i in 1:1000) {
  ind <- paste("ind", i, sep = "")
  
  if (ind == "ind1") {
    optimal_h[i] <- find_house(test, "ind1")
  } else {
    new_test <- filter(test, !(house %in% optimal_h[1:(i-1)]))
    optimal_h[i] <- find_house(new_test, ind)
  }
}

final <- population |>
  bind_cols(optimal_h) |>
  rename(house = 3) |>
  left_join(unique(select(houses, house, location)))

write_csv(final, "simulated_data.csv")
