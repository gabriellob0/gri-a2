#load_packages("sfdep", "igraph")

#neighbours <- clustered_areas |>
#  select(OAXXCD, index, nb) |>
#  st_set_geometry(NULL) |>
#  unnest_wider(nb, names_sep = "") |>
#  pivot_longer(!c(OAXXCD, index)) |>
#  filter(!is.na(value), value != 0) |>
#  select(index, value, OAXXCD)


#g <- graph_from_data_frame(neighbours)

#plot(g)

#groups <- tibble(oa = unique(neighbours$OAXXCD), group = components(g)$membership) |>
#  mutate(count = n(), .by = group) |>
#  filter(group == 1)

#|> #or sf_use_s2(TRUE) https://github.com/r-spatial/sf/issues/1762
#  mutate(nb = st_contiguity(geometry),
#         index = row_number())
