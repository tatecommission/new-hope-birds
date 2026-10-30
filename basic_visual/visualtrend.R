library(tidyverse)
durham_spring <- read_csv("../data/spring/durham_spring_final.csv")
jordan_lake_spring <- read_csv("../data/spring/jordan_lake_spring_final.csv")
chapel_hill_spring <- read_csv("../data/spring/chapel_hill_spring_final.csv")

durham_winter <- read_csv("../data/winter/Durham-1968-2023-winter.xlsx - DURCBC.csv")
chapel_hill_winter <- read_csv("../data/winter/chapel_hill_winter_final.csv")
jordan_lake_winter <- read_csv("../data/winter/jordan_lake_winter_final.csv")

names(durham_spring)
names(durham_winter)

one_species <- "Canada Goose"

tidy_wide <- function(df, region, season, species_col = "COMMON NAME") {
  df |>
    rename(species = all_of(species_col)) |>
    pivot_longer(
      cols            = matches("\\d{4}"),   # every column whose name has a 4-digit year
      names_to        = "year",
      names_transform = parse_number,        # "1968" or "X1968" -> 1968
      values_to       = "count",
      values_transform = as.numeric
    ) |>
    select(species, year, count) |>
    mutate(region = region, season = season)
}
tidy_durham_winter <- function(df, region, season, species_col = "COMMON NAME") {
  df |>
    rename(species = all_of(species_col)) |>
    pivot_longer(
      cols            = matches("\\.\\.\\.\\d+$"),   # every column whose name has a 4-digit year
      names_to        = "year",
      names_transform = parse_number,        # "1968" or "X1968" -> 1968
      values_to       = "count",
      values_transform = as.numeric
    ) |>
    select(species, year, count) |>
    mutate(region = region, season = season)
}

all_data <- bind_rows(
  tidy_wide(durham_spring,      "Durham",      "Spring", species_col = "species"),
  tidy_wide(jordan_lake_spring, "Jordan Lake", "Spring"),
  tidy_wide(chapel_hill_spring, "Chapel Hill", "Spring"),
  tidy_wide(jordan_lake_winter, "Jordan Lake", "Winter"),
  tidy_wide(chapel_hill_winter, "Chapel Hill", "Winter")
)
