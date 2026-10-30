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


tidy_wide <- function(df, region, season, species_col = NULL) {
  
  idx <- if (is.numeric(species_col)) {
    species_col
  } else if (is.character(species_col)) {
    which(str_to_lower(names(df)) == str_to_lower(species_col))[1]
  } else {
    which(str_detect(str_to_lower(names(df)), "species|common"))[1]
  }
  
  if (is.na(idx)) {
    stop("Can't find the species column for ", region, " ", season,
         ". First columns are: ", paste(head(names(df), 8), collapse = ", "))
  }
  
  df |>
    rename(species = all_of(names(df)[idx])) |>
    pivot_longer(
      cols             = matches("\\d{4}"),   # columns whose name has a 4-digit year
      names_to         = "year",
      names_transform  = parse_number,
      values_to        = "count",
      values_transform = as.numeric
    ) |>
    select(species, year, count) |>
    mutate(region = region, season = season)
}

tidy_durham_winter <- function(df) {
  is_year <- function(x) replace_na(str_detect(as.character(x), "^(19|20)\\d{2}$"), FALSE)
  
  hdr   <- which.max(apply(df, 1, function(r) sum(is_year(r))))  # row holding the years
  years <- as.character(unlist(df[hdr, ]))
  keep  <- is_year(years)                                        # which columns are years
  
  body <- df[-seq_len(hdr), ]                                    # drop title/header rows
  
  body[, keep] |>
    set_names(years[keep]) |>
    mutate(species = body[[3]], .before = 1) |>                  # 3rd column = Common Name
    pivot_longer(-species, names_to = "year", values_to = "count",
                 names_transform  = as.numeric,
                 values_transform = as.numeric) |>
    mutate(region = "Durham", season = "Winter")
}
all_data <- bind_rows(
  tidy_wide(durham_spring,      "Durham",      "Spring", species_col = "species"),
  tidy_wide(jordan_lake_spring, "Jordan Lake", "Spring"),
  tidy_wide(chapel_hill_spring, "Chapel Hill", "Spring"),
  tidy_durham_winter(durham_winter),
  tidy_wide(jordan_lake_winter, "Jordan Lake", "Winter"),
  tidy_wide(chapel_hill_winter, "Chapel Hill", "Winter")
)

species_data <- all_data |>
  filter(str_to_lower(str_squish(species)) == str_to_lower(one_species)) |>
  drop_na(count) |>                       # blank cells = not measured that year
  group_by(region, season, year) |>
  summarise(count = sum(count), .groups = "drop")

if (nrow(species_data) == 0) stop("Species not found: ", one_species)

plot_separate <- ggplot(species_data, aes(x = year, y = count)) +
  geom_point(color = "steelblue", size = 2) +
  geom_line(color = "steelblue", alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") +
  facet_grid(season ~ region, scales = "free_x") +
  labs(title    = paste("Count trends:", one_species),
       subtitle = "Yearly counts with linear trend line; each panel shows only the years measured",
       x = "Year", y = "Count") +
  theme_minimal(base_size = 12) +
  theme(strip.text = element_text(face = "bold"))

plot_overlay <- ggplot(species_data,
                       aes(x = year, y = count,
                           color = region, linetype = season, shape = season,
                           group = interaction(region, season))) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1) +
  labs(title    = paste("Count trends:", one_species),
       subtitle = "6 trend lines: 3 regions x 2 seasons",
       x = "Year", y = "Count",
       color = "Region", linetype = "Season", shape = "Season") +
  theme_minimal(base_size = 12)


print(plot_separate)
print(plot_overlay)