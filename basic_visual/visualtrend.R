library(tidyverse)
library(jsonlite)   # for the dropdown page at the bottom (comes with tidyverse)

# If R can't find the files, set the working directory to the folder this script is in:
#setwd("/Users/sub/newhope/basic_visual")      # <- uncomment and edit if needed
# (or in RStudio: Session > Set Working Directory > To Source File Location)

durham_spring <- read_csv("../data/spring/durham_spring_final.csv")
jordan_lake_spring <- read_csv("../data/spring/jordan_lake_spring_final.csv")
chapel_hill_spring <- read_csv("../data/spring/chapel_hill_spring_final.csv")

durham_winter <- read_csv("../data/winter/Durham-1968-2023-winter.xlsx - DURCBC.csv")
chapel_hill_winter <- read_csv("../data/winter/chapel_hill_winter_final.csv")
jordan_lake_winter <- read_csv("../data/winter/jordan_lake_winter_final.csv")

# names(durham_spring)    # uncomment to check column names
# names(durham_winter)

# curviness of the trend lines: smaller = wigglier (0.3-0.5), larger = smoother (0.8-1)
span_value <- 0.6


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
  
  # the header row is near the top, so only search the first 15 rows (faster)
  hdr   <- which.max(apply(head(df, 15), 1, function(r) sum(is_year(r))))
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
  tidy_wide(durham_spring,      "Durham",      "Spring"),
  tidy_wide(jordan_lake_spring, "Jordan Lake", "Spring"),
  tidy_wide(chapel_hill_spring, "Chapel Hill", "Spring"),
  tidy_durham_winter(durham_winter),
  tidy_wide(jordan_lake_winter, "Jordan Lake", "Winter"),
  tidy_wide(chapel_hill_winter, "Chapel Hill", "Winter")
)

# =============================================================
# PLOT FUNCTION: any species, 2 x 3 separate panels
# (season = rows, region = columns), curvy trend line with a 95% band
# and a vertical 95% CI bar on the right side of each panel
# =============================================================
plot_species <- function(species_name, span = span_value) {
  
  species_data <- all_data |>
    filter(str_to_lower(str_squish(species)) == str_to_lower(species_name)) |>
    drop_na(count) |>                       # blank cells = not measured that year
    group_by(region, season, year) |>
    summarise(count = sum(count), .groups = "drop") |>
    group_by(region, season) |>
    mutate(n_years = n()) |>
    ungroup()
  
  if (nrow(species_data) == 0) stop("Species not found: ", species_name)
  
  # curves need enough years to fit; sites with fewer years just show points
  smooth_data <- filter(species_data, n_years >= 8)
  
  # 95% CI of the smoothed value in each site's LAST year (the side bar)
  end_ci <- smooth_data |>
    group_by(region, season) |>
    group_modify(~ {
      fit  <- loess(count ~ year, data = .x, span = span)
      yr   <- max(.x$year)
      p    <- predict(fit, data.frame(year = yr), se = TRUE)
      half <- qt(0.975, p$df) * p$se.fit
      tibble(year  = yr,
             fit   = unname(p$fit),
             lo    = max(unname(p$fit) - half, 0),          # counts can't go below 0
             hi    = unname(p$fit) + half,
             x_bar = yr + 0.04 * diff(range(.x$year)))      # a little right of the data
    }) |>
    ungroup() |>
    drop_na(fit, lo, hi)
  
  ggplot(species_data, aes(x = year, y = count)) +
    geom_point(color = "grey35", size = 1.8, alpha = 0.7) +
    # curvy trend line with shaded confidence band
    geom_smooth(data = smooth_data, method = "loess", formula = y ~ x, span = span,
                se = TRUE, color = "firebrick", fill = "firebrick",
                alpha = 0.2, linewidth = 1.1) +
    # vertical CI bar on the right side of each panel
    geom_linerange(data = end_ci, aes(x = x_bar, ymin = lo, ymax = hi),
                   inherit.aes = FALSE, color = "firebrick", linewidth = 1.4) +
    geom_point(data = end_ci, aes(x = x_bar, y = fit),
               inherit.aes = FALSE, color = "firebrick", size = 2.2) +
    facet_grid(season ~ region, scales = "free_x") +        # each panel keeps its own years
    scale_x_continuous(expand = expansion(mult = c(0.03, 0.10))) +   # room for the bar
    coord_cartesian(ylim = c(0, NA)) +
    labs(title    = paste("Count trends:", species_name),
         subtitle = "Curved trend with 95% confidence band; side bar = 95% CI of the final smoothed value",
         x = "Year", y = "Count") +
    theme_minimal(base_size = 12) +
    theme(strip.text = element_text(face = "bold"))
}

# plot any species by name:
print(plot_species("Wood Duck"))

# =============================================================
# CLICKABLE DROPDOWN FOR ALL SPECIES (opens in your browser)
# Builds species_trends.html: pick any species from the dropdown.
# No Shiny needed. Needs internet to load the Plotly library.
# =============================================================
plot_data <- all_data |>
  mutate(species = str_squish(species)) |>
  filter(!is.na(species), species != "") |>
  drop_na(count) |>                 # blank cells = not measured that year
  group_by(species, region, season, year) |>
  summarise(count = sum(count), .groups = "drop")

# smoothed curve + 95% band for every species / region / season
fit_curve <- function(d) {
  fit <- tryCatch(suppressWarnings(loess(count ~ year, data = d, span = span_value)),
                  error = function(e) NULL)
  if (is.null(fit)) return(tibble())
  grid <- seq(min(d$year), max(d$year), length.out = 60)
  p <- tryCatch(suppressWarnings(predict(fit, data.frame(year = grid), se = TRUE)),
                error = function(e) NULL)
  if (is.null(p)) return(tibble())
  half <- qt(0.975, p$df) * p$se.fit
  tibble(year = grid,
         fit  = as.numeric(p$fit),
         lo   = pmax(as.numeric(p$fit) - half, 0),
         hi   = as.numeric(p$fit) + half) |>
    drop_na()
}

# the curves take a while, so they are saved after the first run
# (delete curve_data.rds if your data changes)
if (file.exists("curve_data.rds")) {
  curve_data <- readRDS("curve_data.rds")
} else {
  curve_data <- plot_data |>
    group_by(species, region, season) |>
    filter(n() >= 8) |>             # need enough years for a curve
    group_modify(~ fit_curve(.x)) |>
    ungroup() |>
    mutate(across(c(year, fit, lo, hi), ~ round(.x, 2)))
  saveRDS(curve_data, "curve_data.rds")
}

points_json <- toJSON(plot_data,  dataframe = "columns", na = "null", digits = NA)
curves_json <- toJSON(curve_data, dataframe = "columns", na = "null", digits = NA)

html_template <- r"---(<!DOCTYPE html>
<html>
<head>
<meta charset="utf-8">
<title>Bird Species Count Trends</title>
<script src="https://cdn.plot.ly/plotly-2.35.2.min.js"></script>
<style>
  body { font-family: Arial, sans-serif; margin: 20px; color: #222; }
  h1 { font-size: 22px; margin-bottom: 10px; }
  .controls { display: flex; gap: 24px; align-items: center; margin-bottom: 12px; flex-wrap: wrap; }
  label { font-weight: bold; margin-right: 6px; }
  select { font-size: 15px; padding: 6px 8px; min-width: 280px; }
  #plot { width: 100%; height: 680px; }
  .hint { color: #777; font-size: 13px; }
</style>
</head>
<body>
<h1>Bird Species Count Trends</h1>
<div class="controls">
  <div>
    <label for="sp">Species:</label>
    <select id="sp"></select>
  </div>
  <span class="hint">Shaded band = 95% CI of the curve; side bar = 95% CI of the final smoothed value. Hover for values, drag to zoom.</span>
</div>
<div id="plot"></div>

<script>
const D = __DATA__;      // yearly counts
const C = __CURVES__;    // smoothed curves + confidence bands
const regions = ["Durham", "Jordan Lake", "Chapel Hill"];
const seasons = ["Spring", "Winter"];

function indexBy(obj) {
  const m = {};
  const sp = obj.species || [];
  for (let i = 0; i < sp.length; i++) { (m[sp[i]] = m[sp[i]] || []).push(i); }
  return m;
}
const ptsBy = indexBy(D), curBy = indexBy(C);
const speciesList = Object.keys(ptsBy).sort();

function draw(sp) {
  const pi = ptsBy[sp] || [];
  const ci = curBy[sp] || [];
  if (!pi.length) return;

  const traces = [], annotations = [];
  const layout = { title: { text: sp + ": yearly counts with smoothed trend" },
                   margin: { t: 80, r: 20 }, annotations: annotations, showlegend: false,
                   grid: { rows: 2, columns: 3, pattern: "independent", xgap: 0.08, ygap: 0.2 } };

  seasons.forEach((se, i) => {
    regions.forEach((re, j) => {
      const pts = pi.filter(k => D.region[k] === re && D.season[k] === se)
                    .sort((a, b) => D.year[a] - D.year[b]);
      const x = pts.map(k => D.year[k]), y = pts.map(k => D.count[k]);

      const cidx = ci.filter(k => C.region[k] === re && C.season[k] === se);
      const cv = { x: cidx.map(k => C.year[k]), fit: cidx.map(k => C.fit[k]),
                   lo: cidx.map(k => C.lo[k]),  hi: cidx.map(k => C.hi[k]) };
      const hasCurve = cv.x.length > 1;
      const last = cv.x.length - 1;
      const name = re + " - " + se;

      const panel = i * 3 + j + 1;
      const sfx = panel === 1 ? "" : String(panel);
      const ax = { xaxis: "x" + sfx, yaxis: "y" + sfx };

      traces.push(Object.assign({
        x: x, y: y, mode: "markers", type: "scatter", showlegend: false,
        marker: { color: "#555", size: 6, opacity: 0.7 },
        hovertemplate: "Year %{x}<br>Count %{y}<extra>" + name + "</extra>"
      }, ax));

      if (hasCurve) {
        const xb = cv.x[last] + 0.04 * (cv.x[last] - cv.x[0]);
        traces.push(Object.assign({ x: cv.x, y: cv.hi, mode: "lines", type: "scatter",
          line: { width: 0 }, showlegend: false, hoverinfo: "skip" }, ax));
        traces.push(Object.assign({ x: cv.x, y: cv.lo, mode: "lines", type: "scatter",
          line: { width: 0 }, fill: "tonexty", fillcolor: "rgba(178,34,34,0.2)",
          showlegend: false, hoverinfo: "skip" }, ax));
        traces.push(Object.assign({ x: cv.x, y: cv.fit, mode: "lines", type: "scatter",
          line: { color: "firebrick", width: 3 }, showlegend: false,
          hovertemplate: "Smoothed: %{y:.1f}<extra></extra>" }, ax));
        traces.push(Object.assign({ x: [xb, xb], y: [cv.lo[last], cv.hi[last]],
          mode: "lines", type: "scatter", line: { color: "firebrick", width: 5 },
          showlegend: false,
          hovertemplate: "95% CI of final value: " + cv.lo[last].toFixed(1) + " to " +
                         cv.hi[last].toFixed(1) + "<extra></extra>" }, ax));
        traces.push(Object.assign({ x: [xb], y: [cv.fit[last]], mode: "markers", type: "scatter",
          marker: { color: "firebrick", size: 9 }, showlegend: false, hoverinfo: "skip" }, ax));
      }

      layout["xaxis" + sfx] = { tickformat: "d" };
      layout["yaxis" + sfx] = { rangemode: "tozero" };
      const xgap = 0.08, ygap = 0.2;
      const w = (1 - 2 * xgap) / 3, h = (1 - ygap) / 2;
      annotations.push({ text: "<b>" + name + "</b>", showarrow: false,
        xref: "paper", yref: "paper", xanchor: "center", yanchor: "bottom",
        x: j * (w + xgap) + w / 2, y: 1 - i * (h + ygap) + 0.01 });
    });
  });

  Plotly.react("plot", traces, layout, { responsive: true });
}

// fill the dropdown with every species
const spBox = document.getElementById("sp");
speciesList.forEach(s => {
  const o = document.createElement("option"); o.value = s; o.textContent = s; spBox.appendChild(o);
});
spBox.value = speciesList.includes("Canada Goose") ? "Canada Goose" : speciesList[0];

draw(spBox.value);
spBox.addEventListener("change", () => draw(spBox.value));
</script>
</body>
</html>
)---"

fill_in <- function(x, key, value) {
  p <- strsplit(x, key, fixed = TRUE)[[1]]
  paste0(p[1], value, p[2])          # drop the data into the page
}
html <- html_template |>
  fill_in("__DATA__",   points_json) |>
  fill_in("__CURVES__", curves_json)

out_file <- "species_trends.html"
writeLines(html, out_file, useBytes = TRUE)
message("Created ", normalizePath(out_file))
browseURL(out_file)   # opens the page in your default browser