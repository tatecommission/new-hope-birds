# Rebuild all six location/season habitat CSVs.
# From the project root: Rscript r-files/build-habitat-data.R
# Dependency: jsonlite (install.packages("jsonlite") if unavailable).
# All input CSVs are read-only. Habitat assignments and their sources are in
# habitat_species_mapping.json; see habitat_data_notes.txt for count conventions.

stopifnot(requireNamespace("jsonlite", quietly = TRUE))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(script_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1]))))
} else if (dir.exists("data")) {
  normalizePath(".")
} else {
  normalizePath("..")
}
stopifnot(dir.exists(file.path(root, "data")))
mapping <- jsonlite::fromJSON(
  file.path(root, "r-files", "habitat_species_mapping.json"),
  simplifyVector = FALSE
)
groups <- c("forest_birds", "grassland_birds", "aridland_birds",
            "wetland_and_coastal_birds", "tundra_birds", "habitat_generalists")
labels <- c("Forest birds", "Grassland birds", "Aridland birds",
            "Wetland and coastal birds", "Tundra birds", "Habitat generalists")
missing_tokens <- c("", "NA", "N/A", "*", "?", "x")
marker_tokens <- c("*", "?", "x")
audit <- list(
  definition = paste("Raw observed counts grouped by primary breeding habitat.",
                     "Not effort-adjusted population estimates."),
  sources = list(), excluded_records = list(), non_numeric_records = list(),
  habitat_coverage = list(), year_reconciliation = list()
)
outputs <- list()

read_text_csv <- function(file, header = TRUE) {
  read.csv(file, header = header, colClasses = "character", check.names = FALSE,
           na.strings = character(), stringsAsFactors = FALSE,
           blank.lines.skip = FALSE, strip.white = FALSE, fill = TRUE)
}

for (season in c("spring", "winter")) {
  input_dir <- file.path(root, "data", season)
  files <- list.files(input_dir, pattern = "_final\\.csv$")
  raw_durham <- "Durham-1968-2023-winter.xlsx - DURCBC.csv"
  if (season == "winter" && !"durham_winter_final.csv" %in% files) {
    files <- c(files, raw_durham)
  }
  for (file in sort(files)) {
    input_path <- file.path(input_dir, file)
    is_raw <- identical(file, raw_durham)
    if (is_raw) {
      raw <- read_text_csv(input_path, header = FALSE)
      stopifnot(raw[4, 3] == "Common Name", raw[4, 5] == "1968",
                raw[4, 59] == "2023", raw[185, 3] == "reported # species")
      # Only E:BG are annual counts. Later columns include statistics, duplicate
      # recent-year counts and spreadsheet formula errors; do not aggregate them.
      bird_rows <- 6:183
      bird_rows <- bird_rows[nzchar(trimws(raw[bird_rows, 3]))]
      data <- raw[bird_rows, c(3, 5:59), drop = FALSE]
      names(data) <- c("COMMON NAME", as.character(unlist(raw[4, 5:59])))
      data[] <- lapply(data, trimws)
      # Blank count cells in the original annual count block mean no recorded
      # birds. Preserve * and x as unknown counts, not numeric zeros.
      for (j in 2:ncol(data)) data[[j]][data[[j]] == ""] <- "0"
      dataset <- "durham_winter_final"
      preparation <- paste("Raw header row 4, annual columns E:BG, nonblank bird",
        "rows 6:183. Blank count cells become zero; * and x stay unknown.",
        "Exclude summary, effort, statistics, and repeated count columns.")
    } else {
      data <- read_text_csv(input_path)
      dataset <- sub("\\.csv$", "", file)
      preparation <- "Existing final CSV unchanged."
    }
    species_labels <- trimws(data[[1]])
    years <- names(data)[-1]
    stopifnot(all(grepl("^[0-9]{4}$", years)), !anyDuplicated(years),
              !anyDuplicated(species_labels))
    unmapped <- setdiff(species_labels, names(mapping))
    if (length(unmapped)) stop("Unmapped labels in ", file, ": ", paste(unmapped, collapse = ", "))
    row_mapping <- mapping[species_labels]
    habitats <- vapply(row_mapping, function(x) if (is.null(x$habitat)) NA_character_ else x$habitat, character(1))
    canonical <- vapply(row_mapping, function(x) if (is.null(x$species)) NA_character_ else x$species, character(1))
    text <- as.matrix(data[-1])
    text[] <- trimws(text)
    invalid <- !(text %in% missing_tokens | grepl("^[0-9]+$", text))
    if (any(invalid)) stop("Unexpected nonnumeric count in ", file, ": ", paste(unique(text[invalid]), collapse = ", "))
    counts <- matrix(suppressWarnings(as.numeric(text)), nrow = nrow(data),
                     dimnames = list(species_labels, years))
    stopifnot(all(is.na(counts) | (is.finite(counts) & counts >= 0 & counts == floor(counts))))

    marker_cells <- which(matrix(text %in% marker_tokens, nrow = nrow(data)), arr.ind = TRUE)
    for (k in seq_len(nrow(marker_cells))) {
      i <- marker_cells[k, 1]; j <- marker_cells[k, 2]
      audit$non_numeric_records[[length(audit$non_numeric_records) + 1L]] <-
        list(dataset = dataset, label = species_labels[i], year = as.integer(years[j]), value = text[i, j])
    }
    excluded <- which(is.na(habitats))
    for (i in excluded) {
      audit$excluded_records[[length(audit$excluded_records) + 1L]] <- list(
        dataset = dataset, label = species_labels[i], reason = row_mapping[[i]]$basis,
        counts = as.list(setNames(counts[i, ], years)))
    }

    result <- matrix(0, nrow = length(groups), ncol = length(years),
                     dimnames = list(labels, years))
    for (g in seq_along(groups)) {
      selected <- which(!is.na(habitats) & habitats == groups[g])
      by_species <- split(selected, canonical[selected])
      for (j in seq_along(years)) {
        total <- 0; numeric_species <- 0L; missing_species <- 0L; marked_species <- 0L
        for (indices in by_species) {
          values <- counts[indices, j]
          # Alias rows represent alternate spellings across years. Do not
          # silently double-count positive values for the same species/year.
          if (sum(values > 0, na.rm = TRUE) > 1L) {
            stop("Overlapping positive alias counts: ", dataset, ", ",
                 canonical[indices[1]], ", ", years[j])
          }
          if (all(is.na(values))) {
            missing_species <- missing_species + 1L
          } else {
            total <- total + sum(values, na.rm = TRUE)
            numeric_species <- numeric_species + 1L
          }
          if (any(text[indices, j] %in% marker_tokens)) marked_species <- marked_species + 1L
        }
        # An empty habitat group is zero; an entirely missing group is NA.
        result[g, j] <- if (numeric_species == 0L && missing_species > 0L) NA_real_ else total
        audit$habitat_coverage[[length(audit$habitat_coverage) + 1L]] <- list(
          dataset = dataset, habitat = groups[g], year = as.integer(years[j]),
          bird_count = result[g, j], species_with_numeric_counts = numeric_species,
          species_with_missing_counts = missing_species,
          species_with_non_numeric_markers = marked_species)
      }
    }
    # Terminal checks: every numeric source value appears either in one habitat
    # sum or in the explicit excluded-record audit, with no duplicate allocation.
    raw_totals <- colSums(counts, na.rm = TRUE)
    excluded_totals <- colSums(counts[excluded, , drop = FALSE], na.rm = TRUE)
    assigned_totals <- colSums(result, na.rm = TRUE)
    stopifnot(all(assigned_totals + excluded_totals == raw_totals))
    for (j in seq_along(years)) {
      audit$year_reconciliation[[length(audit$year_reconciliation) + 1L]] <- list(
        dataset = dataset, year = as.integer(years[j]), input_numeric_sum = raw_totals[j],
        habitat_numeric_sum = assigned_totals[j], excluded_numeric_sum = excluded_totals[j])
    }
    output_name <- paste0(sub("_final$", "_habitat", dataset), ".csv")
    output <- data.frame(habitat = labels, result, check.names = FALSE, row.names = NULL)
    outputs[[output_name]] <- output
    audit$sources[[length(audit$sources) + 1L]] <- list(
      file = file.path("data", season, file), dataset = dataset, output = output_name,
      md5 = unname(tools::md5sum(input_path)), input_rows = nrow(data),
      years = length(years), first_year = years[1], last_year = tail(years, 1),
      preparation = preparation)
  }
}

stopifnot(length(outputs) == 6L)
output_dir <- file.path(root, "habitat_data")
dir.create(output_dir, showWarnings = FALSE)
for (filename in names(outputs)) {
  output <- outputs[[filename]]
  target <- file.path(output_dir, filename)
  write.csv(output, target, row.names = FALSE, na = "NA")
  saved <- read.csv(target, check.names = FALSE, stringsAsFactors = FALSE)
  stopifnot(identical(names(saved), names(output)), identical(saved$habitat, labels),
            isTRUE(all.equal(as.matrix(saved[-1]), as.matrix(output[-1]), check.attributes = FALSE)))
  message(filename, ": ", nrow(saved), " habitat rows, ", ncol(saved) - 1L, " years")
}
jsonlite::write_json(audit, file.path(root, "r-files", "habitat_data_audit.json"),
                     auto_unbox = TRUE, pretty = TRUE, na = "null", digits = NA)
message("Verified ", length(audit$year_reconciliation), " annual reconciliations across six datasets.")
