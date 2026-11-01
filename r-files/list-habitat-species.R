# Compile the species list from the same mapping used by build-habitat-data.R.
# Run from project root: Rscript r-files/list-habitat-species.R
stopifnot(requireNamespace("jsonlite", quietly = TRUE))
script_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
root <- if (length(script_arg)) {
  dirname(dirname(normalizePath(sub("^--file=", "", script_arg[1]))))
} else if (dir.exists("data")) normalizePath(".") else normalizePath("..")
mapping <- jsonlite::fromJSON(file.path(root, "r-files", "habitat_species_mapping.json"),
                              simplifyVector = FALSE)
groups <- c("forest_birds", "grassland_birds", "aridland_birds",
            "wetland_and_coastal_birds", "tundra_birds", "habitat_generalists")
titles <- c("Forest birds", "Grassland birds", "Aridland birds",
            "Wetland and coastal birds", "Tundra birds", "Habitat generalists")
assigned <- Filter(function(x) !is.null(x$habitat), mapping)
species <- vapply(assigned, `[[`, character(1), "species")
assigned <- assigned[!duplicated(species)]
stopifnot(length(assigned) == 278L)
lines <- c(
  "SPECIES BY HABITAT", "",
  "Scope: named species in the six location/season datasets used for habitat summaries.",
  "276 distinct named species plus two hybrids. Alternate source spellings are consolidated.",
  "The lists use primary breeding habitat, including for winter sightings in North Carolina.",
  "Unidentified birds and summary rows are not assigned to a habitat and are not listed.", "",
  "Unmarked entries follow Rosenberg et al. (2019), Data S1, Breeding.Biome.",
  "The original biome is shown in parentheses. Four forest biomes are combined;",
  "Wetland and Coasts are combined. The other four categories are retained.",
  "[S] indicates a supplemental project assignment, not a Data S1 habitat assignment.",
  "[H] indicates a hybrid assigned from the habitats of both parental species.", "",
  "Primary reference: https://www.science.org/doi/10.1126/science.aaw1313",
  "Data S1 copy: https://www.birds.cornell.edu/home/wp-content/uploads/2019/09/DECLINE-OF-NORTH-AMERICAN-AVIFAUNA-SCIENCE-2019.pdf#page=56", ""
)
for (g in seq_along(groups)) {
  members <- Filter(function(x) identical(x$habitat, groups[g]), assigned)
  members <- members[order(tolower(vapply(members, `[[`, character(1), "species")))]
  lines <- c(lines, paste0(toupper(titles[g]), " (", length(members), " entries)"))
  for (m in members) {
    tag <- if (startsWith(m$basis, "Supplementary")) " [S]" else
      if (grepl("Hybrid|parental", m$basis)) " [H]" else ""
    description <- if (tag == "") paste0(" (", m$biome, ")") else ""
    lines <- c(lines, paste0("- ", m$species, description, tag))
  }
  lines <- c(lines, "")
}
lines <- c(lines, "SUPPLEMENTAL AND HYBRID ASSIGNMENTS", "")
special <- Filter(function(x) x$basis != "Data S1 breeding biome", assigned)
special <- special[order(vapply(special, `[[`, character(1), "species"))]
for (m in special) {
  lines <- c(lines, paste0(m$species, " -> ", titles[match(m$habitat, groups)]),
             paste0("  Basis: ", m$basis), paste0("  Source: ", m$source), "")
}
lines <- c(lines,
  "The study gives the introduced species an Introduced label, rather than one",
  "of its ten habitat labels. Mute Swan is assigned to wetlands/coasts; European",
  "Starling, House Sparrow, and Rock Pigeon to habitat generalists, including",
  "human-modified environments. The generalist placements are project conventions.",
  "Iceland Gull and Lesser Black-backed Gull use coastal breeding habitats;",
  "Red Phalarope breeds on tundra. These three are absent from Data S1.",
  "Brewster's Warbler parents: Blue-winged Warbler and Golden-winged Warbler.",
  "The other hybrid's parents are White-throated Sparrow and Dark-eyed Junco.", "",
  "For every original source label and alias, see r-files/habitat_species_mapping.json.",
  "For unidentified records and missing counts, see r-files/habitat_data_audit.json."
)
target <- file.path(root, "habitat_data", "species_by_habitat.txt")
writeLines(lines, target, useBytes = TRUE)
stopifnot(sum(grepl("^- ", readLines(target))) == length(assigned))
message("Wrote ", target, " with ", length(assigned), " unique species/hybrid entries.")
