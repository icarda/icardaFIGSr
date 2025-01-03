# Load required libraries
library(roxygen2)
library(pkgdown)
library(yaml)

# Step 1: Get a list of all datasets in the `data/` folder
dataset_files <- list.files("data", pattern = "\\.rda$", full.names = TRUE)
dataset_names <- tools::file_path_sans_ext(basename(dataset_files))

# Step 2: Ensure all datasets have documentation in the `man/` folder
for (dataset in dataset_names) {
  # Check if .Rd file exists
  rd_file <- file.path("man", paste0(dataset, ".Rd"))
  if (!file.exists(rd_file)) {
    warning(paste("Dataset", dataset, "is missing documentation!"))
  }
}

# Step 3: Update _pkgdown.yml with dataset references
# Load the existing _pkgdown.yml file
pkgdown_file <- "_pkgdown.yml"
pkgdown_config <- yaml::read_yaml(pkgdown_file)

# Add datasets to the "Datasets" section under `reference`
dataset_section <- list(
  title = "Datasets",
  contents = dataset_names
)

# Update the reference section
reference_sections <- pkgdown_config$reference$sections
reference_sections <- lapply(reference_sections, function(section) {
  if (section$title == "Datasets") {
    section$contents <- dataset_names
  }
  return(section)
})

# If no "Datasets" section exists, append it
if (!any(sapply(reference_sections, function(section) section$title == "Datasets"))) {
  reference_sections <- append(reference_sections, list(dataset_section))
}

pkgdown_config$reference$sections <- reference_sections

# Step 4: Save the updated _pkgdown.yml file
yaml::write_yaml(pkgdown_config, pkgdown_file)

# Step 5: Build the pkgdown site
pkgdown::build_site()