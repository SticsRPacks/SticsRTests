options(warn = -1)

library(xfun)
library(knitr)
if (any("SticsRFiles" == .packages())) {
  options('future.debug' = FALSE)
}

# Getting the tutorial path from the SticsRPacks library
tutorial_rmd <- normalizePath(
  system.file("tutorials/SticsRpacks/SticsRpacks.Rmd", package = "SticsRPacks"),
  mustWork = TRUE
)

# Making a copy of the tutorial
test_dir <- file.path(tempdir(), "SticsRpacks")
if (!dir.exists(test_dir)) dir.create(test_dir)
tutorial_test_rmd <- file.path(test_dir, "SticsRpacks_test.Rmd")
file.copy(from = tutorial_rmd, to = tutorial_test_rmd)

# gsub some parts of the tutorial for activating chunks evaluation
# and masking some functions return objects

# substitute the dowload url of the latest version of JavaStics
xfun::gsub_file(
  file = tutorial_test_rmd,
  "download_url <-.*",
  "download_url <- \"https://w3.avignon.inrae.fr/forge/attachments/download/3727/JavaSTICS-latest.zip\"",
  fixed = FALSE
)

xfun::gsub_file(
  file = tutorial_test_rmd,
  "eval=FALSE",
  "eval=TRUE",
  fixed = TRUE
)

xfun::gsub_file(
  file = tutorial_test_rmd,
  "optim_options = list(nb_rep = 3, out_dir = workspace_path)",
  "optim_options = list(nb_rep = 3, out_dir = workspace_path, maxeval = 3)",
  fixed = TRUE
)

if (Sys.getenv("CI") != "") {
  xfun::gsub_file(
    file = tutorial_test_rmd,
    "parallel = TRUE",
    "parallel = TRUE, cores = 2",
    fixed = TRUE
  )
}

# removing outputs from get_sim
xfun::gsub_file(
  file = tutorial_test_rmd,
  "get_sim(workspace = workspace_path)",
  "x <- get_sim(workspace = workspace_path)",
  fixed = TRUE
)

xfun::gsub_file(
  file = tutorial_test_rmd,
  'get_sim(workspace = workspace_path, usm = c("banana", "Turmeric"))',
  'x <- get_sim(workspace = workspace_path, usm = c("banana", "Turmeric"))',
  fixed = TRUE
)

xfun::gsub_file(
  file = tutorial_test_rmd,
  'get_sim(workspace = workspace_path, usm = c("wheat", "maize"), var = "lai_n")',
  'x <- get_sim(workspace = workspace_path, usm = c("wheat", "maize"), var = "lai_n")',
  fixed = TRUE
)

xfun::gsub_file(file = tutorial_test_rmd, "print(sim)", "", fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd, "print(p)", "", fixed = TRUE)


xfun::gsub_file(file = tutorial_test_rmd, "print(res1)", "", fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd, "print(res2)", "", fixed = TRUE)

# Creating a R script from the tutorial Rmd file
tutorial_test_r <- file.path(test_dir, "tutorial_test.R")

knitr::purl(
  input = tutorial_test_rmd,
  output = tutorial_test_r,
  documentation = 2
)


# Running the Tutorial script for
# Default use case with downloading JavaStics from the forge
test_tuto_download_javastics <- function(tutorial_test_r) {
  javastics_in_path <- Sys.setenv(javastics_path = "")
  status <- system(paste("Rscript", tutorial_test_r), intern = FALSE)
  as.logical(status + 1)
}
# Local JavaStics installation use case
test_tuto_local_javastics <- function(tutorial_test_r) {
  stics_version <- SticsRFiles::get_stics_versions_compat()$latest_version
  javastics_path <- system.file(
    paste0("stics/", stics_version),
    package = "SticsRTests"
  )
  Sys.setenv(javastics_path = javastics_path)
  status <- system(paste("Rscript", tutorial_test_r), intern = FALSE)
  as.logical(status + 1)
}

test_that("Test Tutorial", {
  expect_equal(test_tuto_download_javastics(tutorial_test_r), TRUE)
  expect_equal(test_tuto_local_javastics(tutorial_test_r), TRUE)
})

unlink(tutorial_test_rmd)

unlink(tutorial_test_r)
