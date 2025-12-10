options(warn=-1)

# import::from(xfun, xfun::gsub_file)
# import::from(knitr, purl)
library(xfun)
library(knitr)

# Getting the tutorial path from the SticsRPacks library
tutorial_rmd <- normalizePath(
  system.file("tutorials/SticsRpacks/SticsRpacks.Rmd", package = "SticsRPacks"),
  mustWork = TRUE
)

# Making a copy of the tutorial
tutorial_test_rmd <- "SticsRpacks_test.Rmd"
file.copy(from = tutorial_rmd,
          to = tutorial_test_rmd)

# gsub some parts of the tutorial for activating chunks evaluation
# and masking some functions return objects
xfun::gsub_file(file = tutorial_test_rmd,
          "eval=FALSE","eval=TRUE",
          fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd,
          "optim_options = list(nb_rep = 3, out_dir = workspace_path)",
          "optim_options = list(nb_rep = 3, out_dir = workspace_path, maxeval = 3)",
          fixed = TRUE)

if (Sys.getenv("CI") != "") {
  xfun::gsub_file(file = tutorial_test_rmd,
            "parallel = TRUE","parallel = TRUE, cores = 2",
            fixed = TRUE)
}

# removing outputs from get_sim
xfun::gsub_file(file = tutorial_test_rmd,
          "get_sim(workspace = workspace_path)",
          "x <- get_sim(workspace = workspace_path)",
          fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd,
          'get_sim(workspace = workspace_path, usm = c("banana", "Turmeric"))',
          'x <- get_sim(workspace = workspace_path, usm = c("banana", "Turmeric"))',
          fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd,
          'get_sim(workspace = workspace_path, usm = c("wheat", "maize"), var = "lai_n")',
          'x <- get_sim(workspace = workspace_path, usm = c("wheat", "maize"), var = "lai_n")',
          fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd,
          "print(sim)",
          "",
          fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd,
          "print(p)",
          "",
          fixed = TRUE)


xfun::gsub_file(file = tutorial_test_rmd,
          "print(res1)",
          "",
          fixed = TRUE)

xfun::gsub_file(file = tutorial_test_rmd,
          "print(res2)",
          "",
          fixed = TRUE)

# Creating a R script from the tutorial Rmd file
tutorial_test_r <- "tutorial_test.R"

knitr::purl(input = tutorial_test_rmd,
     output = tutorial_test_r,
     documentation = 2)

# Running the Tutorial script for
# Default use case with downloading JavaStics from the forge
#test_tuto_download_javastics <- function() {
#  javastics_in_path <- Sys.setenv(javastics_path="")
#  source(tutorial_test_r)
#  return (TRUE)
#}
# Local JavaStics installation use case
test_tuto_local_javastics <- function() {
  version <- get_stics_versions_compat()$latest_version
  javastics_path <- system.file(paste0("stics/", version),
                                package = "SticsRTests")
  Sys.setenv(javastics_path = javastics_path)
  source(tutorial_test_r)
  return (TRUE)
}

test_that("Test Tutorial", {
  #expect_equal(test_tuto_download_javastics(), TRUE)
  expect_equal(test_tuto_local_javastics(), TRUE)
})

unlink(tutorial_test_rmd)

unlink(tutorial_test_r)
