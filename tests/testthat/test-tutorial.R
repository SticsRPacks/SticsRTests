
#Download and transform Tutorial .RMD to .R
tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
tutorial_rmd <- file.path(tmpdir, "SticsRpacks.Rmd")
download.file("https://raw.githubusercontent.com/SticsRPacks/SticsRPacks/main/inst/tutorials/SticsRpacks/SticsRpacks.Rmd",
              tutorial_rmd)
xfun::gsub_file(file = tutorial_rmd,
                "eval=FALSE","eval=TRUE",
                fixed = TRUE)

xfun::gsub_file(file = tutorial_rmd,
                "optim_options = list(nb_rep = 3, out_dir = workspace_path)",
                "optim_options = list(nb_rep = 3, out_dir = workspace_path, maxeval = 3)",
                fixed = TRUE)

if (!Sys.getenv("CI") != "") {
  xfun::gsub_file(file = tutorial_rmd,
                  "parallel = TRUE","parallel = TRUE, cores = 2",
                  fixed = TRUE)
}

tutorial_r <-file.path(tmpdir, "tutorial.R")
knitr::purl(input = tutorial_rmd,
            output = tutorial_r,
            documentation = 2)


# Test Tutorial
test_tuto <- function() {
  source(tutorial_r)
  return (TRUE)
}

test_that("Test Tutorial", {
  expect_equal(test_tuto(), TRUE)
})
