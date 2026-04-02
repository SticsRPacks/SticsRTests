#' ---
#' title: "Tutorial for SticsRPacks"
#' output:
#'   learnr::tutorial:
#'     theme: lumen
#'     ace_theme: xcode
#'     progressive: false
#'     allow_skip: true
#'     df_print: "paged"
#'     toc: true
#'     toc_depth: 4
#'     allow_hide: true
#'     clean: true
#' description: "A tutorial to learn SticsRPacks"
#' runtime: shiny_prerendered
#' editor_options:
#'   markdown:
#'     wrap: sentence
#' ---
#'
## ----setup, include=FALSE---------------------------------------------------------------------------------------------------------------------------------------------
library(learnr)
library(SticsRPacks)
library(dplyr)
library(tidyr)
library(ggplot2)

# function for displaying functions help
display_help <- function(subject, pkg = NULL) {
  tc <- textConnection("s", "w", local = TRUE)
  tools:::Rd2HTML(utils:::.getHelpFile(help(subject, (pkg))), out = tc)
  knitr::asis_output(htmltools::htmlPreserve(s))
}

# function for extracting stics_version from version
# information returned by the stics executable through SticsOnR::check_stics_exe
get_stics_version <- function(version_string) {
  # Extract the version number from the string
  gsub(
    pattern = "([a-z]*[_]{0,1}[v|V]*)([0-9]{1,}.[0-9]{1,}.[0-9]{1,})(_)([0-9-]*)",
    x = version_string,
    replacement = "\\2"
  )
}
# for simplifying use
get_version_num <- SticsRFiles:::get_version_num

# removing warnings in chunks
knitr::opts_chunk$set(warning = FALSE)

options(stringsAsFactors = FALSE)

tutorial_options(exercise.lines = 5, exercise.timelimit = 300)

# OS specific parameters
linux <- SticsRFiles:::is_unix()
attributes(linux) <- NULL

#--------------------------------------------------------------------------
# Prior to getting this environment variable put it in
# a .Renviron file (in user base directory: Documents for windows,
# /home/username)
# usethis::edit_r_environ() may be used to edit the file
# In any case the R session must be restarted to take effect
#--------------------------------------------------------------------------

# Detecting which JavaStics to use: local specific version or
# the latest version downloaded from
# https://w3.avignon.inrae.fr/forge
#
javastics_in_path <- Sys.getenv("javastics_path")
local_javastics <- !(javastics_in_path == "")

work_dir <- file.path(
  normalizePath(dirname(tempdir()), winslash = "/"),
  "SticsRPacks"
)

tmp_dir <- file.path(work_dir, "tmp")

# For avoiding problems under windows
# with spaces in user dir path
if (.Platform$OS.type == "windows") {
  work_dir <- shortPathName(work_dir)
}
# Replacing Windows double backslash file separator with slash
work_dir <- gsub(pattern = "\\\\", replacement = "/", x = work_dir)

# Creating work_dir if needed
if (!dir.exists(work_dir)) {
  dir.create(work_dir)
}
# Creating a tmp dir or emptying it if it exists
if (!dir.exists(tmp_dir)) {
  dir.create(tmp_dir)
} else {
  tmp_files <- list.files(
    path = tmp_dir,
    pattern = "\\.RData$",
    full.names = TRUE
  )
  file.remove(tmp_files)
}


if (!local_javastics) {
  stics_version <- "latest"
  download_url <- "https://w3.avignon.inrae.fr/forge/attachments/download/3651/JavaSTICS-latest.zip"

  javastics_path <- SticsRPacks:::download_javastics(
    download_url = download_url,
    output_dir = work_dir
  )

  workspace_path <- file.path(javastics_path, "example", fsep = "/")

  workspace_PW1 <- "tutorials/SticsRpacks/PW1_V10"

  # getting java version from jre embedded in JavaSTICS
  java_target <- SticsRPacks:::get_javastics_java_version(javastics_path)
} else {
  # Check JavaStics in path, got from javastics_path env. variable
  if (
    !dir.exists(javastics_in_path) ||
      !any(grepl("JavaStics.exe", list.files(javastics_in_path)))
  ) {
    stop(
      javastics_in_path,
      ": JavaStics directory set in 'javastics_path'\n",
      "environment variable does not exist, aborting"
    )
  }

  # Using the local JavaStics path
  javastics_path <- javastics_in_path

  # Getting exe name
  stics_exe_name <- SticsOnR:::list_stics_exe(javastics_path)[[2]][[1]]

  # Getting executable version
  check <- try(
    SticsOnR:::check_stics_exe(
      file.path(javastics_path, "bin", stics_exe_name),
      version = TRUE
    ),
    silent = TRUE
  )

  # Stopping the tutorial when the executable is not found
  if (class(check) == "try-error") {
    stop(
      "The Stics executable (",
      stics_exe_name,
      ") was not found in JavaStics bin directory: ",
      javastics_path
    )
  }

  # getting the STICS numerical version
  stics_version_string <- attr(check, "version")
  stics_version <- get_stics_version(stics_version_string)

  stics_version_num <- SticsRFiles:::get_version_num(
    stics_version
  )
}
