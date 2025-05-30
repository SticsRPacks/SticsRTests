
library(SticsRFiles)
library(SticsOnR)

stics_version <- "V10.0"

path_to_JavaStics = system.file("stics", package = "SticsRTests")
javastics = normalizePath(file.path(path_to_JavaStics,stics_version), winslash = "/")

# Define the workspaces ---------------------------------------------------

data_dir = SticsRFiles::download_data(example_dirs="study_case_1",
                                      stics_version = stics_version)

workspace = file.path(data_dir, "XmlFiles")

# Define the variables to simulate ----------------------------------------

sim_variables = c("lai(n)","masec(n)")
# Generate the var.mod file:
gen_varmod(workspace, sim_variables)

# Run the simulations -----------------------------------------------------

# Usms to simul te (you can subset if needed)
usms = get_usms_list(file = file.path(workspace,"usms.xml"))
usms = usms[1:3]

# Standard run ------------------------------------------------------------

test_that("run_javastics works using standard call", {
  usms_out = run_javastics(javastics = javastics,
                           workspace = workspace,
                           usm = usms, verbose = FALSE)
  errored = unlist(lapply(usms_out, function(x) x$error))

  expect_true(all(!errored))
})


# Using a custom exe ------------------------------------------------------

custom_stics_exe =
  switch (SticsRFiles:::user_os(),
          win = "stics_custom.exe",
          lin = "stics_custom_lin",
          #mac = "stics_custom_mac",
  )

test_that("run_javastics works using a custom executable: exe path", {
  usms_out = run_javastics(javastics = javastics,
                           workspace = workspace,
                           stics_exe = file.path(javastics,"custom_exe",custom_stics_exe),
                           usm = usms, verbose = FALSE)
  errored = unlist(lapply(usms_out, function(x) x$error))

  expect_true(all(!errored))
})

test_that("run_javastics works using a custom executable: exe name", {
  usms_out = run_javastics(javastics = javastics,
                           workspace = workspace,
                           stics_exe = custom_stics_exe,
                           usm = usms, verbose = FALSE)
  errored = unlist(lapply(usms_out, function(x) x$error))

  expect_true(all(!errored))
})

test_that("run_javastics works using a custom executable: exe label", {
  usms_out = run_javastics(javastics = javastics,
                           workspace = workspace,
                           stics_exe = paste0("stics_custom_",
                                              SticsRFiles:::user_os()),
                           usm = usms, verbose = FALSE)
  errored = unlist(lapply(usms_out, function(x) x$error))

  expect_true(all(!errored))
})


# Remove copy of exe: -----------------------------------------------------

file.remove(file.path(javastics,"bin",custom_stics_exe))

