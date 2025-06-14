
context("Stics Wrapper")
stics_version <- "V10.1.0"

# fixing the number of cores in the context of Github CI
if (Sys.getenv("CI") != "") {
  cores <- 2
} else {
  cores <- NA
}


# Define path to JavaStics and download data
path_to_JavaStics <- system.file("stics", package = "SticsRTests")
javastics_path <- file.path(path_to_JavaStics, stics_version)

# Download and prepare data
data_dir <- file.path(SticsRFiles::download_data(example_dirs = "study_case_1", stics_version = stics_version))
stics_inputs_path <- file.path(data_dir, "TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)
javastics_workspace_path <- file.path(data_dir, "XmlFiles")
SticsRFiles::gen_usms_xml2txt(
  javastics = javastics_path, workspace = javastics_workspace_path,
  out_dir = stics_inputs_path, verbose = TRUE
)

# Set options for Stics wrapper
model_options <- SticsOnR::stics_wrapper_options(javastics = javastics_path, workspace = stics_inputs_path, parallel = FALSE)

# Standard wrapper tests
param_names <- c("dlaimax", "durvieF")
param_lb <- c(0.0005, 50)
param_values <- setNames(param_lb, param_names)

res <- CroptimizR::test_wrapper(
  model_function = SticsOnR::stics_wrapper,
  model_options = model_options,
  param_values = param_values,
  situation = "bo96iN+", var = "lai_n"
)

test_that("Stics Wrapper succeed test_wrapper tests", {
  expect_true(all(res$test_results))
})


# Test forcing of parameter per situation works
param_values <- data.frame(situation = "bo96iN+", dlaimax = 0.0005, durvieF = 50)
model_options <- SticsOnR::stics_wrapper_options(javastics = javastics_path, workspace = stics_inputs_path, parallel = FALSE)

var_name <- "lai_n"
sim_without_forcing <- SticsOnR::stics_wrapper(model_options = model_options, var = "lai_n", situation = c("bo96iN+", "bou99t1"))
sim_with_forcing <- SticsOnR::stics_wrapper(param_values = param_values, model_options = model_options, var = "lai_n", situation = c("bo96iN+", "bou99t1"))

test_that("Parameter forcing per situation works", {
  expect_gt(
    sum(
      abs(sim_with_forcing$sim_list[["bo96iN+"]][, var_name] -
        sim_without_forcing$sim_list[["bo96iN+"]][, var_name]),
      na.rm = TRUE
    ),
    0
  )
  expect_identical(sim_without_forcing$sim_list[["bou99t1"]], sim_with_forcing$sim_list[["bou99t1"]])
})


# Test selection of results using arguments situation, sit_var_dates_mask, var, dates
situation_names <- c("bo96iN+", "lu97iN+")
res <- SticsOnR::stics_wrapper(model_options = model_options, situation = situation_names)
test_that("Asking results for a list of situation using situation works", {
  expect_equal(situation_names, names(res$sim_list))
})
situation_names <- c("bo96iN+", "toto", "titi")
test_that("Asking results for a non existing USM using situation lead to a warning", {
  expect_warning(SticsOnR::stics_wrapper(model_options = model_options, situation = situation_names), regexp = "These USMs will not be simulated")
})
obs_list <- SticsRFiles::get_obs(javastics_workspace_path)
obs_list[["toto"]] <- obs_list[["bo96iN+"]]
test_that("Asking results for a non existing USM using sit_var_dates_mask lead to a warning", {
  expect_warning(SticsOnR::stics_wrapper(model_options = model_options, sit_var_dates_mask = obs_list), regexp = "These USMs will not be simulated")
})
obs_list <- SticsRFiles::get_obs(javastics_workspace_path)
obs_list[["bo96iN+"]] <- NULL
res <- SticsOnR::stics_wrapper(model_options = model_options, sit_var_dates_mask = obs_list)
test_that("Asking results for a list of variables / dates using sit_var_dates_mask works", {
  expect_equal(names(obs_list), names(res$sim_list))
  expect_equal(names(obs_list$`lu97iN+`), names(res$sim_list$`lu97iN+`))
  expect_equal(obs_list$`lu97iN+`$Date, res$sim_list$`lu97iN+`$Date)
})
var_list <- c("mafruit", "lai_n", "masec_n")
res <- SticsOnR::stics_wrapper(model_options = model_options, situation = "lu97iN+", var = var_list)
test_that("Asking results for a list of variables using var works", {
  expect_equal(sort(var_list), sort(setdiff(names(res$sim_list$`lu97iN+`), c("Plant", "Date")))) # tail is used to remove Date, ian, mo, jo, jul column in sim
})
var_list_parenthesis <- c("mafruit", "lai(n)", "masec(n)")
var_list <- c("mafruit", "lai_n", "masec_n")
res <- SticsOnR::stics_wrapper(model_options = model_options, situation = "lu97iN+", var = var_list_parenthesis)
test_that("Asking results for a list of variables with parenthesis in the name works", {
  expect_equal(sort(var_list), sort(setdiff(names(res$sim_list$`lu97iN+`), c("Plant", "Date")))) # tail is used to remove Date, ian, mo, jo, jul column in sim
})

# To uncomment when check of existence of stics variable will be implemented
# var_list <- c("mafruit", "lai", "toto")
# test_that("Asking results for a list of non-existing variables lead to a warning", {
#  expect_warning(SticsOnR::stics_wrapper(model_options = model_options, situation="lu97iN+", var = var_list), regexp="not simulated by the Stics model")
# })
dates <- obs_list$`lu97iN+`$Date
res <- SticsOnR::stics_wrapper(model_options = model_options, situation = "lu97iN+", dates = dates)
test_that("Asking results for a list of Dates using dates argument works", {
  expect_equal(obs_list$`lu97iN+`$Date, res$sim_list$`lu97iN+`$Date)
})
dates <- c("01/01/2034")
test_that("Asking results for a non-simulated date lead to a warning", {
  expect_warning(SticsOnR::stics_wrapper(model_options = model_options, situation = "lu97iN+", dates = dates), regexp = "Not any requested date")
})


# Test Design-Of-Experiment
# TO BE DOE LATER: not yet operational ... (we should add a column like DoE in results ...)
# situation_name="bo96iN+"
# var_name="mafruit"
# param_values <- tibble::tibble(dlaimax=runif(3,min=0.0005,max=50), durvieF=runif(3,min=0.0025,max=400))
# res <- SticsOnR::stics_wrapper(param_values = param_values, var = var_name, model_options = model_options)
# id_to_test=sample(1:nrow(param_values),2)
#
# test_that("Asking results for a DOE works", {
#   expect_gt(sum( abs(res$sim_list[[situation_name]][id_to_test[1],var_name]-
#                      res$sim_list[[situation_name]][id_to_test[2],var_name]),
#                  na.rm=TRUE),
#             0)
# })


# Test wrong model path lead to an error
test_that("Wrong model path lead to an error", {
  expect_error(SticsOnR::stics_wrapper_options(javastics = "", workspace = stics_inputs_path))
  expect_error(SticsOnR::stics_wrapper_options(javastics = paste0(stics_path, "ugly_suffix"), workspace = stics_inputs_path))
})

# Test unexisting data path lead to an error
test_that("Unexisting data path lead to an error", {
  expect_error(SticsOnR::stics_wrapper_options(javastics = stics_path, workspace = paste0(stics_inputs_path, "ugly_suffix")))
})


# Test rotations

javastics_workspace_path <- file.path(javastics_path, "example")

## Generate Stics input files from JavaStics input files
stics_inputs_path <- normalizePath(file.path(tempdir(), "RotationTests"), winslash = "/", mustWork = FALSE)
dir.create(stics_inputs_path, showWarnings = FALSE)

SticsRFiles::gen_usms_xml2txt(
  javastics = javastics_path, workspace = javastics_workspace_path,
  out_dir = stics_inputs_path,
  usm = c("demo_BareSoil2", "demo_Wheat1", "banana", "demo_maize3"), verbose = TRUE
)

model_options <- SticsOnR::stics_wrapper_options(javastics = javastics_path, workspace = stics_inputs_path, parallel = TRUE, cores = cores)
sim_without_successive <- SticsOnR::stics_wrapper(model_options = model_options)

model_options <- SticsOnR::stics_wrapper_options(javastics = javastics_path, workspace = stics_inputs_path, successive = list(c("demo_Wheat1", "demo_BareSoil2", "demo_maize3")), parallel = TRUE, cores = cores)
sim_with_successive <- SticsOnR::stics_wrapper(model_options = model_options)

model_options <- SticsOnR::stics_wrapper_options(javastics = javastics_path, workspace = stics_inputs_path, successive = list(c("demo_Wheat1", "demo_BareSoil2", "demo_maize3")), parallel = TRUE, cores = cores)
sim_with_successive_restricted_results <- SticsOnR::stics_wrapper(model_options = model_options, situation = c("banana", "demo_maize3"))

maize_succ_res <- file(file.path(stics_inputs_path, "demo_maize3", "mod_bdemo_maize3.sti"), "rb")
nb_grep_maize <- grep("rotation", readLines(maize_succ_res))
baresoil_succ_res <- file(file.path(stics_inputs_path, "demo_BareSoil2", "mod_bdemo_BareSoil2.sti"), "rb")
nb_grep_baresoil <- grep("rotation", readLines(baresoil_succ_res))

test_that("Test rotation", {
  expect_true(any(grepl("rotation", readLines(file(file.path(stics_inputs_path, "demo_BareSoil2", "mod_bdemo_BareSoil2.sti"), "rb")))))
  expect_true(any(grepl("rotation", readLines(file(file.path(stics_inputs_path, "demo_maize3", "mod_bdemo_maize3.sti"), "rb")))))
  expect_identical(sim_with_successive$sim_list$banana, sim_without_successive$sim_list$banana)
  expect_identical(sim_with_successive$sim_list$demo_Wheat1, sim_without_successive$sim_list$demo_Wheat1)
  expect_false(identical(sim_with_successive$sim_list$demo_BareSoil2, sim_without_successive$sim_list$demo_BareSoil2))
  expect_false(identical(sim_with_successive$sim_list$demo_maize3, sim_without_successive$sim_list$demo_maize3))
  expect_identical(names(sim_with_successive_restricted_results$sim_list), c("banana", "demo_maize3"))
  expect_identical(sim_with_successive_restricted_results$sim_list$demo_maize3, sim_with_successive$sim_list$demo_maize3)
  expect_true(length(nb_grep_maize) > 0)
  expect_true(length(nb_grep_baresoil) > 0)
})



# Test intercrops

javastics_workspace_path <- file.path(javastics_path, "example")

## Generate Stics input files from JavaStics input files
stics_inputs_path <- normalizePath(file.path(tempdir(), "IntercropTests"), winslash = "/", mustWork = FALSE)
dir.create(stics_inputs_path, showWarnings = FALSE)

SticsRFiles::gen_usms_xml2txt(
  javastics = javastics_path, workspace = javastics_workspace_path,
  out_dir = stics_inputs_path,
  usm = c("intercrop_pea_barley"), verbose = TRUE
)

# Set options for Stics wrapper
model_options <- SticsOnR::stics_wrapper_options(javastics = javastics_path, workspace = stics_inputs_path, parallel = FALSE)
sim_intercrop <- SticsOnR::stics_wrapper(model_options = model_options, situation = c("intercrop_pea_barley"))

test_that("Test rotation", {
  expect_true(length(unique(sim_intercrop$sim_list$intercrop_pea_barley$Plant)) > 1)
  expect_true(length(unique(sim_intercrop$sim_list$intercrop_pea_barley$Dominance)) > 1)
})
