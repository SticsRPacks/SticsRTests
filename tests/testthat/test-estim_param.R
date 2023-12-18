# Test filter obs (temporary test)
library(SticsOnR)
library(SticsRFiles)
library(CroptimizR)
library(dplyr)

stics_version <- "V10.0"
javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", stics_version = stics_version))

javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)
SticsRFiles::gen_usms_xml2txt(javastics = javastics_path, workspace = javastics_workspace_path,
                              out_dir = stics_inputs_path, verbose = TRUE)

sit_name="bo96iN+"  # can be a vector of situation names if you want to consider several, e.g. c("bo96iN+","bou00t1")
var_name="lai_n"    # can be a vector of variable names if you want to consider several, e.g. c("lai_n","masec_n")
obs_list= SticsRFiles::get_obs(javastics_workspace_path, usm = sit_name)

obs_list= CroptimizR::filter_obs(obs_list, var= var_name, include=TRUE)

test_that("Test filter_obs", {
  expect_equal(names(obs_list), "bo96iN+")
})



# Download and transform Vignette simple_case for different tests
# ---------------------------------------------------------------

context("Parameter estimation")

tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
simple_case_rmd <-file.path(tmpdir,"Parameter_estimation_simple_case.Rmd")
download.file("https://raw.github.com/SticsRPacks/CroptimizR/main/vignettes/Parameter_estimation_simple_case.Rmd",
              simple_case_rmd)

## set the parameters for a run in auto_test mode
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$path_to_JavaStics",
                replacement=paste0("\"",javastics_path,"\""),
                fixed=TRUE)

## generate the R script
simple_case_r <-file.path(tmpdir,"Parameter_estimation_simple_case.R")
knitr::purl(input=simple_case_rmd,
            output=simple_case_r, documentation = 2)



# Test Vignette simple_case
# -------------------------

## Copy the simple case R script for this test
simple_case_r_tmp <-file.path(tmpdir,"Parameter_estimation_simple_case_tmp.R")
file.copy(from=simple_case_r, to=simple_case_r_tmp, overwrite=TRUE)

## Define initial values as those used for computing the reference results
## (random sampling may lead to different values on different platforms even with the same seed)
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="ub = c(dlaimax = 0.0025, durvieF = 400)",
                replacement="ub=c(dlaimax=0.0025, durvieF=400),\n  init_values=data.frame(dlaimax=c(0.0023862966, 0.0008777006), durvieF=c(118.3769, 290.9086))",
                fixed=TRUE)

## change the options of the parameter estimation method
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="optim_options$nb_rep <- 7",replacement="optim_options$nb_rep <- 2",fixed=TRUE)
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="optim_options$maxeval <- 500",replacement="optim_options$maxeval <- 4",fixed=TRUE)

## adapt the version of the Stics input files to the Stics version used
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="stics_version = \"V9.0\"",
                replacement=paste0("stics_version = \"",stics_version,"\""),fixed=TRUE)


## run it
source(simple_case_r_tmp)

## load the results
load(file.path(optim_options$out_dir,"optim_results.Rdata"))
nlo_new<-lapply(res$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...
load(system.file(file.path("extdata","ResSimpleCase2rep4it"), "optim_results.Rdata", package = "CroptimizR"))
nlo<-lapply(nlo,function(x) {x$call<-NULL;x})

test_that("Test Vignette simple_case", {
  expect_equal(nlo_new[[1]]$x0, c(0.0023862966, 118.3769), tolerance = 1e-7)
  expect_equal(nlo_new[[2]]$x0, c(0.0008777006, 290.9086), tolerance = 1e-7)
  expect_equal(sapply(nlo_new, "[[","solution"), sapply(nlo, "[[","solution"), tolerance = 1e-4)
  expect_equal(sapply(nlo_new, "[[","objective"), sapply(nlo, "[[","objective"), tolerance = 1e-4)
  expect_true(file.exists(file.path(optim_options$out_dir,"EstimatedVSinit.pdf")))
  expect_true(file.exists(file.path(optim_options$out_dir,"ValuesVSit.pdf")))
  expect_true(file.exists(file.path(optim_options$out_dir,"ValuesVSit_2D.pdf")))
})



# Test optimization of an unexisting parameter
# --------------------------------------------
# TO DO



# Test Vignette specific and varietal
# -----------------------------------

tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
vignette_rmd <-file.path(tmpdir,"Parameter_estimation_Specific_and_Varietal.Rmd")
download.file("https://raw.github.com/SticsRPacks/CroptimizR/main/vignettes/Parameter_estimation_Specific_and_Varietal.Rmd",
              vignette_rmd)

## set the parameters for a run in auto_test mode
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$path_to_JavaStics",
                replacement=paste0("\"",javastics_path,"\""),
                fixed=TRUE)

## Define initial values as those used for computing the reference results
## (random sampling may lead to different values on different platforms even with the same seed)


xfun::gsub_file(file=vignette_rmd,
                pattern="ub = 0.0025",
                replacement="ub = 0.0025,\n    init_values = c(0.001386297, 0.001877701)",
                fixed=TRUE)


xfun::gsub_file(file=vignette_rmd,
                pattern="ub = c(400, 450)",
                replacement="ub = c(400,450),\n  init_values = data.frame(c(293.3769, 115.9086), c(299.3398,162.9456))",
                fixed=TRUE)

## change the options of the parameter estimation method
xfun::gsub_file(file=vignette_rmd,
                pattern="optim_options$nb_rep <- 7",replacement="optim_options$nb_rep <- 2",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="optim_options$maxeval <- 1000",replacement="optim_options$maxeval <- 4",fixed=TRUE)


## adapt the version of the Stics input files to the Stics version used
xfun::gsub_file(file=vignette_rmd,
                pattern="stics_version = \"V9.0\"",
                replacement=paste0("stics_version = \"",stics_version,"\""),fixed=TRUE)

## Fixing cores number for CI context on Github
if (Sys.getenv("CI") != "") {
  xfun::gsub_file(file = vignette_rmd,
                  "parallel = TRUE","parallel = TRUE, cores = 2",
                  fixed = TRUE)
}


## generate the R script
knitr::purl(input=vignette_rmd,
            output=file.path(tmpdir,"Parameter_estimation_Specific_and_Varietal.R"), documentation = 2)

## run it
source(file.path(tmpdir,"Parameter_estimation_Specific_and_Varietal.R"))

## load the results
load(file.path(optim_options$out_dir,"optim_results.Rdata"))
nlo_new<-lapply(res$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...
load(system.file(file.path("extdata","ResSpecVar_2rep4it",stics_version), "optim_results.Rdata", package = "CroptimizR"))
nlo<-lapply(res$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...

test_that("Test Vignette specific and varietal", {
  expect_equal(nlo_new[[1]]$x0, c(0.001386297, 293.3769, 299.3398), tolerance = 1e-7)
  expect_equal(nlo_new[[2]]$x0, c(0.001877701, 115.9086, 162.9456), tolerance = 1e-7)
  expect_equal(sapply(nlo_new, "[[","solution"), sapply(nlo, "[[","solution"), tolerance = 1e-4)
  expect_equal(sapply(nlo_new, "[[","objective"), sapply(nlo, "[[","objective"), tolerance = 1e-4)
  expect_true(file.exists(file.path(optim_options$out_dir,"EstimatedVSinit.pdf")))
  expect_true(file.exists(file.path(optim_options$out_dir,"ValuesVSit.pdf")))
  expect_true(file.exists(file.path(optim_options$out_dir,"ValuesVSit_2D.pdf")))
})




# # Test that model crash end with proper error message
# # ---------------------------------------------------
#
# Commented since actually estim_param do not through an error: this is due to the return()
# command in the on.exit function ... We have to find a way of catching that an error triggered
# the on.exit and do not execute the return in that case ... or replace the on.exit by a tryCatch.
#
# javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
# data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", stics_version = stics_version))
# javastics_workspace_path=file.path(data_dir,"XmlFiles")
# stics_inputs_path=file.path(data_dir,"TxtFiles")
# dir.create(stics_inputs_path, showWarnings = FALSE)
# SticsRFiles::gen_usms_xml2txt(javastics = javastics_path, workspace = javastics_workspace_path,
#                               out_dir = stics_inputs_path, verbose = TRUE)
#
# sit_name <- "bo96iN+"
# obs_list <- SticsRFiles::get_obs(javastics_workspace_path, usm_name = sit_name)
#
# ## Remove a file necessary to run Stics to make it crash
# file.remove(file.path(stics_inputs_path,sit_name,"tempopar.sti"))
#
# ## Run estim_param
# param_info=list(lb=c(dlaimax=0.0005),
#                 ub=c(dlaimax=0.0020), init_values=c(dlaimax=c(0.001, 0.0011, 0.0013)))
# optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, out_dir=data_dir, ranseed=1234)
# transform_sim <- function(model_results, ...) {
#   model_results$sim_list$`bo96iN+` <- dplyr::mutate(model_results$sim_list$`bo96iN+`, lai_n=lai_n*2)
#   return(model_results)
# }
# transform_obs <- function(obs_list, ...) {
#   obs_list$`bo96iN+` <- dplyr::mutate(obs_list$`bo96iN+`, lai_n=lai_n*2)
#   return(obs_list)
# }
#
# test_that("Test model crash end with proper error message", {
#   expect_error(estim_param(obs_list=obs_list,
#                            model_function=SticsOnR::stics_wrapper,
#                            model_options=model_options,
#                            optim_options=optim_options,
#                            param_info=param_info, info_level = 4,
#                            transform_sim=transform_sim, transform_obs=transform_obs),
#                "Error")
# })


# Test var argument and init values are taken into account
# --------------------------------------------------------------

# For that, create a synthetic observed variable laiX2=mai_n*2 which is not simulated,
# set var=lai_n to get the simulated lai, use transform_sim to dynamically compute laiX2
# and try to retrieve the parameter value used to create the synthetic observation.
# level_info is also tested here ...

javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", stics_version = stics_version))
javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)
SticsRFiles::gen_usms_xml2txt(javastics = javastics_path, workspace = javastics_workspace_path,
                              out_dir = stics_inputs_path, verbose = TRUE)

## Create synthetic observations
model_options <- SticsOnR::stics_wrapper_options(javastics=javastics_path, workspace = stics_inputs_path, parallel=FALSE)
tmp <- SticsOnR::stics_wrapper(model_options=model_options, param_values=c(dlaimax=0.0012), var="lai_n", situation="bo96iN+")
obs_synth <- tmp$sim_list
obs_synth$`bo96iN+` <- obs_synth$`bo96iN+` %>% dplyr::mutate(laiX2=lai_n*2) %>% dplyr::select(-lai_n) %>%
  dplyr::slice(seq(1,nrow(.),by=2))

transform_sim <- function(model_results, ...) {
  model_results$sim_list$`bo96iN+` <- dplyr::mutate(model_results$sim_list$`bo96iN+`, laiX2=lai_n*2)
  return(model_results)
}

## Try to retrieve dlaimax value with the standard method
param_info=list(lb=c(dlaimax=0.0005),
                ub=c(dlaimax=0.0020),
                init_values=data.frame(dlaimax=c(0.0005, 0.0017)))
optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, out_dir=data_dir, ranseed=1234)
optim_results=estim_param(obs_list=obs_synth,
                          crit_function = crit_ols,
                          model_function=SticsOnR::stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info, transform_sim = transform_sim,
                          var="lai_n",
                          info_level=4, info_crit_func = NULL)

test_that("Test var and transform_sim arguments with nloptr", {
  expect_equal(optim_results$final_values[["dlaimax"]],0.0012, tolerance = 1e-4)
})
test_that("Test init_values are taken into account in nloptr", {
  expect_equal(optim_results$init_values[1:2,],as.numeric(param_info$init_values[1:2,]))
})
test_that("level_info is working as expected", {
  expect_false(identical(optim_results$sim_intersect, optim_results$sim))
  expect_false(identical(optim_results$sim_transformed, optim_results$sim))
  expect_false(identical(optim_results$sim_transformed, optim_results$sim_intersect))
  expect_true(!is.null(optim_results$sim) && !is.null(optim_results$sim_intersect) &&
                !is.null(optim_results$sim_transformed) && !is.null(optim_results$obs_intersect))
  expect_equal(unique(optim_results$params_and_crit$rep), c(1,2,3))
  expect_true(length(optim_results$sim)==45 && length(optim_results$sim_intersect)==45 &&
                length(optim_results$sim_transformed)==45 && length(optim_results$obs_intersect)==45)
})


## Same but with optim package
optim_options=list(nb_rep=3, control=list(maxit=7), out_dir=data_dir, ranseed=1234)
optim_results=estim_param(obs_list=obs_synth,
                          crit_function = crit_ols,
                          optim_method="optim",
                          model_function=SticsOnR::stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info, transform_sim = transform_sim,
                          var="lai_n")

test_that("Test var and transform_sim arguments with optim", {
  expect_equal(optim_results$final_values[["dlaimax"]],0.0012, tolerance = 1e-4)
})
test_that("Test init_values are taken into account in optim", {
  expect_equal(optim_results$init_values[1:2,],as.numeric(param_info$init_values[1:2,]))
})


# Test force_param_values argument
# --------------------------------
# For that, create a synthetic observed variable lai_n created by forcing the values
# dlaimax and durvieF, and then try to estimate dlaimax by forcing durvieF to its true value
# (if the forcing is not performed correctly we should not be able to retrieve the correct
# value of dlaimax + estimated values of dlaimax from same starting points but with 2 different
# forced values of durvieF should give quite different results).

javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", stics_version = stics_version))
javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)
SticsRFiles::gen_usms_xml2txt(javastics = javastics_path, workspace = javastics_workspace_path,
                              out_dir = stics_inputs_path, verbose = TRUE)

## Create synthetic observations
model_options <- SticsOnR::stics_wrapper_options(javastics=javastics_path, workspace = stics_inputs_path, parallel=FALSE)
tmp <- SticsOnR::stics_wrapper(model_options=model_options, param_values=c(dlaimax=0.0012, durvieF=100),
                               var=c("lai_n","masec_n"), situation="bo96iN+")
obs_synth <- tmp$sim_list

## Try to retrieve dlaimax value
param_info=list(lb=c(dlaimax=0.0005),
                ub=c(dlaimax=0.0020), init_values=c(dlaimax=c(0.001, 0.0011, 0.0013)))
optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, out_dir=data_dir, ranseed=1234)
optim_results1=estim_param(obs_list=obs_synth,
                           crit_function = crit_ols,
                           model_function=SticsOnR::stics_wrapper,
                           model_options=model_options,
                           optim_options=optim_options,
                           param_info=param_info, forced_param_values = c(durvieF=100))
optim_results2=estim_param(obs_list=obs_synth,
                           crit_function = crit_ols,
                           model_function=SticsOnR::stics_wrapper,
                           model_options=model_options,
                           optim_options=optim_options,
                           param_info=param_info, forced_param_values = c(durvieF=300))

test_that("Test forced_param_values argument", {
  expect_equal(optim_results1$final_values[["dlaimax"]],0.0012, tolerance = 1e-5)
  expect_gt(optim_results1$final_values[["dlaimax"]]-optim_results2$final_values[["dlaimax"]],
            1e-4)
})


# Test DREAM-ZS takes into account initial values
# -----------------------------------------------

model_options <- SticsOnR::stics_wrapper_options(javastics=javastics_path, workspace = stics_inputs_path, parallel=FALSE)
tmp <- SticsOnR::stics_wrapper(model_options=model_options, param_values=c(dlaimax=0.0012), var="lai_n", situation="bo96iN+")
obs_synth <- tmp$sim_list

param_info=list(lb=c(dlaimax=0.0005),
                ub=c(dlaimax=0.0020), init_values=data.frame(dlaimax=c(0.001, 0.0011, 0.0013)))

optim_options=list()
optim_options$iterations <- 10
optim_options$startValue <- 3 # Number of markov chains
optim_options$out_dir <- data_dir # path where to store the results (graph and Rdata)
optim_options$ranseed <- 1234 # seed for random numbers

optim_results=estim_param(obs_list=obs_synth,
                          crit_function=likelihood_log_ciidn,
                          model_function=SticsOnR::stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info,
                          optim_method="BayesianTools.dreamzs")

test_that("Test DREAM-ZS takes into account initial values", {
  expect_equal(optim_results$post_sample[1:2],as.numeric(param_info$init_values[1:2,]))
})



# Test rotations
# --------------
# Here we use observations for a given USM (demo_maize3) and try to estimate parameters for another non-observed USM (demo_Wheat1)
# which in run before the observed one in a rotation

javastics_workspace_path=file.path(javastics_path,"example")

## Generate Stics input files from JavaStics input files
stics_inputs_path=file.path(tempdir(),"RotationTests")
dir.create(stics_inputs_path, showWarnings = FALSE)

SticsRFiles::gen_usms_xml2txt(javastics = javastics_path, workspace = javastics_workspace_path,
                              out_dir = stics_inputs_path, usm = c("demo_BareSoil2","demo_Wheat1","demo_maize3"), verbose = TRUE)

model_options= SticsOnR::stics_wrapper_options(javastics=javastics_path, workspace = stics_inputs_path, successive = list(c("demo_Wheat1","demo_BareSoil2","demo_maize3")), parallel=TRUE)
sim_with_successive=SticsOnR::stics_wrapper(model_options=model_options, situation=c("demo_Wheat1","demo_BareSoil2","demo_maize3"), var=c("AZnit_1"),
                                            param_values=data.frame(situation=c("demo_Wheat1"), durvieF=350))

## Create synthetic observations
obs_synth <- sim_with_successive$sim_list["demo_maize3"]

## Try to retrieve dlaimax value with the standard method
param_info=list()
param_info$durvieF=list(lb=50,ub=500,sit_list=list("demo_Wheat1"))
optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, out_dir=stics_inputs_path, ranseed=1234)
optim_results=estim_param(obs_list=obs_synth,
                          crit_function = crit_ols,
                          model_function=SticsOnR::stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info)

test_that("Test rotation", {
  expect_equal(optim_results$final_values[["durvieF"]],350, tolerance = 1)
})



# # Test Vignette DREAM
# # -------------------
# TO update when the way of defining initial values will be uniformized in CroptimizR
#
#
# tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
# vignette_rmd <-file.path(tmpdir,"Parameter_estimation_DREAM.Rmd")
# download.file("https://raw.github.com/SticsRPacks/CroptimizR/main/vignettes/Parameter_estimation_DREAM.Rmd",
#               vignette_rmd)
#
# ## set the parameters for a run in auto_test mode
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
# javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$path_to_JavaStics",
#                 replacement=paste0("\"",javastics_path,"\""),
#                 fixed=TRUE)
#
# ## Change the name of the executable (will be included i nthe vignette when SticsRFiles functions will be stabilized)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="stics_path=file.path(javastics_path,\"bin/stics_modulo.exe\")",
#                 replacement="SticsOnR::init_javastics_pref(javastics_path,overwrite = TRUE); stics_path=file.path(javastics_path,\"bin\",SticsOnR::list_stics_exe(javastics_path)$current[[1]])",
#                 fixed=TRUE)
#
# ## change the options of the parameter estimation method
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="optim_options$iterations <- 10000 # Total number of iterations",
#                 replacement="optim_options$iterations <- 4 # Total number of iterations",fixed=TRUE)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="optim_options$iterations <- 1000 # Total number of new iterations",
#                 replacement="optim_options$iterations <- 4 # Total number of new iterations",fixed=TRUE)
#
# ## generate the R script
# knitr::purl(input=vignette_rmd,
#             output=file.path(tmpdir,"Parameter_estimation_DREAM.R"), documentation = 2)
#
# ## run it
# source(file.path(tmpdir,"Parameter_estimation_DREAM.R"))
#
# ## load the results
# load(file.path(optim_options$out_dir,"optim_results.Rdata"))
# res$out=NULL
# res_new=res
# load(system.file(file.path("extdata","ResSpecVarDREAM_4it"), "optim_results.Rdata", package = "CroptimizR"))
# res$out=NULL
#
# test_that("Test Vignette DREAM", {
#   expect_equal(res_new,res)
#   expect_true(file.exists(file.path(optim_options$out_dir,"correlationPlots.pdf")))
#   expect_true(file.exists(file.path(optim_options$out_dir,"iterAndDensityPlots.pdf")))
#   expect_true(file.exists(file.path(optim_options$out_dir,"marginalPlots.pdf")))
# })



# Test Vignette AgMIP phase III protocol
# --------------------------------------

tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
vignette_rmd <-file.path(tmpdir,"AgMIP_Calibration_Phenology_protocol.Rmd")
download.file("https://raw.github.com/SticsRPacks/CroptimizR/main/vignettes/AgMIP_Calibration_Phenology_protocol.Rmd",
              vignette_rmd)

## set the parameters for a run in auto_test mode
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$path_to_JavaStics",
                replacement=paste0("\"",javastics_path,"\""),
                fixed=TRUE)

## Define initial values as those used for computing the reference results
## (random sampling may lead to different values on different platforms even with the same seed)
xfun::gsub_file(file=vignette_rmd,
                pattern="ub = c(stlevamf = 500, stamflax = 800, tdmin = 8, stressdev = 1, tdmax = 32)",
                replacement="ub = c(stlevamf = 500, stamflax = 800, tdmin = 8, stressdev = 1, tdmax = 32), init_values=data.frame(stlevamf = c(138.2656, 446.0060), stamflax = c(506.3340, 639.4217), tdmin = c(4.841205, 6.351278), stressdev = c(0.003672681, 0.547923659), tdmax = c(30.67603, 27.26267))",
                fixed=TRUE)

## change the options of the parameter estimation method
xfun::gsub_file(file=vignette_rmd,
                pattern="list(nb_rep = c(10, 5))",
                replacement="list(nb_rep = 2, maxeval=4)",fixed=TRUE)

## adapt the version of the Stics input files to the Stics version used
xfun::gsub_file(file=vignette_rmd,
                pattern="stics_version = \"V9.0\"",
                replacement=paste0("stics_version = \"",stics_version,"\""),fixed=TRUE)

## Fixing cores number for CI context on Github
if (Sys.getenv("CI") != "") {
  xfun::gsub_file(file = vignette_rmd,
                  "parallel = TRUE, cores = 4","parallel = TRUE, cores = 2",
                  fixed = TRUE)
}

## generate the R script
knitr::purl(input=vignette_rmd,
            output=file.path(tmpdir,"AgMIP_Calibration_Phenology_protocol.R"), documentation = 2)

## Seems that optim_options and optim_results.Rdata are not overwritten => try to remove them before run
file.remove(file.path(optim_options$out_dir,"optim_results.Rdata"))
rm(optim_options, param_info)

## run it
source(file.path(tmpdir,"AgMIP_Calibration_Phenology_protocol.R"))

## load the results
load(file.path(optim_options$out_dir,"optim_results.Rdata"))
nlo_new<-lapply(res$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...
load(system.file(file.path("extdata","ResAgmipPheno_2rep4it",stics_version), "optim_results.Rdata", package = "CroptimizR"))
nlo<-lapply(res$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...

test_that("Test Vignette AgMIP Phase III protocol", {
  expect_equal(optim_options$nb_rep,2)
  expect_equal(optim_options$maxeval,4)
  expect_equal(param_info$init_values,
               data.frame(stlevamf = c(138.2656, 446.0060),
                          stamflax = c(506.3340, 639.4217),
                          tdmin = c(4.841205, 6.351278),
                          stressdev = c(0.003672681, 0.547923659),
                          tdmax = c(30.67603, 27.26267)))
  expect_equal(nlo_new[[1]]$x0, c(405.5104, 764.4217, 4.841205), tolerance = 1e-4)
  expect_equal(nlo_new[[2]]$x0, c(405.5104, 764.4217, 6.351278), tolerance = 1e-4)
  expect_equal(sapply(nlo_new, "[[","solution"), sapply(nlo, "[[","solution"), tolerance = 1e-4)
  expect_equal(sapply(nlo_new, "[[","objective"), sapply(nlo, "[[","objective"), tolerance = 1e-4)
  expect_true(file.exists(file.path(optim_options$out_dir,"param_selection_steps.csv")))
  for (i in 1:4) {
    expect_true(file.exists(file.path(optim_options$out_dir,"results_all_steps",paste0("step_",i), "optim_results.Rdata")))
    expect_true(file.exists(file.path(optim_options$out_dir,"results_all_steps",paste0("step_",i), "EstimatedVSinit.pdf")))
    expect_true(file.exists(file.path(optim_options$out_dir,"results_all_steps",paste0("step_",i), "ValuesVSit.pdf")))
    expect_true(file.exists(file.path(optim_options$out_dir,"results_all_steps",paste0("step_",i), "ValuesVSit_2D.pdf")))
  }
})


# Test use of wls criterion
# -------------------------

javastics_path=file.path(system.file("stics", package = "SticsRTests"),stics_version)
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", stics_version = stics_version))
javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path, showWarnings = FALSE)
SticsRFiles::gen_usms_xml2txt(javastics = javastics_path, workspace = javastics_workspace_path,
                              out_dir = stics_inputs_path, verbose = TRUE)

## Create synthetic observations
model_options <- SticsOnR::stics_wrapper_options(javastics=javastics_path, workspace = stics_inputs_path, parallel=FALSE)
tmp <- SticsOnR::stics_wrapper(model_options=model_options, param_values=c(dlaimax=0.0012),
                               var=c("lai_n"), situation="bo96iN+")
obs_synth <- tmp$sim_list
param_info=list(lb=c(dlaimax=0.0005),
                ub=c(dlaimax=0.0020), init_values=c(dlaimax=c(0.001, 0.0011, 0.0013)))
optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, out_dir=data_dir, ranseed=1234)

## Try to retrieve dlaimax value using OLS
optim_results_ols=estim_param(obs_list=obs_synth,
                           crit_function = crit_ols,
                           model_function=SticsOnR::stics_wrapper,
                           model_options=model_options,
                           optim_options=optim_options,
                           param_info=param_info)
## Same but with wls, using a weight=1
weight <- function(...) {
  return(1)
}
optim_results_wls=estim_param(obs_list=obs_synth,
                           crit_function = crit_wls,
                           model_function=SticsOnR::stics_wrapper,
                           model_options=model_options,
                           optim_options=optim_options,
                           param_info=param_info,
                           weight = weight)
test_that("Test use of WLS, weight equal to 1", {
  expect_equal(optim_results_ols$final_values[["dlaimax"]],
               optim_results_wls$final_values[["dlaimax"]], tolerance = 1e-5)
})
## Now bias large values of LAI and take them into account or not using the weight
## check that estimated value of dlaimax is correct when weight of large values is set to Inf
obs_synth$`bo96iN+`$lai_n[obs_synth$`bo96iN+`$lai_n>5] <-
  obs_synth$`bo96iN+`$lai_n[obs_synth$`bo96iN+`$lai_n>5] + 3
optim_results_wls1=estim_param(obs_list=obs_synth,
                              crit_function = crit_wls,
                              model_function=SticsOnR::stics_wrapper,
                              model_options=model_options,
                              optim_options=optim_options,
                              param_info=param_info,
                              weight = weight)
weight <- function(obs,...) {
  w <- rep(1,length(obs))
  w[obs>8] <- Inf
  return(w)
}
optim_results_wls2=estim_param(obs_list=obs_synth,
                               crit_function = crit_wls,
                               model_function=SticsOnR::stics_wrapper,
                               model_options=model_options,
                               optim_options=optim_options,
                               param_info=param_info,
                               weight = weight)
test_that("Test use of WLS, weight equal to Inf", {
  expect_equal(optim_results_ols$final_values[["dlaimax"]],
               optim_results_wls2$final_values[["dlaimax"]], tolerance = 1e-5)
  expect_gt(optim_results_wls1$final_values[["dlaimax"]]-optim_results_wls2$final_values[["dlaimax"]],
            1e-4)
})
## test incorrect format for weight function
w0 <- 1
test_that("Test use of wls: error is caught for incorrect format of weight", {
  expect_error(estim_param(obs_list=obs_synth,
                           crit_function = crit_wls,
                           model_function=SticsOnR::stics_wrapper,
                           model_options=model_options,
                           optim_options=optim_options,
                           param_info=param_info,
                           weight = w0))
})
