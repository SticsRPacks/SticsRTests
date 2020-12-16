# Test filter obs (temporary test)
library(SticsOnR)
library(SticsRFiles)
library(CroptimizR)
library(dplyr)

javastics_path=file.path(system.file("stics", package = "SticsRTests"),"V90")
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", version_name = "V9.0"))

javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
                 target_path = stics_inputs_path, verbose = TRUE)

sit_name="bo96iN+"  # can be a vector of situation names if you want to consider several, e.g. c("bo96iN+","bou00t1")
var_name="lai_n"    # can be a vector of variable names if you want to consider several, e.g. c("lai_n","masec_n")
obs_list= SticsRFiles::get_obs(javastics_workspace_path, usm_name = sit_name)

obs_list= CroptimizR::filter_obs(obs_list, var_names= var_name, include=TRUE)

test_that("Test filter_obs", {
  expect_equal(names(obs_list), "bo96iN+")
})



# Download and transform Vignette simple_case for different tests
# ---------------------------------------------------------------

context("Parameter estimation")

tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
simple_case_rmd <-file.path(tmpdir,"Parameter_estimation_simple_case.Rmd")
download.file("https://raw.github.com/SticsRPacks/CroptimizR/master/vignettes/Parameter_estimation_simple_case.Rmd",
              simple_case_rmd)

## set the parameters for a run in auto_test mode
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
xfun::gsub_file(file=simple_case_rmd,
                pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
javastics_path=file.path(system.file("stics", package = "SticsRTests"),"V90")
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
                pattern="ub=c(dlaimax=0.0025, durvieF=400))",
                replacement="ub=c(dlaimax=0.0025, durvieF=400),init_values=data.frame(dlaimax=c(0.0023862966, 0.0008777006),
                                      durvieF=c(118.3769, 290.9086)))",
                fixed=TRUE)

## change the options of the parameter estimation method
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="optim_options$nb_rep <- 7",replacement="optim_options$nb_rep <- 2",fixed=TRUE)
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="optim_options$maxeval <- 500",replacement="optim_options$maxeval <- 4",fixed=TRUE)

## run it
source(simple_case_r_tmp)

## load the results
load(file.path(optim_options$path_results,"optim_results.Rdata"))
nlo_new<-lapply(optim_results$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...
load(system.file(file.path("extdata","ResultsSimpleCase2repet4iter"), "optim_results.Rdata", package = "CroptimizR"))
nlo<-lapply(nlo,function(x) {x$call<-NULL;x})

test_that("Test Vignette simple_case", {
  expect_equal(nlo_new,nlo, tolerance = 1e-5)
  expect_true(file.exists(file.path(optim_options$path_results,"EstimatedVSinit.pdf")))
})


# Test model crash
# ----------------

## Copy the simple case R script for this test
simple_case_r_tmp <-file.path(tmpdir,"Parameter_estimation_simple_case_tmp.R")
file.copy(from=simple_case_r, to=simple_case_r_tmp, overwrite=TRUE)

# remove a file necessary for Stics
xfun::gsub_file(file=simple_case_r_tmp,
                pattern="optim_results=estim_param(obs_list=obs_list,",
                replacement="file.remove(file.path(stics_inputs_path,sit_name,\"tempopar.sti\"))
optim_results=estim_param(obs_list=obs_list,",
                fixed=TRUE)

test_that("Test Vignette model crash", {
  expect_error(source(simple_case_r_tmp),"error")
})


# Test optimization of an unexisting parameter
# TO DO



# Test Vignette specific and varietal

tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
vignette_rmd <-file.path(tmpdir,"Parameter_estimation_Specific_and_Varietal.Rmd")
download.file("https://raw.github.com/SticsRPacks/CroptimizR/master/vignettes/Parameter_estimation_Specific_and_Varietal.Rmd",
              vignette_rmd)

## set the parameters for a run in auto_test mode
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
javastics_path=file.path(system.file("stics", package = "SticsRTests"),"V90")
xfun::gsub_file(file=vignette_rmd,
                pattern="params$path_to_JavaStics",
                replacement=paste0("\"",javastics_path,"\""),
                fixed=TRUE)

## Define initial values as those used for computing the reference results
## (random sampling may lead to different values on different platforms even with the same seed)
xfun::gsub_file(file=vignette_rmd,
                pattern="lb=0.0005,ub=0.0025)",
                replacement="lb=0.0005,ub=0.0025, init_values=c(0.001386297, 0.001877701))",
                fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="lb=c(50,100),ub=c(400,450))",
                replacement="lb=c(50,100),ub=c(400,450), init_values=data.frame(c(293.3769, 115.9086), c(299.3398,162.9456)))",
                fixed=TRUE)

## change the options of the parameter estimation method
xfun::gsub_file(file=vignette_rmd,
                pattern="optim_options$nb_rep <- 7",replacement="optim_options$nb_rep <- 2",fixed=TRUE)
xfun::gsub_file(file=vignette_rmd,
                pattern="optim_options$maxeval <- 1000",replacement="optim_options$maxeval <- 4",fixed=TRUE)

## generate the R script
knitr::purl(input=vignette_rmd,
            output=file.path(tmpdir,"Parameter_estimation_Specific_and_Varietal.R"), documentation = 2)

## run it
source(file.path(tmpdir,"Parameter_estimation_Specific_and_Varietal.R"))

## load the results
load(file.path(optim_options$path_results,"optim_results.Rdata"))
nlo_new<-lapply(optim_results$nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...
load(system.file(file.path("extdata","ResultsSpecificVarietal_2repet4iter"), "optim_results.Rdata", package = "CroptimizR"))
nlo<-lapply(nlo,function(x) {x$call<-NULL;x}) # remove "call" since it may change between code versions ...

test_that("Test Vignette specific and varietal", {
  expect_equal(nlo_new,nlo, tolerance = 1e-4)
  expect_true(file.exists(file.path(optim_options$path_results,"EstimatedVSinit.pdf")))
})


# Test var_names argument
# For that, create a synthetic observed variable laiX2=mai_n*2 which is not simulated,
# set var_names=lai_n to get the simulated lai, use transform_sim to dynamically compute laiX2
# and try to retrieve the parameter value used to create the synthetic observation.

javastics_path=file.path(system.file("stics", package = "SticsRTests"),"V90")
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", version_name = "V9.0"))
javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
                              target_path = stics_inputs_path, verbose = TRUE)

## Create synthetic observations
model_options <- stics_wrapper_options(javastics_path, data_dir = stics_inputs_path, parallel=FALSE)
tmp <- stics_wrapper(model_options=model_options, param_values=c(dlaimax=0.0012), var_names="lai_n", sit_names="bo96iN+")
obs_synth <- tmp$sim_list
obs_synth$`bo96iN+` <- obs_synth$`bo96iN+` %>% dplyr::mutate(laiX2=lai_n*2) %>% dplyr::select(-lai_n)

transform_sim <- function(model_results, ...) {
  model_results$sim_list$`bo96iN+` <- dplyr::mutate(model_results$sim_list$`bo96iN+`, laiX2=lai_n*2)
  return(model_results)
}

# Try to retrieve dlaimax value
param_info=list(lb=c(dlaimax=0.0005),
                ub=c(dlaimax=0.0020))
optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, path_results=data_dir, ranseed=1234)
optim_results=estim_param(obs_list=obs_synth,
                          crit_function = crit_ols,
                          model_function=stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info, transform_sim = transform_sim,
                          var_names="lai_n")

test_that("Test var_names, transform_sim and transform_obs arguments", {
  expect_equal(optim_results$final_values[["dlaimax"]],0.0012, tolerance = 1e-4)
})



# Test force_param_values argument
# For that, create a synthetic observed variable lai_n created by forcing the values
# dlaimax and durvieF, and then try to estimate dlaimax by forcing durvieF to its true value
# (if the forcing is not performed correctly we should not be able to retrieve the correct
# value of dlaimax + estimated values of dlaimax from same starting points but with 2 different
# forced values of durvieF should give quite different results).

javastics_path=file.path(system.file("stics", package = "SticsRTests"),"V90")
data_dir= file.path(SticsRFiles::download_data(example_dirs="study_case_1", version_name = "V9.0"))
javastics_workspace_path=file.path(data_dir,"XmlFiles")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = javastics_workspace_path,
                              target_path = stics_inputs_path, verbose = TRUE)

## Create synthetic observations
model_options <- stics_wrapper_options(javastics_path, data_dir = stics_inputs_path, parallel=FALSE)
tmp <- stics_wrapper(model_options=model_options, param_values=c(dlaimax=0.0012, durvieF=100),
                     var_names="lai_n", sit_names="bo96iN+")
obs_synth <- tmp$sim_list

# Try to retrieve dlaimax value
param_info=list(lb=c(dlaimax=0.0005),
                ub=c(dlaimax=0.0020), init_values=c(dlaimax=c(0.001, 0.0011, 0.0013)))
optim_options=list(nb_rep=3, maxeval=15, xtol_rel=1e-01, path_results=data_dir, ranseed=1234)
optim_results1=estim_param(obs_list=obs_synth,
                          crit_function = crit_ols,
                          model_function=stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info, forced_param_values = c(durvieF=100))
optim_results2=estim_param(obs_list=obs_synth,
                          crit_function = crit_ols,
                          model_function=stics_wrapper,
                          model_options=model_options,
                          optim_options=optim_options,
                          param_info=param_info, forced_param_values = c(durvieF=300))

test_that("Test forced_param_values argument", {
  expect_equal(optim_results1$final_values[["dlaimax"]],0.0012, tolerance = 1e-5)
  expect_gt(optim_results1$final_values[["dlaimax"]]-optim_results2$final_values[["dlaimax"]],
                      2e-4)
})



# # Test Vignette DREAM
#
# TO update when the way of defining initial values will be uniformized in CroptimizR
#
#
# tmpdir <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
# vignette_rmd <-file.path(tmpdir,"Parameter_estimation_DREAM.Rmd")
# download.file("https://raw.github.com/SticsRPacks/CroptimizR/master/vignettes/Parameter_estimation_DREAM.Rmd",
#               vignette_rmd)
#
# ## set the parameters for a run in auto_test mode
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$eval_auto_test",replacement="TRUE",fixed=TRUE)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$eval_auto_vignette",replacement="FALSE",fixed=TRUE)
# xfun::gsub_file(file=vignette_rmd,
#                 pattern="params$eval_manual_vignette",replacement="FALSE",fixed=TRUE)
# javastics_path=file.path(system.file("stics", package = "SticsRTests"),"V90")
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
# load(file.path(optim_options$path_results,"optim_results.Rdata"))
# res$out=NULL
# res_new=res
# load(system.file(file.path("extdata","ResultsSpecificVarietalDREAM_4iter"), "optim_results.Rdata", package = "CroptimizR"))
# res$out=NULL
#
# test_that("Test Vignette DREAM", {
#   expect_equal(res_new,res)
#   expect_true(file.exists(file.path(optim_options$path_results,"correlationPlots.pdf")))
#   expect_true(file.exists(file.path(optim_options$path_results,"iterAndDensityPlots.pdf")))
#   expect_true(file.exists(file.path(optim_options$path_results,"marginalPlots.pdf")))
# })


