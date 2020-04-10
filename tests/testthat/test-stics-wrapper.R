# Test for checking java version
print("java information")
system("which java")
system("java -version")
# quit(status = 0)

# Define path to JavaStics and stics exe
path_to_JavaStics=system.file("stics", package = "SticsRTests")
javastics_path=file.path(path_to_JavaStics,"V90")
SticsOnR::init_javastics_pref(javastics_path,overwrite = TRUE)
stics_path=file.path(javastics_path,"bin",SticsOnR::list_stics_exe(javastics_path)$current[[1]])
if ( SticsOnR:::is_unix() ) {
  system2(command = "chmod",args = c("+x", stics_path))
}

# Download and prepare data
data_dir= file.path(SticsRFiles::download_data(),"study_case_1","V9.0")
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = file.path(data_dir,"XmlFiles"),
                              target_path = stics_inputs_path, display = TRUE)

# Set options for Stics wrapper
model_options=SticsOnR::stics_wrapper_options(stics_path,stics_inputs_path,
                                              parallel=FALSE)


# Test parameter forcing works
param_names=c("dlaimax","durvieF")
param_lb=c(0.0005,50)
param_ub=c(0.0025,400)
var_name="lai_n"
situation_name="bo96iN+"
param_values_min <- array( param_lb,
                           dim=c(1,length(param_lb),1),
                           dimnames=list(NULL,param_names,situation_name))
param_values_max <- array( param_ub,
                           dim=c(1,length(param_ub),1),
                           dimnames=list(NULL,param_names,situation_name))
sim_max       <- SticsOnR::stics_wrapper(param_values = param_values_min, model_options = model_options)
sim_min       <- SticsOnR::stics_wrapper(param_values = param_values_max, model_options = model_options)

test_that("Parameter forcing works", {
  expect_gt(sum( abs(sim_max$sim_list[[1]][[situation_name]][,var_name]-
                       sim_min$sim_list[[1]][[situation_name]][,var_name]),
                 na.rm=TRUE),
            0)
})


# Test effect of sit_var_dates_mask
situation_names=c("bo96iN+","lu97iN+")
res <- SticsOnR::stics_wrapper(model_options = model_options, sit_var_dates_mask = situation_names)

test_that("Asking results for a list of situation works", {
  expect_equal(situation_names,names(res$sim_list[[1]]))
})


# Test Design-Of-Experiment
situation_name="bo96iN+"
var_name="mafruit"
param_names=c("dlaimax","durvieF")
param_lb=c(0.0005,50)
param_ub=c(0.0025,400)
param_values <- array( sapply(seq_along(param_names),function(i) runif(3,min=param_lb[i],max=param_ub[i])),
                       dim=c(3,length(param_lb),1),
                       dimnames=list(NULL,param_names,situation_name))
res <- SticsOnR::stics_wrapper(param_values = param_values, model_options = model_options)
id_to_test=sample(1:dim(param_values)[1],2)

test_that("Asking results for a DOE works", {
  expect_equal(length(res$sim_list),dim(param_values)[1])
  expect_gt(sum( abs(res$sim_list[[id_to_test[1]]][[situation_name]][,var_name]-
                     res$sim_list[[id_to_test[2]]][[situation_name]][,var_name]),
                 na.rm=TRUE),
            0)
})


# Test wrong model path lead to an error
test_that("Wrong model path lead to an error", {
  expect_error(stics_wrapper_options("",stics_inputs_path))
  expect_error(stics_wrapper_options(paste0(stics_path,"ugly_suffix"),stics_inputs_path))
})

# Test unexisting data path lead to an error
test_that("Unexisting data path lead to an error", {
  expect_error(stics_wrapper_options(stics_path,paste0(stics_inputs_path,"ugly_suffix")))
})
