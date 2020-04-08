# Define path to JavaStics and download data
path_to_JavaStics=system.file("stics", package = "SticsRTests")
javastics_path=file.path(path_to_JavaStics,"V90")
data_dir= file.path(SticsRFiles::download_data(),"study_case_1","V9.0")


# Test parameter forcing works
param_names=c("dlaimax","durvieF")
param_lb=c(0.0005,50)
param_ub=c(0.0025,400)
var_name="lai_n"
situation_name="bo96iN+"
SticsOnR::init_javastics_pref(javastics_path,overwrite = TRUE)
stics_path=file.path(javastics_path,"bin",SticsOnR::list_stics_exe(javastics_path)$current[[1]])

if ( SticsOnR:::is_unix() ) {
  system2(paste0("chmod +x ",stics_path))
}
stics_inputs_path=file.path(data_dir,"TxtFiles")
dir.create(stics_inputs_path)
SticsRFiles::gen_usms_xml2txt(javastics_path = javastics_path, workspace_path = file.path(data_dir,"XmlFiles"),
                 target_path = stics_inputs_path, display = TRUE)
model_options=SticsOnR::stics_wrapper_options(stics_path,stics_inputs_path,
                                    parallel=FALSE)
wrapper=SticsOnR::stics_wrapper
param_values_min <- array( param_lb,
                           dim=c(1,length(param_lb),1),
                           dimnames=list(NULL,param_names,situation_name))
param_values_max <- array( param_ub,
                           dim=c(1,length(param_ub),1),
                           dimnames=list(NULL,param_names,situation_name))
sim_max       <- wrapper(param_values = param_values_min, model_options = model_options)
sim_min       <- wrapper(param_values = param_values_max, model_options = model_options)

test_that("Parameter forcing works", {
  expect_gt(sum( abs(sim_max$sim_list[[1]][[situation_name]][,var_name]-
                       sim_min$sim_list[[1]][[situation_name]][,var_name]),
                 na.rm=TRUE),
            0)
})
