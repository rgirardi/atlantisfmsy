#' Run set of fishing strategies scenarios.
#'
#' @description Run a predefine set of fishing scenarios on a several functional
#' groups using Fmsy multipliers or F per functional group. Unselected functional
#' groups remains at the same fishing level as in the initial model. F distribution
#' across fleet isn't changed.
#' @section Warning:
#'   At the moment, the package is not designed to run on OSX, it only
#'   works on Linux and Windows (To run on OSX these functions need to be
#'   modified: \code{\link{atlantisfmsy_simu}},
#'   \code{\link{atlantisfmsy_restart}}, \code{\link{atlantisfmsy_modelcopy}},
#'   \code{\link{atlantis_bachchange}}, and \code{\link{atlantis_paraselect}}).
#'   As well if you are not providing the name of your bach/shell file, this
#'   package required that only one batch/shell file is in the model directory
#'   (if more than one batch/shell files are present please remove them, leave
#'   only the one used to run the calibrated model).
#' @param func_grp The code of Atlantis functional groups for which F scenarios will
#'   be run.
#' @param folder_path The working directory: where the F simulations will be
#'   run and stored.
#' @param model_path The directory of the calibrated model (containing all the
#'   parameters files and one bach file. Forcing files can be stored in a direct
#'   parent directory of model_path). \strong{WARNING:} Only working if the forcing
#'   folder is in the main model directory \code{model_path} or if it is in the
#'   direct parent directory. If not please either modify this package or modify
#'   the path structure of your Atlantis input forcing parameters file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew, atlantisMerged).
#' @param burnin_time The time in days needed for the model to pseudo-stabilize
#'   before starting the additional 30 years of simulation.
#' @param label Name of the scenarios (used to set names of output folder and files).
#' @param batch_file The name of the batch/shell file with extension you are using
#'   to run your model. If not provided, the function will search for the unique
#'   batch file in your \code{folder_path}. \strong{Default:} NULL.
#' @param fmult Set of F multipliers, one value per functional groups in \code{func_grp}.
#' If wants to be used as set of fishing mortalities (in y-1), don't supply values for
#' \code{fref}.
#' @param fref Set of reference F (in y-1), one value per functional groups in
#' \code{func_grp}. The default value is set to 1.
#' @return Atlantis outputs for every scenarios defined previously.
#'function(func_grp, folder_path, model_path, exe_name, burnin_time, label, batch_file = NULL, fmult, fref = rep(1,length(fmult)))
#' @examples
#' atlantisf_simu(c("COD","LBT"), "C:/Atlantis/AtlantisEEC/Fmsy",
#'                   "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                   "atlantismain", 7300, "HTL", "runAtlantis.bat")
#' atlantisf_simu(c("COD","LBT"), "/home/Atlantis/AtlantisEEC/Fmsy",
#'                   "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                   "atlantisNew", 7300, "HTL", "runAtlantis.sh")
#'
#' @seealso \code{\link{atlantis_checkmodule}} to check if all the modules are
#'   on (physics, biology, and fishery), \code{\link{atlantis_fgrpon}} to check
#'   if the functional group is active, \code{\link{atlantis_fgrpimp}} to check
#'   if the functional group is fished and impacted,
#'   \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_fleetopen}} to check if fleets are active,
#'   \code{\link{atlantis_checkf}} to check the formulation of the fishing
#'   pressure used (i.e. F, effort, dynamics model...),
#'   \code{\link{atlantis_fdistri}} to extract the distribution of fishing
#'   pressure amongst fleets, \code{\link{atlantisf_modelcopy}} to create the simulation
#'   directory and copy the calibrated model inside it, \code{\link{atlantis_runtime}}
#'   to modify the runtime in the run parameters file, \code{\link{atlantis_wsummary}}
#'   to modify periodicity of summary outputs, \code{\link{atlantis_fchange}}
#'   to change the value of F, \code{\link{atlantis_bachchange}} to change the
#'   name of output files, \code{\link[stringr]{str_split_fixed}} to split a chain
#'   of characters, \code{\link[qdapRegex]{ex_number}} to extract number from a chain of
#'   characters, \code{\link[ncdf4]{nc_open}} to open netCDF4 files,
#'   \code{\link[ncdf4]{ncvar_get}} to extract variables from netCDF4 files,
#'   \code{\link[ncdf4]{nc_close}} to close netCDF4 files, and
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt,
#'   .csv ...).
#' @export

# Function used:
# - atlantis_checkmodule (check.R)
# - atlantis_fgrpon (check.R)
# - atlantis_fgrpimp (check.R)
# - atlantis_paraselect (fileselect.R)
# - atlantis_fleetopen (check.R)
# - atlantis_checkf (check.R)
# - atlantis_fdistri (ini.R)
# - atlantisf_modelcopy (simudir.R)
# - atlantis_runtime (ini.R)
# - atlantis_wsummary (ini.R)
# - atlantis_fchange (core.R)
# - atlantis_bachchange (core.R)

# #test
# rm(list = ls())
# gc()
# # library(devtools)
# # devtools::install_github("rgirardi/atlantisfmsy", build_vignettes = T)
#
# library(atlantisfmsy)
#
# # help(atlantisfmsy_simu)
#
# # folder_path <- "Z:"
# # model_path <- "Z:/AEEC_F"
# # exe_name <- "atlantisMerged"
# folder_path <- "D:"
# model_path <- "C:/Users/rgirardi/Dropbox/AEEC_Fv6290"
# exe_name <- "atlantismain"
# burnin_time <- 30 * 365
#
# batch_file <- "runAtlantis.bat"
#
# func_grp <- c("COD","LBT")
# fref <- c(0.1, 0.1)
# fmult <- seq(0, 1, by = 0.2)
# label <- "HTL"

atlantis_fsimu = function(func_grp, folder_path, model_path, exe_name, burnin_time, label, batch_file = NULL, fmult, fref = rep(1,length(fmult))){
  # convert path on Windows to avoid issues with space in path
  folder_path <- pathconvert(folder_path)
  model_path <- pathconvert(model_path)

  # Check os used.
  os <- Sys.info()['sysname']

  if(!os %in% c("Windows","Linux"))
    stop("This package is not developped to work on iOS. Several modifications are required to copy files and directories, and run Atlantis.")

  # Check if model path is the right path.
  if(length(grep(exe_name, list.files(model_path))) == 0)
    stop("Verify that model_path is the correct path to your calibrated model folder. If it is, verify that exe_name is the correct name of atlantis executable and that the executable is in the folder. Then run atlantisfmsy_simu again.")

  # Check if there is more than one shell/batch file in the model directory.
  if(is.null(batch_file)){
    if(os == "Windows"){
      if(length(list.files(model_path)[regexpr(".bat", list.files(model_path), fixed = T) != -1]) != 1)
        stop("More than one batch file exist in the model directory. Please remove the one not used to run the model.")
    } else {
      if(length(list.files(model_path)[regexpr(".sh", list.files(model_path), fixed = T) != -1]) != 1)
        stop("More than one shell file exist in the model directory. Please remove the one not used to run the model.")
    }
  }

  # Check if physics, biology and fishery modules are activated in Atlantis.
  if(atlantis_checkmodule(model_path, exe_name, batch_file) == 0)
    stop(paste("Please check the file ", atlantis_paraselect(model_path, exe_name, "-r", batch_file), ", at least one of the following modules is not running in the calibrated model: physics, biology or fishery.", sep = ""))

  for (func_grp_temp in func_grp){
    # Check if functional groups on in calibrated model.
    if(atlantis_fgrpon(func_grp_temp, model_path, exe_name, batch_file) == 0)
      stop(paste("The functional group ", func_grp_temp, " is turned off in the calibrated model.", sep = ""))

    # Check if functional group is fished and impacted in the calibrated model.
    if(atlantis_fgrpimp(func_grp_temp, model_path, exe_name, batch_file) == 0)
      stop(paste("The functional group ", func_grp_temp, " is either not fished or not impacted. If you still want to estimate the Fmsy for that functional group please modify the file ", atlantis_paraselect(model_path, exe_name, "-s", batch_file), " to turn on IsFished and IsImpacted. If you want to apply a particular distribution of the fishing pressure accross your fleets please fill the parameter mFC for that functional group with the distribution you want to apply. If you wnat to keep the F distributes uniformly accross fleets leave all the values in the vector mFC equal to 0. Then run the function atlantisfmsy_simu again.", sep = ""))
  }


  # # Check if simulation folder already exist or not. If it exist and atlantisfmsy_restart wasn't use, ask to delete the folder and stop the script.
  # if(paste(folder_path, "AtlantisMSY", func_grp, sep = "/") %in% list.dirs(paste(folder_path, "AtlantisMSY", sep = "/"), recursive = F) & is.null(fmin) & restart == 0)
  #   stop(paste("You've already run the simulation for the functional group: ", func_grp, ". If your computer stopped during the simulation please use atlantisfmsy_restart or else remove the folder ", func_grp," from the directory AtlantisMSY and run the function atlantisfmsy_simu again.", sep = ""))

  # Check fleets definition in the calibrated model.
  run_time <- burnin_time + (30 * 365) # burn in time + 30 extra years, average catch will be calculated on the 5 last years.
  harvest_filename <- atlantis_paraselect(model_path, exe_name, "-h", batch_file) #looking for harvest parameters file.

  fishing_para <- atlantis_fleetopen(model_path, exe_name, harvest_filename, run_time, batch_file) #check if fleets turn on.
  if(sum(fishing_para$active_flt) == 0)
    stop("No fleet is active in your model please change your harvest parameters file.")

  fishing_para <- atlantis_checkf(model_path, harvest_filename, fishing_para) #check if model is calibrated with fishing mortality.
  if(sum(fishing_para$active_flt * fishing_para$effortmodel) != 0)
    stop("Your Atlantis model is not calibrated with fishing mortality (efffortmodel = 0) for open fleets.")

  # Extract distribution of F amongst fleets in the calibrated model.
  f_prop <- list()
  for (fg in func_grp){
    f_prop_fg <- atlantis_fdistri(fg, model_path, harvest_filename, fishing_para)
    f_prop[[which(func_grp == fg)]] <- f_prop_fg
  }

  # Create simulation folder for f scenarios
  simu_path <- atlantisf_modelcopy(folder_path, model_path, exe_name, batch_file, folder_name = label)

  #change running time in Atlantis.
  atlantis_runtime(simu_path, exe_name, run_time, batch_file) #burning time plus 30 years

  #change stock state summary periodicity.
  atlantis_wsummary(simu_path, exe_name, batch_file)


  while(length(fmult)>0){
    fsp <- fmult[1] * fref
    for (fg in func_grp) {
      atlantis_fchange(fg, simu_path, harvest_filename, f_prop[[which(func_grp == fg)]], fsp[which(func_grp == fg)]/365, fishing_para) #change F in harvest file.
    }
    atlantis_bachchange(label, simu_path, exe_name, fmult[1]/365, batch_file, msy = F, output = F) #renamed the output files using label and F multipliers

    gwd_ini <- getwd()
    setwd(simu_path)
    if (os == "Windows") {
      shell(list.files()[regexpr(".bat", list.files(), fixed = T) != -1]) #run Atlantis on Windows.
    } else {
      system(paste("sh",list.files()[regexpr(".sh", list.files(), fixed = T) != -1], sep= " ")) #run Atlantis on Linux.
    }
    setwd(gwd_ini)
    fmult <- fmult[-1]

    output_path <- unlist(strsplit(atlantis_paraselect(simu_path, exe_name, "-o", batch_file), "/"))  #search for the output directory in bach file.
    output_path <- file.path(simu_path, output_path[-length(output_path)])
    utils::write.table(fmult, file.path(output_path, "Fnext_simu.txt"), sep = ",", dec = ".", row.names = F)
  }
  print(paste0("Done with ", label, " scenarios!"))
}
