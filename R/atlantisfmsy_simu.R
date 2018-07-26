#' Fmsy estimation in Atlantis ecosystem model.
#'
#' @description Run set of simulations to estimate Fmsy in an Atlantis model for
#'   a specific functional group. See Atlantis website for more information on
#'   the ecosystem model (\url{https://research.csiro.au/atlantis/}, 03/01/2018).
#'   The iterative simulations protocole to find Fmsy was defined by Dr. E.A.
#'   Fulton (CSIRO) for the IndiSeas project (\url{http://www.indiseas.org/},
#'   accessed 03/01/2018). It is designed first to gradualy increase F for a
#'   particular functional group in the model up to the collapse of the stock.
#'   Then, it narrows down the value of Fmsy (in y-1) with a precision of two
#'   decimals. To do so, it copies/pastes all the files necessary to run the
#'   calibrated model into a seperate directory and then modifies the parameter
#'   files in this directory before each simulation.
#'  @section Warning:
#'   At the moment, the package is not designed to run on OSX, it only
#'   works on Linux and Windows (To run on OSX these functions need to be
#'   modified: \code{\link{atlantisfmsy_simu}},
#'   \code{\link{atlantisfmsy_restart}}, \code{\link{atlantisfmsy_modelcopy}},
#'   \code{\link{atlantis_bachchange}}, and \code{\link{atlantis_paraselect}}).
#'   As well this package required that only one bach/shell file is in the model
#'   directory (if more than one bach/shell files are present please remove
#'   them, leave only the one used to run the calibrated model).
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param folder_path The working directory: where the Fmsy simulations will be
#'   run and stored.
#' @param model_path The directory of the calibrated model (containing all the
#'   parameters files and one bach file. Forcing files can be stored in a direct
#'   parent directory of model_path). \strong{WARNING:} Only working if the forcing
#'   folder is in the main model directory \code{model_path} or if it is in the
#'   direct parent directory. If not please either modify this package or modify
#'   the path structure of your Atlantis input forcing parameters file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param burnin_time The time in days needed for the model to pseudo-stabilize
#'   before starting the additional 30 years of simulation.
#' @param fmax The maximal value of F the simulation is going to test in y-1.
#'   \strong{WARNING:} only work if \code{fmax} < 10 y-1 (see
#'   \code{\link{atlantis_bachchange}}).
#' @param fmin The minimum value of F to test in y-1 (only used when the
#'   simulation are restarted after an interruption). The default value is NULL.
#' @param restart a flag indicating if the simulation is restarted from an
#'   interruption or not and if the first set of simulation have been completed
#'   or not. The default value is 0. Mainly used by
#'   \code{\link{atlantisfmsy_restart}}.
#' @return \code{fmsy} The dataframe containing all the values of F (sum of mFC
#'   parameter value in y-1) tested and the average yearly catch of the 5 last
#'   years of simulation. As well, a message with the Fmsy value for the
#'   Atlantis functional group considered is printed and all the simulation
#'   necessary to estimate the Fmsy are stored in the folder_path.
#' @examples
#' atlantisfmsy_simu("COD", "C:/Atlantis/AtlantisEEC/Fmsy",
#'                   "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                   "atlantismain", 7300, 4)
#' atlantisfmsy_simu("COD", "/home/Atlantis/AtlantisEEC/Fmsy",
#'                   "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                   "atlantisNew", 7300, 4)
#'
#' \dontrun{atlantisfmsy_simu("COD", "C:/Atlantis/AtlantisEEC/Fmsy",
#'                            "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                            "atlantismain", 7300, 10)}
#' \dontrun{atlantisfmsy_simu("COD", "/home/Atlantis/AtlantisEEC/Fmsy",
#'                            "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                            "atlantisNew", 7300, 10)}
#' @seealso \code{\link{atlantis_checkmodule}} to check if all the modules are
#'   on (physics, biology, and fishery), \code{\link{atlantis_fgrpon}} to check
#'   if the functional group is active, \code{\link{atlantis_fgrpimp}} to check
#'   if the functional group is fished and impacted,
#'   \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_fleetopen}} to check if fleets are active,
#'   \code{\link{atlantis_checkf}} to check the formulation of the fishing
#'   pressure used (i.e. F, effort, dynamics model...),
#'   \code{\link{atlantis_fdistri}} to extract the distribution of fishing
#'   pressure amongst fleets, \code{\link{atlantisfmsy_inisimu}} to initialize
#'   and run the first set of simulations, \code{\link{atlantisfmsy_fmaxcatch}}
#'   to find the F with most important catch and set the two next F to be
#'   tested, \code{\link{atlantisfmsy_completion}} to check if Fmsy has been
#'   reached, \code{\link{atlantis_fchange}} to change the value of F,
#'   \code{\link{atlantis_bachchange}} to change the name of output files,
#'   \code{\link[stringr]{str_split_fixed}} to split a chain of characters,
#'   \code{\link[qdapRegex]{ex_number}} to extract number from a chain of
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
# - atlantisfmsy_inisimu (core.R)
# - atlantisfmsy_fmaxcatch (core.R)
# - atlantisfmsy_completion (check.R)
# - atlantis_fchange (core.R)
# - atlantis_bachchange (core.R)

atlantisfmsy_simu = function(func_grp, folder_path, model_path, exe_name, burnin_time, fmax, fmin = NULL, restart = 0) {

  # Check os used.
  os <- Sys.info()['sysname']

  if(!os %in% c("Windows","Linux"))
    stop("This package is not developped to work on iOS. Several modifications are required to copy files and directories, and run Atlantis.")

  # Check if model path is the right path.
  if(length(grep(exe_name, list.files(model_path))) == 0)
    stop("Verify that model_path is the correct path to your calibrated model folder. If it is, verify that exe_name is the correct name of atlantis executable and that the executable is in the folder. Then run atlantisfmsy_simu again.")

  # Check if physics, biology and fishery modules are activated in Atlantis.
  if(atlantis_checkmodule(model_path, exe_name) == 0)
    stop(paste("Please check the file ", atlantis_paraselect(model_path, exe_name, "-r"), ", at least one of the following modules is not running in the calibrated model: physics, biology or fishery.", sep = ""))

  # Check if functional group on in calibrated model.
  if(atlantis_fgrpon(func_grp, model_path, exe_name) == 0)
    stop(paste("The functional group ", func_grp, " is turned off in the calibrated model.", sep = ""))

  # Check if functional group is fished and impacted in the calibrated model.
  if(atlantis_fgrpimp(func_grp, model_path, exe_name) == 0)
    stop(paste("The functional group ", func_grp, " is either not fished or not impacted. If you still want to estimate the Fmsy for that functional group please modify the file ", atlantis_paraselect(model_path, exe_name, "-s"), " to turn on IsFished and IsImpacted. If you want to apply a particular distribution of the fishing pressure accross your fleets please fill the parameter mFC for that functional group with the distribution you want to apply. If you wnat to keep the F distributes uniformly accross fleets leave all the values in the vector mFC equal to 0. Then run the function atlantisfmsy_simu again.", sep = ""))

  # Check if simulation folder already exist or not. If it exist and atlantisfmsy_restart wasn't use, ask to delete the folder and stop the script.
  if(paste(folder_path, "AtlantisMSY", func_grp, sep = "/") %in% list.dirs(paste(folder_path, "AtlantisMSY", sep = "/"), recursive = F) & is.null(fmin) & restart == 0)
    stop(paste("You've already run the simulation for the functional group: ", func_grp, ". If your computer stopped during the simulation please use atlantisfmsy_restart or else remove the folder ", func_grp," from the directory AtlantisMSY and run the function atlantisfmsy_simu again.", sep = ""))

  # Check if there is more than one shell/batch file in the model directory.
  if(os == "Windows"){
    if(length(list.files(model_path)[regexpr(".bat", list.files(model_path), fixed = T) != -1]) != 1)
      stop("More than one batch file exist in the model directory. Please remove the one not used to run the model.")
  } else {
    if(length(list.files(model_path)[regexpr(".sh", list.files(model_path), fixed = T) != -1]) != 1)
      stop("More than one shell file exist in the model directory. Please remove the one not used to run the model.")
  }

  # Check fleets definition in the calibrated model.
  run_time <- burnin_time + (30 * 365) # burn in time + 30 extra years, average catch will be calculated on the 5 last years.
  harvest_filename <- atlantis_paraselect(model_path, exe_name, "-h") #looking for harvest parameters file.

  fishing_para <- atlantis_fleetopen(model_path, exe_name, harvest_filename, run_time) #check if fleets turn on.
  if(sum(fishing_para$active_flt) == 0)
    stop("No fleet is active in your model please change your harvest parameters file.")

  fishing_para <- atlantis_checkf(model_path, harvest_filename, fishing_para) #check if model is calibrated with fishing mortality.
  if(sum(fishing_para$active_flt * fishing_para$effortmodel) != 0)
    stop("Your Atlantis model is not calibrated with fishing mortality (efffortmodel = 0) for open fleets.")

  # Extract distribution of F amongst fleets in the calibrated model.
  f_prop <- atlantis_fdistri(func_grp, model_path, harvest_filename, fishing_para)

  # run the first set of F (from F = 0 to fmax by 0.4 step).
  if(restart == 0) {
    simu_path <- atlantisfmsy_inisimu(func_grp, folder_path, model_path, exe_name, harvest_filename, run_time, f_prop, fmax, fmin, fishing_para)
    atlantisfmsy_fmaxcatch(func_grp, simu_path, exe_name, run_time, fmax) #determine the maximum yield and the F associate. Then determine the next set of F to simulate. return end = 1 if Fmsy is reached or 2 if maximum yield is reached for fmax.
    output_path <- unlist(strsplit(atlantis_paraselect(simu_path, exe_name, "-o"), "/"))  #search for the output directory in bach file.
    output_path <- output_path[-length(output_path)]
    output_path <- file.path(simu_path, output_path)
    end <- atlantisfmsy_completion(func_grp, output_path)

    if(end == 2) {return(invisible())}

  } else {
    simu_path <- file.path(folder_path, "AtlantisMSY", func_grp) #if restart the simulation after computer shutdown and atlantisfmsy_inisimu completed.
    output_path <- unlist(strsplit(atlantis_paraselect(simu_path, exe_name, "-o"), "/"))  #search for the output directory in bach file.
    output_path <- output_path[-length(output_path)]
    output_path <- file.path(simu_path, output_path)
    end <- 0
  }

  gwd_ini <- getwd()
  setwd(simu_path)
  while(end == 0) {
    # setwd(output_path)
    f_simu <- utils::read.table(file.path(output_path, "Fnext_simu.txt"), sep = ",", dec = ".", header = T) #table previously created by atlantisfmsy_fmaxcatch.
    if(dim(f_simu)[2] == 4)
    {
      f_simu <- as.numeric(f_simu[, c("ft1", "ft2")]) / 365
    } else {
      f_simu <- as.numeric(f_simu[,grep('ft[0-9]', names(f_simu))]) / 365
    }

    while(length(f_simu) != 0) {
      atlantis_fchange(func_grp, simu_path, harvest_filename, f_prop, f_simu, fishing_para) #change F in harvest file.
      f_simu <- atlantis_bachchange(func_grp, simu_path, exe_name, f_simu) #renamed the output files.
      if(os == "Windows") {
        shell(list.files()[regexpr(".bat", list.files(), fixed = T) != -1]) #run Atlantis on Windows.
      } else {
        system(paste("sh",list.files()[regexpr(".sh", list.files(), fixed = T) != -1], sep= " ")) #run Atlantis on Linux.
      }
      gc()
    }
    atlantisfmsy_fmaxcatch(func_grp, simu_path, exe_name, run_time, fmax)  #determine the maximum yield and the F associated.
    end <- atlantisfmsy_completion(func_grp, output_path)  #Then determine the next set of F to simulate. return end = 1 if Fmsy is reached or 2 if maximum yield is reached for fmax (meaning that no Fmsy have been found).
    gc()
  }
  setwd(gwd_ini)

  fmsy <- utils::read.table(paste(output_path,"/Fmsy_", func_grp,".txt", sep = ""), sep = ",", dec = ".", header = T) #table previously created by atlantisfmsy_fmaxcatch.

  gc()
  print(paste("Your simulation is completed. The Fmsy for ", func_grp, " functional group is : ", fmsy$f[fmsy$yield == max(fmsy$yield)], " y-1", sep = ""))
  return(fmsy)
}
