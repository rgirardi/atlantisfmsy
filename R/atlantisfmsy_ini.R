#' Change simulation run time.
#'
#' @description Change the run time in the run parameters file. It opens the run
#'   parameters file, select the \code{tstop} parameter and replaces his value by:
#'   \code{runtime} = \code{burning_time} + 10950 days.
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param run_time The total duration of the simulation in days
#'   (\code{burnin_time} + 10950).
#' @return Modify run parameters file.
#' @examples
#' atlantis_runtime("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantismain", 18250)
#' atlantis_runtime("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantisNew", 18250)
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection, and
#'   \code{\link{atlantis_openfile}} to open parameter files and select
#'   parameters.
#' @export

# Function used:
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)

atlantis_runtime = function(path, exe_name, run_time) {
  # convert path on Windows to avoid issues with space in path
  path <- pathconvert(path)

  #selection of Atlantis parameters file.
  infilename <- atlantis_paraselect(path, exe_name, "-r") #looking for run parameters file.

  para <- atlantis_openfile(path, infilename, "tstop")
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  line_para[grep("[0-9]+", line_para)[1]] <- as.character(run_time)

  params[idxline] <- paste(as.character(line_para), collapse = " ")

  write(params, file.path(path, infilename))
  gc()
}

#' Change writing stock state summary periodicity.
#'
#' @description Change writing stock state summary periodicity in the run
#'   parameters file. It opens the run parameters file, select the
#'   \code{tsumout} parameter and replaces his value by: 365 days.
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @return Modify run parameters file.
#' @examples
#' atlantis_runtime("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantismain")
#' atlantis_runtime("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantisNew")
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection, and
#'   \code{\link{atlantis_openfile}} to open parameter files and select
#'   parameters.
#' @export

# Function used:
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)

atlantis_wsummary = function(path, exe_name) {
  # convert path on Windows to avoid issues with space in path
  path <- pathconvert(path)

  #selection of Atlantis parameters file.
  infilename <- atlantis_paraselect(path, exe_name, "-r") #looking for run parameters file.

  para <- atlantis_openfile(path, infilename, "tsumout")
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  line_para[grep("[0-9]+", line_para)[1]] <- as.character(365)

  params[idxline] <- paste(as.character(line_para), collapse = " ")

  write(params, file.path(path, infilename))
  gc()
}

#' Atlantis F distribution.
#'
#' @description Extract the fishing mortality distribution accross fleets in
#'   Atlantis. It opens the harvest parameters file, select the \code{mFC_XXX}
#'   vector for the considered functional group \code{func_grp} and return the
#'   proportion of the total F allocated to each fleet. If the functional group
#'   \code{func_grp} is unfished in the calibrated model this function will
#'   attribute fishing pressure evenly accross active fleets.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param model_path The directory of the calibrated model (containing all the
#'   parameters files and one bach file. Forcing files can be stored in a direct
#'   parent directory of model_path). \strong{WARNING:} Only working if the forcing
#'   folder is in the main model directory \code{model_path} or if it is in the
#'   direct parent directory. If not please either modify this package or modify
#'   the path structure of your Atlantis input forcing parameters file.
#' @param harvest_filename The name of the harvest parameters file with its
#'   extension (ex: file.prm).
#' @param fishing_para A dataframe containing data from the fishing fleet csv
#'   file plus two extra columns, one to indicate if the fleet is active (1) or
#'   not (0) named \code{active_flt} and one named \code{effortmodel} containing
#'   the effortmodel option used for each fleet (output from the function
#'   \code{\link{atlantis_checkf}}).
#' @param raw.distri If TRUE return the raw value of fishing mortality (F) for
#'   active fleets (non-active fleets are set to 0), else the distribution of F is
#'   returned (vector of values between [0-1] and their sum is 1). \strong{Default:}
#'   FALSE.
#' @return \code{f_prop} A vector with the proportion of fishing pressure
#'   applied per each fleet. The order of the fleets is the same as the one used
#'   in Atlantis.
#' @examples
#' atlantis_fdistri("COD", "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'  "AEEC_harvest.prm", fishing_para)
#' atlantis_fdistri("COD", "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'  "AEEC_harvest.prm", fishing_para)
#' @seealso \code{\link{atlantis_openfile}} to open a parameters file and select
#'   a parameter.
#' @export

# Function used:
# - atlantis_openfile (fileselect.R)

atlantis_fdistri = function(func_grp, model_path, harvest_filename, fishing_para, raw.distri = F) {
  # convert path on Windows to avoid issues with space in path
  path <- pathconvert(model_path)

  #open harvest file.
  para <- atlantis_openfile(model_path, harvest_filename, paste("mFC_", func_grp, sep = ""))
  params <- para[[1]]
  idxline <- para[[2]] + 1

  #extract F parameter for species func_grp.
  mfc_sp <- as.numeric(unlist(strsplit(params[idxline], " ")))
  mfc_sp <- mfc_sp[!is.na(mfc_sp)]

  #Calculate the distribution of F accross fleets based on harvest parameter file.
  f_prop <- mfc_sp * fishing_para$active_flt #only considered active fleet.
  if(!raw.distri){
    if(sum(f_prop) != 0){
      f_prop <- f_prop / sum(f_prop)
    } else {
      warning("You are trying to calculate Fmsy for an unfished functional group (at least in the calibrated model). The package will attribute evenly the fishing pressure across active fleets.")
      f_prop <- fishing_para$active_flt / sum(fishing_para$active_flt) #if null in calibrated Atlantis model F is distributed equally accross active fleet.
    }
  }
  gc()
  return(f_prop)
}
