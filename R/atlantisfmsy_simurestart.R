#' Restart Fmsy simulation.
#'
#' @description In case of an interruption of \code{\link{atlantisfmsy_simu}},
#'   this function will restart the Fmsy estimation protocol where it was
#'   stopped. It will first look if all the condition to run the simulations are
#'   met, then look for the last value of F simulated, and then restart the
#'   function \code{\link{atlantisfmsy_simu}}.
#' @section Warning:
#'   This doesn't work if the files have been transfered to another computer
#'   because the date of file creation change and this date is used to determine
#'   the last simulated F value (see \code{\link{atlantis_lastsimu}}). If it is
#'   the case, you have to specify a value in the parameter \code{last_run}.
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
#' @param fmax The maximal value of F the simulation is going to test in y-1.
#'   \strong{WARNING:} only work if \code{fmax} < 10 y-1 (see
#'   \code{\link{atlantis_bachchange}}).
#' @param last_run The name of the output file of the last simulation run before
#'   \code{\link{atlantisfmsy_simu}} stopped. The name of the output have to be
#'   the same as the one in the batch/shell file used to run Atlantis.
#'   \strong{Default:} NULL.
#' @param batch_file The name of the batch/shell file with extension you are using
#'   to run your model. If not provided, the function will search for the unique
#'   batch file in your \code{folder_path}. \strong{Default:} NULL.
#' @return \code{fmsy} The dataframe containing all the values of F (sum of mFC
#'   parameter value in y-1) tested and the average yearly catch of the 5 last
#'   years of simulation. As well, a message with the Fmsy value for the
#'   Atlantis functional group considered is printed and all the simulation
#'   necessary to estimate the Fmsy are stored in the folder_path.
#' @examples
#' atlantisfmsy_restart("COD", "C:/Atlantis/AtlantisEEC",
#'                      "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                      "atlantismain", 4)
#' atlantisfmsy_restart("COD", "/home/Atlantis/AtlantisEEC",
#'                      "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                      "atlantisNew", 4)
#' atlantisfmsy_restart("COD", "C:/Atlantis/AtlantisEEC",
#'                      "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                      "atlantismain", 4, "MSY_COD_F012.nc", "runAtlantis.bat")
#' atlantisfmsy_restart("COD", "/home/Atlantis/AtlantisEEC",
#'                      "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                      "atlantisNew", 4, "MSY_COD_F012.nc", "runAtlantis.sh")
#'
#' \dontrun{atlantisfmsy_restart("COD", "C:/Atlantis/AtlantisEEC",
#'                               "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                               "atlantismain", 10)}
#' \dontrun{atlantisfmsy_restart("COD", "/home/Atlantis/AtlantisEEC",
#'                               "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                               "atlantisNew", 10)}
#' @seealso \code{\link{atlantis_fgrpon}} checking if the functional group is
#'   active, \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_openfile}} to open a parameters file and select a
#'   parameter, \code{\link{atlantis_lastsimu}} to find the last F value tested,
#'   \code{\link{atlantisfmsy_completion}} to check if Fmsy has been reached,
#'   \code{\link{atlantisfmsy_simu}} to run the simulation to find the Fmsy,
#'   \code{\link[stringr]{str_split_fixed}} to split a chain of characters,
#'   \code{\link[qdapRegex]{ex_number}} to extract number from a chain of
#'   characters, \code{\link[ncdf4]{nc_open}} to open netCDF4 files,
#'   \code{\link[ncdf4]{ncvar_get}} to extract variables from netCDF4 files,
#'   \code{\link[ncdf4]{nc_close}} to close netCDF4 files,
#'   \code{\link[utils]{write.table}} to write table on the drive, and
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt,
#'   .csv ...).
#' @export

# Function used:
# - atlantis_fgrpon (check.R)
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)
# - atlantis_lastsimu (simurestart.R)
# - atlantisfmsy_completion (check.R)
# - atlantisfmsy_simu (simu.R)

# WARNING doesn't work if files have been transfered to another computer because the date of creation change (see atlantis_lastsimu function). ## CHECK IF COME FROM DOWNLOADING OR ALSO FROM TRANSFERING WITH USB.
atlantisfmsy_restart = function(func_grp, folder_path, model_path, exe_name, fmax, last_run = NULL, batch_file = NULL) {
  # gwd_ini <- getwd()
  # convert path on Windows to avoid issues with space in path
  folder_path <- pathconvert(folder_path)
  model_path <- pathconvert(model_path)

  # Check os used.
  os <- Sys.info()['sysname']

  if(!os %in% c("Windows","Linux"))
    stop("This script is not developped to work on iOS. Several modifications are required to copy files and directories, and run Atlantis.")

  # Check if functional group on in calibrated model.
  if(atlantis_fgrpon(func_grp, model_path, exe_name, batch_file) == 0)
    stop(paste("The functional group ", func_grp, " is turned off in the calibrated model.", sep = ""))

  # test if simulations for func_grp already exist.
  if(!file.path(folder_path, "AtlantisMSY", func_grp) %in% list.dirs(file.path(folder_path, "AtlantisMSY"), recursive = F))
    stop(paste("You have never run this script for the functional group ", func_grp, ". Use the atlantisfmsy_simu function.", sep = ""))

  # test if last_run parameter is well specified if specified.
  if(!is.null(last_run)) {
    test <- stringr::str_split_fixed(last_run, "_", n = 3)
    if(test[1] != "MSY" | test[2] != func_grp | substring(test[3], 1, 1) != "F" | grepl("[^0-9]", substring(test[3], 2, 4)) | substring(test[3], 5, 7) != ".nc")
      stop(paste(last_run, " is not well specified. It should be of the form MSY_", func_grp, "_F[1-9][1-9][1-9].nc. Please change last_run parameter to match the value in your batch/shell file or in your output folder.", sep = ""))
    rm(test)
  }

  simu_path <- file.path(folder_path, "AtlantisMSY", func_grp)

  # path running parameters file.
  infilename <- atlantis_paraselect(simu_path, exe_name, "-r", batch_file)

  # Extract running time from previous run.
  para <- atlantis_openfile(simu_path, infilename, "tstop")
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  run_time <- as.numeric(line_para[grep("[0-9]+", line_para)[1]])

  burnin_time <- run_time - (30 * 365) # burn in time + 30 extra years, average catch will be calculated on the 5 last years.

  # set run parameters file to rewrite previous unfinished simulation if already exist.
  para <- atlantis_openfile(simu_path, infilename, "flagreusefile")
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  line_para[grep("[0-9]+", line_para)[1]] <- "2" # replace output files.

  params[idxline] <- paste(as.character(line_para), collapse = " ")

  write(params, file.path(simu_path, infilename))

  # search for the output directory in bach file.
  output_path <- unlist(strsplit(atlantis_paraselect(simu_path, exe_name, "-o", batch_file), "/"))
  output_path <- output_path[-length(output_path)]
  output_path <- paste(simu_path, output_path, sep = "/")

  if(is.null(last_run)){
    output_files <- atlantis_lastsimu(output_path)[[1]]
    fmin <- atlantis_lastsimu(output_path)[[2]]
  } else {
    output_files <- atlantis_lastsimu(output_path, last_run)[[1]]
    fmin <- atlantis_lastsimu(output_path, last_run)[[2]]
  }
  output_files <- gsub(paste0(output_path,"/"), "", output_files)

  if("Fnext_simu.txt" %in% output_files){

    end <- atlantisfmsy_completion(func_grp, output_path)  #  Determine the next set of F to simulate. return end=1 if Fmsy is reached or if maximum yield is reached for fmax
    if(end == 1)
      stop(paste("Simulation for the functional group ", func_grp, " have already been completed. Please change func_grp. See Fmsy_" , func_grp, ".txt in the output folder ", output_path, " to get the results.", sep = ""))
    if(end == 2)
      return(invisible())

    f_simu <- utils::read.table(file.path(output_path,"Fnext_simu.txt"), sep = ",", dec = ".", header = T)
    # remove simulation already completed from the next set of simulation.
    if (dim(f_simu)[2] == 4){
      if (as.numeric(f_simu$ft2) == fmin){
        f_simu <- f_simu[, names(f_simu) != "ft1"]
        utils::write.table(f_simu, file.path(output_path,"Fnext_simu.txt"), sep = ",", dec = ".", row.names = F)
      }
    }
    fmsy <- atlantisfmsy_simu(func_grp, folder_path, model_path, exe_name, burnin_time, fmax, batch_file, restart = 1) # restart at the second stage: MSY estimation.
  } else {
    fmsy <- atlantisfmsy_simu(func_grp, folder_path, model_path, exe_name, burnin_time, fmax, batch_file, fmin = fmin) # restart the function atlantisfmsy_inisimu (first stage: collapse estimation) with fmin as the starting point of the simulation sequency.
  }

  gc()
  return(fmsy)
}

#' Look for last F simulated.
#'
#' @description It looks for the last value of F simulated. It searches through
#'   all the output files generated by Atlantis in the \code{output_path}. And
#'   looks for the files created the most recently using \code{\link[base]{file.info}}.
#' @param output_path The directory of the simulations outputs for the
#'   functional group considered.
#' @param last_run The name of the output file of the last simulation run before
#'   \code{\link{atlantisfmsy_simu}} stopped. The name of the output have to be
#'   the same as the one in the batch/shell file used to run Atlantis. The
#'   default value is NULL.
#' @return A list containing a vector with the names of each files in
#'   \code{output_path} and the value of the last F simulated in y-1.
#' @examples
#' atlantis_lastsimu("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD/output")
#' atlantis_lastsimu("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD/output")
#' @seealso \code{\link[base]{file.info}} to recover files description.

# WARNING doesn't work if files have been transfered to another computer because the date of creation change.
atlantis_lastsimu = function(output_path, last_run = NULL) {

  output_files <- list.files(output_path, full.names = T)

  if(is.null(last_run)) {
    # Check last F simulated.
    # find last file created (meaning last simulation).
    file.info.nc <- file.info(output_files)[grep(".nc", rownames(file.info(output_files))), ]
    file.info.nc$mjulian <- round(as.numeric(as.POSIXlt(file.info.nc$mtime, format = "%Y-%m-%d %H:%M:%S")))
    file.info.nc <- file.info.nc[file.info.nc$mjulian == max(file.info.nc$mjulian),]
    file.info.nc <- file.info.nc[rownames(file.info.nc) %in% rownames(file.info.nc)[nchar(rownames(file.info.nc)) == min(nchar(rownames(file.info.nc)))],]

    # extract prefix of file names.
    outputID <- rownames(file.info.nc)
  } else {
    outputID <- last_run
  }

  outputID <- gsub(".nc", "", outputID) # remove extension
  outputID <- gsub(paste(output_path,"/", sep = ""), "", outputID) # remove directory

  # extract F simu.
  fmin <- unlist(strsplit(outputID, "_"))[3]
  fmin <- as.numeric(substring(fmin, 2, 4))/100

  gc()
  return(list(output_files, fmin))
}
