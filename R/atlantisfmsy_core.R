#' Change F in Atlantis.
#'
#' @description It modifies the harvest parameters file by applying a new value
#'   of F for the functional group \code{func_grp}. It looks for \code{mFC_XXX}
#'   parameter and and change the value of the vector using the new F value
#'   \code{f_test} that need to be simulated and the distribution of the fishing
#'   pressure accross fleets \code{f_prop} from the calibrated model.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param path The directory of the batch or shell file.
#' @param harvest_filename The name of the harvest parameters file with its
#'   extension (ex: file.prm).
#' @param f_prop The proportion of the total fishing pressure applied per each
#'   fleet. The order of the fleets is the same as the one used in Atlantis.
#' @param f_test The new value of total fishing mortality to be tested in days.
#' @param fishing_para A dataframe containing data from the fishing fleet csv
#'   file plus two extra column, one to indicate if the fleet is active (1) or
#'   not (0) named \code{active_flt} and one named \code{effortmodel} containing
#'   the effortmodel option used for each fleet (output from the function
#'   \code{\link{atlantis_checkf}}).
#' @return Change value of F in the harvest parameters files.
#' @examples
#' atlantis_fchange("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "AEEC_harvest.prm", c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 0.0002739726,
#'   fishing_para)
#' atlantis_fchange("COD", "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "AEEC_harvest.prm", c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 0.0002739726,
#'   fishing_para)
#' @seealso \code{\link{atlantis_openfile}} to open a parameters file and select
#'   a parameter.

# Function used:
# - atlantis_openfile (fileselect.R)

atlantis_fchange = function(func_grp, path, harvest_filename, f_prop, f_test, fishing_para) {
  #open harvest file.
  para <- atlantis_openfile(path, harvest_filename, paste("mFC_", func_grp, sep = ""))
  params <- para[[1]]
  idxline <- para[[2]] + 1

  #extract F parameter for species func_grp.
  mfc_sp <- as.numeric(unlist(strsplit(params[idxline], " ")))
  mfc_sp <- mfc_sp[!is.na(mfc_sp)]

  #distribute new F to fleets according to previous distribution of F in the calibrated model.
  mfc_sp <-  f_prop * f_test[1]

  params[idxline] <- paste(as.character(mfc_sp), collapse = " ")

  # writeLines(params, con = infilecon)
  write(params, file.path(path, harvest_filename))
  gc()
}

#' Change bach/shell Atlantis file.
#'
#' @description It modifies the name of the output files for the future run in
#'   the batch/shell file used to run Atlantis. The name is coded with the code
#'   of the functional group selected \code{func_grp} and the value of F that
#'   will be tested \code{f_test} (example: For \code{func_grp} = "COD" with a
#'   \code{f_test} = 0.0002739726 (0.1 y-1) the output file prefix will be
#'   MSY_COD_F010).
#' @section Warning:
#'   Only work if \code{fmax} < 10 y-1.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param path The directory of the batch or shell file.
#' @param os The operating system used (ex:"Windows" or "Linux"). \strong{WARNING:}
#'   At the moment, the package is not designed to run on OSX (see
#'   \code{\link{atlantisfmsy_modelcopy}} and
#'   \code{\link{atlantis_paraselect}}).
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param f_test The new value of total fishing mortality to be tested in days.
#' @return \code{f_test} The new value of total fishing mortality to be tested
#'   in days, and modify batch/shell Atlantis run file.
#' @examples
#' atlantis_bachchange("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantismain", 0.0002739726, "Windows")
#' atlantis_bachchange("COD", "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantisNew", 0.0002739726, "Linux")
#'
#' \dontrun{atlantis_bachchange("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantismain", 0.0002739726, "Darwin") # for OSX.}
#' @seealso \code{\link{atlantis_openfile}} to open a parameters file and select
#'   a parameter.

# Function used:
# - atlantis_openfile (fileselect.R)

atlantis_bachchange = function(func_grp, path, exe_name, f_test, os = Sys.info()['sysname']) {
  #find the Atlantis run bach/shell file.
  if (os == "Windows") {
    infilename <- list.files(path)[regexpr(".bat", list.files(path), fixed = T) != -1]
  } else {
    infilename <- list.files(path)[regexpr(".sh", list.files(path), fixed = T) != -1]
  }

  para <- atlantis_openfile(path, infilename, exe_name)
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))

  output_name <- line_para[which(line_para == "-o") + 1]
  output_name <- unlist(strsplit(output_name, "/"))

  morta <- as.character(round(f_test[1] * 365, digits = 2))
  morta <- gsub("[.]", "", morta)

  while(nchar(morta) != 3) {
    morta <- paste(morta, "0", sep = "")
  }

  output_name[length(output_name)] <- paste("MSY_", func_grp, "_F", morta, ".nc", sep = "")

  line_para[which(line_para == "-o") + 1] <- paste(output_name, collapse = "/")
  params <- paste(line_para, collapse = " ")


  write(params, file.path(path, infilename))
  f_test <- f_test[-1]

  gc()
  return(f_test)
}

#' F simulations up to the collapse of the stock.
#'
#' @description In the directory \code{folder_path}, it creates the folder
#'   \code{AtlantisMSY} and the sub-folder named after the functional group
#'   \code{func_grp} where all simulations will take place. The calibrated model
#'   files from the directory \code{model_path} is copied into this newly
#'   created folder. Then a series of values of F between F = 0 to F =
#'   \code{fmax} with an incrementation of 0.4 y-1 are tested. When the stock
#'   collapse, the function stop and return the directory of the simulation
#'   folder.
#' @section Warning:
#'   Only work if fmax < 10 y-1, see \code{\link{atlantis_bachchange}}).
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
#' @param os The operating system used (ex:"Windows" or "Linux"). \strong{WARNING:}
#'   At the moment, the package is not designed to run on OSX (see
#'   \code{\link{atlantisfmsy_modelcopy}}, \code{\link{atlantis_bachchange}},
#'   and \code{\link{atlantis_paraselect}}).
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param harvest_filename The name of the harvest parameters file with its
#'   extension (ex: file.prm).
#' @param run_time The total duration of the simulation in days
#'   (\code{burnin_time} + 10950).
#' @param f_prop The proportion of the total fishing pressure applied per each
#'   fleet. The order of the fleets is the same as the one used in Atlantis.
#' @param fmax The maximal value of F the simulation is going to test in y-1.
#'   \strong{WARNING:} only work if \code{fmax} < 10 y-1 (see
#'   \code{\link{atlantis_bachchange}}).
#' @param fmin The minimum value of F to test in y-1 (only used when the
#'   simulation are restarted after an interruption). The default value is 0.
#' @param fishing_para A dataframe containing data from the fishing fleet csv
#'   file plus two extra columns, one to indicate if the fleet is active (1) or
#'   not (0) named \code{active_flt} and one named \code{effortmodel} containing
#'   the effortmodel option used for each fleet (output from the function
#'   \code{\link{atlantis_checkf}}).
#' @return \code{simu_path} The directory of the simulation folder, and run
#'   first set of simulations that are stored in the output folder.
#' @examples
#' atlantisfmsy_inisimu("COD", "C:/Atlantis/AtlantisEEC/",
#'  "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3", "atlantismain",
#'   "AEEC_harvest.prm", 18250, c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 4, 0,
#'    fishing_para, "Windows")
#' atlantisfmsy_inisimu("COD", "/home/Atlantis/AtlantisEEC/",
#'  "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3", "atlantisNew",
#'   "AEEC_harvest.prm", 18250, c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 4, 0,
#'    fishing_para, "Linux")
#'
#' \dontrun{atlantisfmsy_inisimu("COD", "C:/Atlantis/AtlantisEEC/",
#'  "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3", "atlantismain",
#'   "AEEC_harvest.prm", 18250, c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 10, 0,
#'    fishing_para, "Windows")}
#' \dontrun{atlantisfmsy_inisimu("COD", "/home/Atlantis/AtlantisEEC/",
#'  "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3", "atlantisNew",
#'   "AEEC_harvest.prm", 18250, c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 10, 0,
#'    fishing_para, "Linux")}
#' \dontrun{atlantisfmsy_inisimu("COD", "/Atlantis/AtlantisEEC/",
#'  "/Atlantis/AtlantisEEC/AtlantisEECF_v3", "atlantismain",
#'   "AEEC_harvest.prm", 18250, c(0.2, 0.45, 0.05, 0.12, 0.08, 0.1), 10, 0,
#'    fishing_para, "Darwin") # for OSX}
#' @seealso \code{\link{atlantisfmsy_modelcopy}} to create the simulation
#'   directory and copy the calibrated model inside it,
#'   \code{\link{atlantis_runtime}} to modify the runtime in the run parameters
#'   file, \code{\link{atlantis_wsummary}} to modify periodicity of summary
#'   outputs, \code{\link{atlantis_fchange}} to change the value of F in the
#'   harvest pramaters file, \code{\link{atlantis_bachchange}} to change output
#'   names in the batch/shell file, and \code{\link{atlantis_avbiomsp}} to
#'   estimate the functional group biomass.

# Function used:
# - atlantisfmsy_modelcopy (simudir.R)
# - atlantis_runtime (ini.R)
# - atlantis_fchange (core.R)
# - atlantis_bachchange (core.R)
# - atlantis_avbiomsp (check.R)

atlantisfmsy_inisimu = function(func_grp, folder_path, model_path, exe_name, harvest_filename, run_time, f_prop, fmax, fmin, fishing_para, os = Sys.info()['sysname']) {
  gwd_ini <- getwd()
  if (fmax >= 10) stop("You need to modify the code to use F higher than or equal to 10: problem with automatization of Atlantis output names for each simulation")

  #create simulation folder for functional group func_grp and copy model parameters files into the new directory.
  if(is.null(fmin)){
    simu_path <- atlantisfmsy_modelcopy(func_grp, folder_path, model_path, exe_name)
    fmin <- 0
  } else {
    simu_path <- file.path(folder_path, "AtlantisMSY", func_grp) #if restart the simulation after computer shutdown.
  }

  f_test <- seq(fmin, fmax, by = 0.4) / 365 #0.4 previously.

  #change running time in Atlantis.
  atlantis_runtime(simu_path, exe_name, run_time)

  #change stock state summary periodicity.
  atlantis_wsummary(simu_path, exe_name)

  biom_sp <- 1
  #set F for each simulation.
  setwd(simu_path)
  while (length(f_test) > 0 & biom_sp != 0) {
    atlantis_fchange(func_grp, simu_path, harvest_filename, f_prop, f_test, fishing_para) #change F in harvest file.
    f_test <- atlantis_bachchange(func_grp, simu_path, exe_name, f_test) #renamed the output files.

    if (os == "Windows") {
      shell(list.files()[regexpr(".bat", list.files(), fixed = T) != -1]) #run Atlantis on Windows.
    } else {
      system(paste("sh",list.files()[regexpr(".sh", list.files(), fixed = T) != -1], sep= " ")) #run Atlantis on Linux.
    }
    biom_sp <- atlantis_avbiomsp(func_grp, simu_path, exe_name, run_time) #Check if functional group collapsed.
    gc()
  }
  setwd(gwd_ini)
  return(simu_path)
}

#' Find F with the maximum catch and estimate the next F values to simulate.
#'
#' @description It looks into the output files \code{...TOTCATCH.nc} and
#'   determine the value of F (which is coded in the output files name, see
#'   \code{\link{atlantis_bachchange}}) that result on the maximum catch for the
#'   functional group considered \code{func_grp}. Then it determines the two
#'   next values of F that need to be tested.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param run_time The total duration of the simulation in days
#'   (\code{burnin_time} + 10950).
#' @param fmax The maximal value of F the simulation is going to test in y-1.
#'   \strong{WARNING:} only work if \code{fmax} < 10 y-1 (see
#'   \code{\link{atlantis_bachchange}}).
#' @return It writes on the drive, in the output folder, the file Fnext_simu.txt
#'   containing the functional group code \code{func_grp} (sp), the current F
#'   value with a maximum catch (F), and the two next F value to run (Ft1 and
#'   Ft2). It writes as well the file Fmsy_XXX.txt containing the functional
#'   group code \code{func_grp} (sp), the current F value with a maximum catch
#'   (F), and the yearly average catch over the last 5 years of simulation
#'   (yield).
#' @examples
#' atlantisfmsy_fmaxcatch("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantismain", 18250, 4)
#' atlantisfmsy_fmaxcatch("COD", "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "atlantisNew", 18250, 4)
#'
#' \dontrun{atlantisfmsy_fmaxcatch("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "Windows", "atlantismain", 18250, 10)}
#' \dontrun{atlantisfmsy_fmaxcatch("COD", "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'  "Linux", "atlantisNew", 18250, 10)}
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link[ncdf4]{nc_open}} to open netCDF4 files,
#'   \code{\link[ncdf4]{ncvar_get}} to extract variables from netCDF4 files,
#'   \code{\link[ncdf4]{nc_close}} to close netCDF4 files,
#'   \code{\link[stats]{aggregate}} to aggregate data and
#'   \code{\link[utils]{write.table}} to write table on the drive.

# Function used:
# - atlantis_paraselect (fileselect.R)

atlantisfmsy_fmaxcatch = function(func_grp, path, exe_name, run_time, fmax) {
  if (fmax >= 10) stop("You need to modify the code to use F higher than or equal to 10: problem with Atlantis output names")

  fyield <- data.frame(sp = NA, f = NA, yield = NA)

  output_path <- unlist(strsplit(atlantis_paraselect(path, exe_name, "-o"), "/"))  #search for the output directory in bach file.
  output_path <- file.path(path, output_path[-length(output_path)])
  files_name <- list.files(output_path, full.names = T)[regexpr("TOTCATCH.nc", list.files(output_path, full.names = T), fixed = T) != -1 & regexpr("MSY", list.files(output_path), fixed = T) != -1] #list of catch output files from Atlantis previous simulations.

  # setwd(paste(path, output_path, sep = "/"))
  i <- 1
  for (f in 1:length(files_name)) {
    catch.nc <- ncdf4::nc_open(files_name[f])
    time <- round(catch.nc$dim$t$vals / (3600 * 24), 0) #in days.

    catch <- ncdf4::ncvar_get(catch.nc, paste("Tot_", func_grp, "_Catch", sep = ""))  #extract total catch for the functional group considered.

    if (max(time) == run_time) {
      catch_tot <- mean(apply(catch[, time > (run_time - (365 * 5))], 2, sum)) #sum of total catch in each polygon + average catch of the 5 last years.

      if(i == 1) {
        fyield$sp <- func_grp
        fyield$f <- as.numeric(substr(tail(strsplit(files_name[f], "/")[[1]], 1), 10, 12)) / 100 #F in y-1 (extract from the file name).
        fyield$yield <- catch_tot  #total catch output.
      } else {
        fyield <- rbind(fyield, data.frame(sp = func_grp, f = as.numeric(substr(tail(strsplit(files_name[f], "/")[[1]], 1), 10, 12)) / 100, yield = catch_tot))
      }
      i <- i + 1
      ncdf4::nc_close(catch.nc)
    }
    gc()
  }

  utils::write.table(fyield, file.path(output_path, paste("Fmsy_", func_grp, ".txt", sep = "")), sep = ",", dec = ".", row.names = F) # yield for each F scenario already computed.
  fyield_final <- stats::aggregate(fyield$yield, list(sp = fyield$sp), max) #look for the maximum yield in the set of scenarios.
  names(fyield_final) <- gsub("x", "yield_max", names(fyield_final))
  fyield_ini <- merge(fyield_final, fyield)
  names(fyield_final) <- gsub("yield_max", "yield", names(fyield_final))
  fyield_final <- merge(fyield_final, fyield, x.all = T, y.all = F)

  #Dertermine the next simulation to run.
  fyield_final$pos <- which(fyield_ini$yield == fyield_ini$yield_max)
  fyield_final$f1 <- fyield$f[fyield_final$pos - 1]
  fyield_final$f2 <- fyield$f[fyield_final$pos + 1]
  fyield_final$ft1 <- round(fyield_final$f - abs(fyield_final$f1 - fyield_final$f) / 2, 2)
  fyield_final$ft2 <- round(fyield_final$f + abs(fyield_final$f2 - fyield_final$f) / 2, 2)

  fyield_final$ft2[fyield_final$ft2 >= fmax] <- NA
  fyield_ini <- fyield_final[, c("sp", "f", "ft1", "ft2")]
  utils::write.table(fyield_ini, file.path(output_path, "Fnext_simu.txt"), sep = ",", dec = ".", row.names = F)
  gc()
}
