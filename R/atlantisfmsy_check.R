#' Test if Atlantis modules are on.
#'
#' @description Test if at least physics, biology and fishery modules are on in
#'   the calibrated model. It looks inside the run parameters file and check if
#'   \code{flag_fisheries_on} = 1, \code{flag_skip_biol} = 0, and
#'   \code{flag_skip_phys} = 0.
#' @param model_path The directory of the calibrated model (containing all the
#'   parameters files and one bach file. Forcing files can be stored in a direct
#'   parent directory of model_path). \strong{WARNING:} Only working if the forcing
#'   folder is in the main model directory \code{model_path} or if it is in the
#'   direct parent directory. If not please either modify this package or modify
#'   the path structure of your Atlantis input forcing parameters file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @return \code{test} A binary variable (1) all the modules are on (0) at least
#'   one module is off.
#' @examples
#' atlantis_checkmodule("C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                      "atlantismain")
#' atlantis_checkmodule("/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                      "atlantisNew")
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection, and
#'   \code{\link{atlantis_openfile}} to open a parameters file and select a
#'   parameter.

# Function used:
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)

atlantis_checkmodule = function(model_path, exe_name) {

  #initialize flag check.
  test <- c(0, 0, 0) # store the value of c("flag_fisheries_on", "flag_skip_biol", "flag_skip_phys") in Atlantis.

  #looking for run parameters file.
  infilename <- atlantis_paraselect(model_path, exe_name, "-r")

  #open run parameters file and check if the fishery module is on in Atlantis.
  para <- atlantis_openfile(model_path, infilename, "flag_fisheries_on")
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  test[1] <- as.numeric(line_para[grep("[0-9]+", line_para)[1]])

  #check if the biological module is on in Atlantis.
  para <- atlantis_openfile(model_path, infilename, "flag_skip_biol")
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  test[2] <- as.numeric(line_para[grep("[0-9]+", line_para)[1]])

  #check if the biological module is on in Atlantis.
  para <- atlantis_openfile(model_path, infilename, "flag_skip_phys")
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))
  line_para <- gsub("\\t", "", line_para)
  test[3] <- as.numeric(line_para[grep("[0-9]+", line_para)[1]])

  # test if fishery is on, biology and physics aren't skipped in Atlantis.
  if(all(test == c(1, 0, 0)) == T) {
    test <- 1
  } else {
    test <- 0
  }

  gc()
  return(test)
}

#' Test if the fleets are active.
#'
#' @description Test if at least one fleet is open in Atlantis. It opens the
#'   harvest parameters file to check the value of the the parameter
#'   \code{XXX_tStart} for each fleet. If the value of \code{XXX_tStart} is
#'   inferior to \code{run_time} the fleet is considered active. It opens as
#'   well the fishing fleet .csv file to obtain the characteristic of each fleet
#'   and add a column named \code{active_flt} to store the result of the test.
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param harvest_filename The name of the harvest parameters file with its
#'   extension (ex: file.prm).
#' @param run_time The total duration of the simulation in days
#'   (\code{burnin_time} + 10950).
#' @return \code{fishing_para} A dataframe containing data from the fishing
#'   fleet csv file plus an extra column to indicate if the fleet is active (1)
#'   or not (0) named \code{active_flt}.
#' @examples
#' atlantis_fleetopen("C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                    "atlantismain", "AEEC_harvest.prm", 18250)
#' atlantis_fleetopen("/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                    "atlantisNew", "AEEC_harvest.prm", 18250)
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_openfile}} to open a parameter file and select a
#'   parameter, \code{\link[stringr]{str_split_fixed}} to split a chain of
#'   characters, \code{\link[qdapRegex]{ex_number}} to extract number from a
#'   chain of characters, and \code{\link[utils]{read.table}} to open table
#'   stored in text files (.txt, .csv ...).

# Function used:
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)

atlantis_fleetopen = function(path, exe_name, harvest_filename, run_time) {
  #open harvest file.
  para <- atlantis_openfile(path, harvest_filename, "_tStart")
  params <- para[[1]]
  idxline <- para[[2]]

  infilename <- atlantis_paraselect(path, exe_name, "-q") #looking for fishing parameters csv file.
  fishing_para <- utils::read.table(file.path(path, infilename), sep = ",", header = T)
  names(fishing_para) <- tolower(names(fishing_para))

  fleet_tstart <- stringr::str_split_fixed(params[idxline], "[:space:]", 2)
  fleet_tstart <- gsub("\\t", "", fleet_tstart)
  fleet_tstart <- data.frame(code = gsub("_tStart", "",fleet_tstart[, 1]), active_flt = do.call(rbind, qdapRegex::ex_number(fleet_tstart[, 2]))[, 1])

  fleet_tstart$active_flt <- as.numeric(as.character(fleet_tstart$active_flt))
  fleet_tstart$active_flt[fleet_tstart$active_flt <= run_time] <- 1 #if starting day is smaller than the run_time the fleet is considered open.
  fleet_tstart$active_flt[fleet_tstart$active_flt > run_time] <- 0

  fishing_para <- merge(fishing_para, fleet_tstart)
  fishing_para <- fishing_para[order(fishing_para$index), ]

  gc()
  return(fishing_para)
}

#' Test if functional groups is active.
#'
#' @description Test if the functional group \code{func_grp} is active in the
#'   calibrated model. It open the functional groups csv parameters file and
#'   look if the functional group is on.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @return \code{fgrpon} A binary variable telling if the functional group is
#'   (1) activated or not (0).
#' @examples
#' atlantis_fgrpon("COD", "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                 "atlantismain")
#' atlantis_fgrpon("COD", "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                 "atlantisNew")
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection, and
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt,
#'   .csv ...).

# Function used:
# - atlantis_paraselect (fileselect.R)

atlantis_fgrpon = function(func_grp, path, exe_name) {
  infilename <- atlantis_paraselect(path, exe_name, "-s") #looking for functional groups parameter csv file.
  fgrp_para <- utils::read.table(file.path(path, infilename), sep = ",", header = T)
  names(fgrp_para) <- tolower(names(fgrp_para))
  fgrpon <-  fgrp_para$isturned[fgrp_para$code == func_grp]

  gc()
  return(fgrpon)
}

#' Test if functional groups is fished and impacted.
#'
#' @description Test if the functional group \code{func_grp} is fished and
#'   impacted in the calibrated model. It open the functional groups csv
#'   parameters file and look if the parameter \code{IsFished} and
#'   \code{IsImpacted} are on.
#' @param path The directory of the batch or shell file.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @return \code{fgrpimp} A binary variable telling if the functional group is
#'   (1) fished and impacted or not (0).
#' @examples
#' atlantis_fgrpimp("COD", "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                  "atlantismain")
#' atlantis_fgrpimp("COD", "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                  "atlantisNew")
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection, and
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt,
#'   .csv ...).

# Function used:
# - atlantis_paraselect (fileselect.R)

atlantis_fgrpimp = function(func_grp, path, exe_name) {

  infilename <- atlantis_paraselect(path, exe_name, "-s") #looking for functional groups parameter csv file.
  fgrp_para <- utils::read.table(file.path(path, infilename), sep = ",", header = T)
  names(fgrp_para) <- tolower(names(fgrp_para))
  fgrpimp <-  fgrp_para$isfished[fgrp_para$code == func_grp] * fgrp_para$isimpacted[fgrp_para$code == func_grp]

  gc()
  return(fgrpimp)
}

#' Test if model uses fishing mortality.
#'
#' @description Test if the active fleet uses the fishing mortality parameter in
#'   Atlantis. Check in the harvest parameters file if the parameter
#'   \code{XXX_effortmodel} is equal to 0 (Fishing mortality option) for each
#'   active fleet.
#' @param path The directory of the batch or shell file.
#' @param harvest_filename The name of the harvest parameters file with its
#'   extension (ex: file.prm).
#' @param fishing_para A dataframe containing data from the fishing fleet csv
#'   file plus an extra column to indicate if the fleet is active (1) or not (0)
#'   named \code{active_flt} (output from the function
#'   \code{\link{atlantis_fleetopen}}).
#' @return \code{fishing_para} The input dataframe \code{fishing_para} with an
#'   extra column named \code{effortmodel} containing the effortmodel option
#'   used for each fleet.
#' @examples
#' atlantis_checkf("C:/Atlantis/AtlantisEEC/AtlantisEECF_v3", "AEEC_harvest.prm",
#'                 fishing_para)
#' atlantis_checkf("/home/Atlantis/AtlantisEEC/AtlantisEECF_v3", "AEEC_harvest.prm",
#'                 fishing_para)
#' @seealso \code{\link{atlantis_openfile}} to open a parameters file and select
#'   a parameter, \code{\link[stringr]{str_split_fixed}} to split a chain of
#'   characters, and \code{\link[qdapRegex]{ex_number}} to extract number from a
#'   chain of characters.

# Function used:
# - atlantis_openfile (fileselect.R)

atlantis_checkf = function(path, harvest_filename, fishing_para) {
  #check if Atlantis effortmodel parameters different from fishing mortality option (option 0) for open fleet.
  #open harvest file.
  para <- atlantis_openfile(path, harvest_filename, "_effortmodel")
  params <- para[[1]]
  idxline <- para[[2]]

  fleet_effortmodel <- stringr::str_split_fixed(params[idxline], "[:space:]", 2)
  fleet_effortmodel <- gsub("\\t", "", fleet_effortmodel)
  fleet_effortmodel <- data.frame(code = gsub("_effortmodel", "", fleet_effortmodel[, 1]), effortmodel = do.call(rbind, qdapRegex::ex_number(fleet_effortmodel[, 2]))[, 1])

  fleet_effortmodel$effortmodel <- as.numeric(as.character(fleet_effortmodel$effortmodel))
  fishing_para <- merge(fishing_para, fleet_effortmodel)
  fishing_para <- fishing_para[order(fishing_para$index), ]

  gc()
  return(fishing_para)
}

#' Test if the functional group biomass collapsed.
#'
#' @description It opens the output file \code{BiomIndx.txt} and calculates the
#'   average biomass of the selected functional group \code{func_grp} over the 5
#'   last years of simulation. If the average biomass is smaller than 0.1 metric
#'   tons, the stock is considered collapsed and it returns 0.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param run_time The total duration of the simulation in days
#'   (\code{burnin_time} + 10950).
#' @return \code{biom_sp} The average biomass of the 5 last years of simulation.
#'   Return 0 if \code{biom_sp} < 0.1.
#' @examples
#' atlantis_avbiomsp("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                   "atlantismain", 18250)
#' atlantis_avbiomsp("COD", "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                   "atlantisNew", 18250)
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection, and
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt,
#'   .csv ...).

# Function used:
# - atlantis_paraselect (fileselect.R)

atlantis_avbiomsp = function(func_grp, path, exe_name, run_time) {
  output_path <- atlantis_paraselect(path, exe_name, "-o")
  output_path <- gsub(".nc", "BiomIndx.txt", output_path)

  biom_sp <- utils::read.table(file.path(path, output_path), dec = ".", header = T)
  names(biom_sp) <- toupper(names(biom_sp))
  biom_sp <- mean(biom_sp[biom_sp$TIME > (run_time - (5 * 365)), func_grp]) #calculate average biomass on the last 5 years of simulation.
  if(biom_sp < 0.1) {
    biom_sp <- 0
  }
  gc()
  return(biom_sp)
}

#' Test Fmsy simulation completion.
#'
#' @description It determines the F with the maximum cacth in the file
#'   \code{Fmsy_XXX.txt} and compares it to the two values of F planned for the
#'   next round of simulation stored in the file \code{Fnext_simu.txt}. If all
#'   the value are the same the Fmsy for the functional group \code{func_grp}
#'   has been found.
#' @param func_grp The code of the Atlantis functional group for which Fmsy will
#'   be estimated.
#' @param output_path The directory of the output files.
#' @return \code{end} A variable with three posibilities: (0) Fmsy isn't
#'   reached, (1) Fmsy is reached, and (2) no Fmsy have been found so fmax need
#'   to be increased.
#' @examples
#' atlantisfmsy_completion("COD", "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD/output")
#' atlantisfmsy_completion("COD", "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD/output")
#' @seealso \code{\link[utils]{read.table}} to open table stored in text files
#'   (.txt, .csv ...).

atlantisfmsy_completion = function(func_grp, output_path) {

  fyield_final <- utils::read.table(file.path(output_path, "Fnext_simu.txt"), sep = ",", dec = ".", header = T)
  fyield <- utils::read.table(file.path(output_path, paste("Fmsy_", func_grp, ".txt", sep = "")), sep = ",", dec = ".", header = T)

  if(fyield_final$f == max(fyield$f)) {
    end <- 2
    print("You need to increase fmax value. FMSY not reached during the simulation process.")
    gc()
    return(end)
  }

  fmsy_ini <- fyield$f[fyield$yield == max(fyield$yield)]
  if((fyield_final$ft1 == fmsy_ini & fyield_final$ft2 == fmsy_ini) | all(c(fyield_final$ft1, fyield_final$ft2) %in% fyield$f)) {
    end <- 1
    gc()
    return(end)
  } else {
    end <- 0
    gc()
    return(end)
  }
}
