#' Copy the calibrated Atlantis model.
#'
#' @description Create the folder AtlantisMSY in the working directory
#'   \code{folder_path} and a sub-folder named after the functional group code
#'   \code{func_grp} for the set of simulation. It copies/pastes inside it the
#'   files necessary to run Atlantis from the \code{model_path} directory.
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
#' @param batch_file The name of the batch/shell file with extension you are using
#'   to run your model. If not provided, the function will search for the unique
#'   batch file in your \code{folder_path}. \strong{Default:} NULL.
#' @return \code{simu_path} The simulation directory for the functional group
#'   \code{func_grp} considered.
#' @examples
#' atlantisfmsy_modelcopy("COD", "C:/Atlantis/AtlantisEEC",
#'                        "C:/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                        "atlantismain", "runAtlantis.bat")
#' atlantisfmsy_modelcopy("COD", "/home/Atlantis/AtlantisEEC",
#'                        "/home/Atlantis/AtlantisEEC/AtlantisEECF_v3",
#'                        "atlantisNew", "runAtlantis.sh")
#'
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_openfile}} to open a parameters file and select a
#'   parameter, \code{\link[utils]{tail}} to return the last part of an object
#'   and \code{\link[stringr]{str_split_fixed}} to split a chain of characters.
#' @export

# Function used:
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)

atlantisfmsy_modelcopy = function(func_grp, folder_path, model_path, exe_name, batch_file = NULL) {
  #check os used
  os <- Sys.info()['sysname']

  # convert path on Windows to avoid issues with space in path
  folder_path <- pathconvert(folder_path)
  model_path <- pathconvert(model_path)

  gwd_ini <- getwd()
  #create the folder to store Fmsy simulations.
  dir.create(file.path(folder_path, "AtlantisMSY"), showWarnings = FALSE)
  dir.create(file.path(folder_path, paste("AtlantisMSY", func_grp, sep = "/")), showWarnings = FALSE)

  #copy model files in the simulation directory.
  setwd(model_path)
  if (os == "Windows") {
    copy <- paste("xcopy /e /y ", shortPathName(model_path), " ", paste(shortPathName(folder_path), "AtlantisMSY", func_grp, sep = "\\"), sep = "") #copy model information.
    shell(copy)
  } else {
    copy <- paste("cp -a ", model_path, "/. ", paste(folder_path, "AtlantisMSY", func_grp, sep = "/"), "/", sep = "") #copy model information.
    system(copy)
    convert <- paste("cd ", paste(folder_path, "AtlantisMSY", func_grp, sep = "/"), "/", "\n", "flip -uv *", sep = "") #convert Windows text files into Linux text files.
    system(convert)
  }

  simu_path <- file.path(folder_path, "AtlantisMSY", func_grp)
  gc()

  #look for forcing file if they aren't in the model directory.
  forcingfile <- atlantis_paraselect(model_path, exe_name, "-f", batch_file) #prm forcing file name.

  para <- atlantis_openfile(model_path, forcingfile, ".name")
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- params[idxline] #remove comments line.
  line_para <- stringr::str_split_fixed(line_para, ".name ", 2)[, 2] #forcing file list and their associated directory.

  # Develop for California Current model: Copy forcing files that are not inside the main model directory.
  # WARNING: Only working if the forcing folder is in the parent directory of the main model directory.
  # If not please either modify this script or modify the path structure of your Atlantis forcing files.
  if(any(substring(line_para, 1, 2) == "..")){
    line_para <- line_para[substring(line_para, 1, 2) == ".."] #check if forcing input files are stored outside of the main model directory.

    if (os == "Windows") {
      forcing_folder <- unique(stringr::str_split_fixed(line_para, "\\\\", 3)[, 2]) #forcing input files folder.
    } else {
      forcing_folder <- unique(stringr::str_split_fixed(line_para, "/", 3)[, 2]) #forcing input files folder.
    }

    #Check if forcing folder already exist in the parent directory of the simulation path. ADD A SCRIPT THAT CHECK THAT ALL THE FILES ARE PRESENT IN /AtlantisMSY/forcing_folder IN CASE OF COMPUTER CRASH DURING THE COPY OF FILES.
    if (!dir.exists(file.path(folder_path, "AtlantisMSY", forcing_folder))) {
      dir.create(file.path(folder_path, "AtlantisMSY", forcing_folder), showWarnings = FALSE) #create forcing folder.
      forcing_path <- gsub(utils::tail(strsplit(model_path, "/")[[1]], 1), forcing_folder, model_path)

      # copy content of the forcing folder into the new forcing folder used for simulation.
      if (os == "Windows") {
        copy <- paste("xcopy /e /y ", shortPathName(forcing_path), " ", paste(shortPathName(folder_path), "AtlantisMSY", forcing_folder, sep = "\\"), sep = "") #copy model information.
        shell(copy)
      } else {
        copy <- paste("cp -a ", forcing_path, "/. ", paste(folder_path, "AtlantisMSY", forcing_folder, sep = "/"),"/", sep = "") #copy model information.
        system(copy)
        convert <- paste("cd ",paste(folder_path, "AtlantisMSY", forcing_folder, sep = "/"), "/", "\n", "flip -uv *", sep = "") #convert windows text files into Linux text files.
        system(convert)
      }
    }
  }
  setwd(gwd_ini)
  gc()
  return(simu_path)
}


#' Convert paths into shortform on Windows to deals with space in path
#'
#' @description Convert the path \code{path} into the shortform to deals with space
#'   issues in Windows OS path. First the function check the OS to see if it's
#'   Windows and then it check if the path isn't NULL.
#' @param path path to convert
#' @return \code{path} converted path.
#' @note This is only working if the path already exist on your machine. If not, will
#' return path with double backslashes instead of slash but keep the space in the
#' path.
#' @examples
#' pathconvert("C:/Atlantis/AtlantisEEC")
#'
#' \dontrun{pathconvert("/home/Atlantis/AtlantisEEC") # for UNIX}
#'
#' @seealso \code{\link[utils]{shortPathName}} to Convert file paths to the short
#' form.
#' @export

pathconvert <- function(path){
  if(Sys.info()['sysname'] == "Windows" & !is.null(path)) path <- utils::shortPathName(path)
  return(path)
}
