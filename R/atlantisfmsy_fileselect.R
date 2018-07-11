#' Select Atlantis parameters file.
#'
#' @description It looks for the names of the parameters file wanted within the
#'   batch or shell file used to run Atlantis.
#' @param path The directory of the batch or shell file.
#' @param os The operating system used (ex:"Windows" or "Linux"). \strong{WARNING:}
#'   At the moment, the package is not designed to run on OSX (see
#'   \code{\link{atlantisfmsy_modelcopy}}, and
#'   \code{\link{atlantis_bachchange}}.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param index The index used by Atlantis to determine the type of the
#'   different parameters files in the batch/shell file (ex: -i initial
#'   condition file; -o output files; -r run file; -f forcing file; -p physics
#'   file; -b biology file; -s functional groups csv file; -h fishing file; -q
#'   fisheries csv file...).
#' @return \code{infilename} The name of the parameters file selected.
#' @examples
#' atlantis_paraselect("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                     "atlantismain", "-h", "Windows")
#' atlantis_paraselect("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                     "atlantisNew", "-h", "Linux")
#'
#' \dontrun{atlantis_paraselect("/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                              "atlantismain", "-h", "Darwin") # for OSX.}
#' @export

atlantis_paraselect = function(path, exe_name, index, os = Sys.info()['sysname']) {
  #find the Atlantis run bach file
  if (os == "Windows") {
    infilename <- list.files(path)[regexpr(".bat", list.files(path), fixed = T) != -1] #Windows
  } else {
    infilename <- list.files(path)[regexpr(".sh", list.files(path), fixed = T) != -1] #Linux
  }

  para <- atlantis_openfile(path, infilename, exe_name)
  params <- para[[1]]
  idxline <- para[[2]]

  line_para <- unlist(strsplit(params[idxline], " "))

  infilename <- line_para[which(line_para == index) + 1]
  if( index == "-o" & length(which(line_para == "-d")) != 0){
    infilename <- paste(line_para[which(line_para == "-d") + 1], infilename, sep = "/")
  }
  gc()
  return(infilename)
}

#' Open parameters file and look for a specific parameter.
#'
#' @description It opens a parameters file, removes comment lines and finds the
#'   line ID of the parameter looked for.
#' @param path The directory of the parameters file.
#' @param file_names The name of the parameters file with its extension (ex:
#'   file.txt or file.prm).
#' @param para_name The name of the parameter the function will look for.
#' @return a list containing: the open parameters file and the line ID (location
#'   of the parameter in the file).
#' @examples
#' atlantis_openfile("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD", "AEEC35_run.prm", "tstop")
#' atlantis_openfile("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD", "AEEC35_run.prm", "tstop")
#' @export

atlantis_openfile = function(path, file_names, para_name) {
  infilecon <- file(description = file.path(path, file_names), open = "r+")
  on.exit(close(infilecon))

  params <- readLines(con = infilecon, warn = F)
  idxline <- grep(para_name, params)

  if(length(idxline) != 1) {
    idxline <- idxline[which(substring(params[idxline], 1, 1) != "#")]
  }
  gc()
  return(list(params, idxline))
}
