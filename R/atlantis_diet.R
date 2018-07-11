#' Extract availability matrix from biological parameters file
#'
#' @description Extract diet from Biological parameters file
#' and save it if wanted into a readable csv file.
#'
#' @param path The directory of the batch or shell file.
#' @param exe_name The name of the atlantis executable you used (ex:
#'   atlantismain, atlantisNew).
#' @param save.out Flag that indicate if you want to save the output or not.
#'   If \code{TRUE}, \code{output_path} need to be provided.
#'   (Default value is \code{FALSE}).
#' @param output_path The directory were you want to save the csv file
#'   (Default value is \code{NULL}).
#' @return \code{availability} Dataframe containing input availability matrix
#' from the biological parameters file.
#' @examples
#'   atlantis_dietextract("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                     "atlantismain")
#'   atlantis_dietextract("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                     "atlantisNew")
#'   atlantis_dietextract("C:/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                     "atlantismain", save.out = T, output_path =
#'                     "C:/Atlantis/AtlantisEEC/AtlantisMSY/COD/output")
#'   atlantis_dietextract("/home/Atlantis/AtlantisEEC/AtlantisMSY/COD",
#'                     "atlantisNew", save.out = T, output_path =
#'                     "/home/Atlantis/AtlantisEEC/AtlantisMSY/COD/output")
#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_openfile}} to open a parameters file and select a parameter,
#'   \code{\link[stringr]{str_split_fixed}} to split a chain of characters,
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt, .csv ...).
#' @export

# Function used:
# - atlantis_paraselect (fileselect.R)
# - atlantis_openfile (fileselect.R)

atlantis_dietextract <- function(path, exe_name, save.out = F, output_path = NULL){

  # path of csv functional groups settings
  infilename <- atlantis_paraselect(path, exe_name, "-s")

  # read functional groups settings
  fgrp_para <- utils::read.table(file.path(path, infilename), sep = ",", header = T)
  names(fgrp_para) <- tolower(names(fgrp_para))

  # list of potential prey
  prey <- as.character(fgrp_para$code)
  preysed <- paste(fgrp_para$code[fgrp_para$grouptype %in% c("LAB_DET", "REF_DET", "CARRION")], "sed", sep = "")
  prey <- c(prey, preysed)

  # path biological parameters file.
  infilename <- atlantis_paraselect(path, exe_name, "-b")

  # look for availability matrix
  para <- atlantis_openfile(path, infilename, "pPREY")
  params <- para[[1]]
  idxline <- para[[2]]

  # list of predators and their interaction with prey
  pred <- stringr::str_split_fixed(params[idxline], "[ \t\u3000]{1,}", 2)[,1]

  # number of prey
  nprey <- as.numeric(unique(stringr::str_split_fixed(params[idxline], "[ \t\u3000]{1,}", 2)[,2]))

  # test check if there no mistake in parameter files
  if(length(nprey) != 1 | length(prey) != nprey) stop(paste(infilename, ": Check the number of prey in the availability matrix", sep = ""))

  # produce table with in formation on availability matrix
  availability <- data.frame(pred = pred, stringr::str_split_fixed(params[idxline + 1], "[ \t\u3000]{1,}", nprey))
  names(availability) <- c("pred", prey)

  if(save.out) write.table(availability, file.path(output_path, paste(gsub(".prm", "",infilename), "availability.csv", sep = "_")), dec = ".", sep = ",", row.names = F)

  return(availability)
}
