#' Plot catches as a function of the fishing mortality
#'
#' @description Use the Fmsy simulation to plot the catches of a given functional
#' group as a function of the fishing mortality. On this figure, the Fmsy value is
#' displayed as well as the current F value used in the calibrated model.
#' @section Warning:
#' If the Fmsy simulation is not completed the function return an
#' error message.
#'
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
#' @param compareFsq If TRUE compare Fmsy value with Fsq (status-quo) the fishing
#'   mortality applied in the calibrated model. A dashed line is drawn to mark the
#'   position of Fsq and it's value is displayed with a precision of two decimals.
#'   \strong{Default:} FALSE.
#' @param compareYFsq If TRUE in addition to compare Fmsy and Fsq, the yield at Fmsy
#'   and Fsq is displayed with dashed line and the yield (here Landing) values is
#'   diplayed with one decimal precision. \strong{Default:} FALSE. \strong{WARNING:}
#'   If TRUE you need to provide the parameter \code{Fsq_dir} or \code{YFsq} described
#'   below. If no value is provided for those two parameters the function will look
#'   into the output directory of your calibrated model to find the yield (Landing) in
#'   the Catch.txt file. This imply that you had previously run the calibrated model
#'   for at least the \code{burnin_time} + 30 years and that you stored fishery
#'   summary output with 365 days periodicity.
#' @param Fsq_dir If \code{compareYFsq} is TRUE. You can provide here the fool path
#'   (directory + file names + extension) to the Catch.txt produced by the calibrated
#'   model following the requirement described above. \strong{Default:} NULL.
#' @param YFsq If \code{compareYFsq} is TRUE. You can provide here the average yearly
#'   yield (Landing) in tons over the period 55-60 years of the functional group
#' \code{func_grp} produced by the calibrated model following the requirement
#'   described previously. \strong{Default:} NULL.
#' @param shading If TRUE shade the area gain (in green) or lost (in red) by moving
#'   from Fsq to Fmsy. Only applied if \code{compareYFsq} is TRUE. \strong{Default:}
#'   FALSE.
#' @param perc If TRUE display the percentage of change between Fsq and Fmsy and/or
#'   yield at Fsq and Fmsy. \strong{Default:} FALSE.
#' @param addarrow If TRUE display arrow between Fsq and Fmsy and Yield at Fsq and
#'   Fmsy. \strong{Default:} FALSE. Only displayed if \code{perc} is TRUE.
#' @return \code{gt} A TableGrod object ("gtable") plotting the yield (Landing) as a
#'   function of the  fishing mortality and plot the figure.
#' @examples

#' @seealso \code{\link{atlantis_paraselect}} for parameters file selection,
#'   \code{\link{atlantis_openfile}} to open a parameters file and selecta parameter,
#'   \code{\link{atlantis_fleetopen}} to check if fleets are active,
#'   \code{\link{atlantis_checkf}} to check the formulation of the fishing pressure
#'   used (i.e. F, effort, dynamics model...), \code{\link{atlantis_fdistri}} to
#'   extract the distribution of fishing pressure amongst fleets,
#'   \code{\link[utils]{read.table}} to open table stored in text files (.txt,
#'   .csv ...), \url{https://ggplot2.tidyverse.org/} for more information on the
#'   package \code{ggplot2} and
#'   \url{https://www.rdocumentation.org/packages/grid/versions/3.5.1} for additional
#'   inforamtion on the package \code{grid}.
#' @export

# Function used:
# - atlantis_openfile (fileselect.R)
# - atlantis_paraselect (fileselect.R)
# - atlantis_fleetopen (check.R)
# - atlantis_checkf (fileselect.R)
# - atlantis_fdistri (atlantisfmsy_ini.R)

atlantisfmsy_plot <- function(func_grp, folder_path, model_path, exe_name, fmax, compareFsq = F, compareYFsq = F, Fsq_dir = NULL, YFsq = NULL, shading = F, perc = F, addarrow = F){
  # convert path on Windows to avoid issues with space in path
  folder_path <- pathconvert(folder_path)
  model_path <- pathconvert(model_path)

  # Check os used.
  os <- Sys.info()['sysname']

  if(!os %in% c("Windows","Linux"))
    stop("This package is not developped to work on iOS. Several modifications are required to copy files and directories, and run Atlantis.")

  if(!dir.exists(file.path(folder_path, "AtlantisMSY", func_grp)))
    stop(paste0("Simulation directory does not exist. You have not run the simulation for the functional group: ", func_grp,". Please run atlantisfmsy_simu."))

  simu_path <- file.path(folder_path, "AtlantisMSY", func_grp)

  # Get output_path
  output_path <- unlist(strsplit(atlantis_paraselect(simu_path, exe_name, "-o"), "/"))  #search for the output directory in bach file.
  output_path <- file.path(simu_path, output_path[-length(output_path)])

  # Check for completion of the Fmsy simulation
  if(atlantisfmsy_completion(func_grp, output_path)!=1)
    stop(paste0("You did not finished Fmsy simulation for the functional group: ", func_grp,". Please run atlantisfmy_simurestart."))

  if(compareFsq | compareYFsq) {  # path running parameters file.
    infilename <- atlantis_paraselect(simu_path, exe_name, "-r")

    # Extract running time from previous run.
    para <- atlantis_openfile(simu_path, infilename, "tstop")
    params <- para[[1]]
    idxline <- para[[2]]

    line_para <- unlist(strsplit(params[idxline], " "))
    line_para <- gsub("\\t", "", line_para)
    run_time <- as.numeric(line_para[grep("[0-9]+", line_para)[1]])

    # look for open fleets and and check if they are using fishing mortality as proxy of fishing pressure
    harvest_filename <- atlantis_paraselect(model_path, exe_name, "-h") #looking for harvest parameters file.
    fishing_para <- atlantis_fleetopen(model_path, exe_name, harvest_filename, run_time) #check if fleets turn on.
    if(sum(fishing_para$active_flt) == 0)
      stop("No fleet is active in your model please change your harvest parameters file.")

    fishing_para <- atlantis_checkf(model_path, harvest_filename, fishing_para) #check if model is calibrated with fishing mortality.
    if(sum(fishing_para$active_flt * fishing_para$effortmodel) != 0)
      stop("Your Atlantis model is not calibrated with fishing mortality (efffortmodel = 0) for open fleets.")

    # Extract F values in the calibrated model
    Fsq <- sum(atlantis_fdistri(func_grp, model_path, harvest_filename, fishing_para, raw.distri = T)) * 365
  }


  # Get the Fmsy simulation output path
  Fmsy_path <- file.path(output_path, paste0("Fmsy_", func_grp, ".txt"))
  Fmsy_table <- utils::read.table(Fmsy_path, sep = ",", dec = ".", header = T)
  Fmsy <- Fmsy_table$f[Fmsy_table$yield == max(Fmsy_table$yield)]

  x_epsi <- y_epsi <- 0

  if(compareYFsq) {
    # Get landing at Fsq
    if(is.null(YFsq)) {
      if(is.null(Fsq_dir)){
        output_path <- unlist(strsplit(atlantis_paraselect(model_path, exe_name, "-o"), "/"))
        file_name <- gsub(".nc", "Catch.txt", output_path)[2]
        output_path <- file.path(model_path, output_path[-length(output_path)], file_name)
      } else {
        output_path <- Fsq_dir
      }

      Landing_Fsq <- utils::read.table(output_path, sep = " ", dec = ".", header = T)
      Landing_Fsq <- mean(Landing_Fsq[Landing_Fsq$Time <= run_time & Landing_Fsq$Time >= run_time-(5*365), func_grp])
    } else {
      Landing_Fsq <- YFsq
    }

    # add Fsq to the table
    Fsq_table <- data.frame(sp = func_grp, f = Fsq, yield = Landing_Fsq)
    Fmsy_table <- rbind(Fmsy_table, Fsq_table)

    if(shading){
      shade <- subset(subset(Fmsy_table[, c("f", "yield")], f >= min(Fmsy, Fsq)), f <= max(Fmsy, Fsq))

      if(min(Fmsy, Fsq) == Fsq){
        minLand <- as.data.frame(rbind(c(Fsq,0), c(Fsq, Landing_Fsq)))
        maxLand <- as.data.frame(rbind(c(Fmsy, max(Fmsy_table$yield)), c(Fmsy, 0)))
        shade_color <- "#32CD32"
      } else {
        minLand <- as.data.frame(rbind(c(Fmsy,0), c(Fmsy, max(Fmsy_table$yield))))
        maxLand <- as.data.frame(rbind(c(Fsq, Landing_Fsq), c(Fsq, 0)))
        shade_color <- "#FF4040"
      }
      names(minLand) <- names(maxLand) <- names(shade)
      shade <- rbind(minLand, shade, maxLand)
    }

    # deal with issue of label overlap
    if(abs(Landing_Fsq-max(Fmsy_table$yield))/(1.03*max(Fmsy_table$yield)) <= 0.04)  y_epsi <- 0.016

    if(abs(Fsq-Fmsy)/max(Fmsy_table$f) <= 0.023) {
      x_epsi <- 0.01
      if(Fsq > Fmsy) x_epsi <- -x_epsi
    }

    lab <- data.frame(x = c(0.90* max(Fmsy_table$f), 0.90* max(Fmsy_table$f)), y = c(round(Landing_Fsq, 1), round(max(Fmsy_table$yield), 1)), val = c(round(Landing_Fsq, 1), round(max(Fmsy_table$yield), 1)))
  } else {
    lab <- data.frame(x = 0.90* max(Fmsy_table$f), y = round(max(Fmsy_table$yield), 1), val = round(max(Fmsy_table$yield), 1))
  }



  p <- ggplot2::ggplot(data = Fmsy_table, ggplot2::aes(x = f, y = yield)) + ggplot2::geom_line()
  if(compareYFsq & shading) p <- p + ggplot2::geom_polygon(data = shade, ggplot2::aes(x = f, y = yield, fill = shade_color)) + ggplot2::scale_color_manual(values = shade_color) + ggplot2::scale_fill_manual(values = shade_color)
  p <- p + ggplot2::geom_vline(xintercept = Fmsy, linetype="dashed")
  if(compareFsq | compareYFsq)   p <- p + ggplot2::geom_vline(xintercept = Fsq, linetype="dashed")
  if(compareYFsq) p <- p + ggplot2::geom_hline(yintercept = Landing_Fsq, linetype="dashed")
  p <- p + ggplot2::geom_hline(yintercept = round(max(Fmsy_table$yield), 1), linetype="dashed") +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(expand = c(0, 0)) + ggplot2::scale_y_continuous(expand = c(0, 0), limits = c(0, 1.03*max(Fmsy_table$yield))) +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.line = ggplot2::element_line(colour = "black"),
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   panel.background = ggplot2::element_blank(),
                   plot.margin = ggplot2::unit(c(2.2, 0.5, 0.2, 0.2), "cm"),
                   legend.position = "none",
                   plot.title = ggplot2::element_text(hjust = 0.5, vjust = 1, size = 20, face = "bold")) +
    ggplot2::geom_label(data = lab, ggplot2::aes(x = x, y = y, label = paste0(val," T")), colour = NA)
  if(compareYFsq) p <- p + ggplot2::annotate("text", x = 0.90 * max(Fmsy_table$f), y = Landing_Fsq*(1-y_epsi), colour = "black", size = 4,  label = paste0(round(Landing_Fsq, 1), " T"))
  p <- p + ggplot2::annotate("text", x = 0.90 * max(Fmsy_table$f), y = max(Fmsy_table$yield)*(1+y_epsi), colour = "black", size = 4,  label = paste0(round(max(Fmsy_table$yield), 1), " T"))

  if((perc | addarrow) & (compareFsq | compareYFsq)){
    if(Fsq != 0) {
      p <- p + ggplot2::annotate("text", x = ((Fmsy + Fsq)/2) + 2.5 * max(Fmsy_table$f) * abs(x_epsi) , y = 0.1 * max(Fmsy_table$yield), angle = 90, colour = "black", size = 4,  label = paste0(round(100 * (Fmsy - Fsq)/ Fsq), " %"), fontface = "bold")
      if(compareYFsq) if(Landing_Fsq != 0) p <- p + ggplot2::annotate("text", x = 0.90 * max(Fmsy_table$f), y = ((max(Fmsy_table$yield) + Landing_Fsq)/2)*(1-5*y_epsi), colour = "black", size = 4,  label = paste0(round(100 * (max(Fmsy_table$yield) - Landing_Fsq)/ Landing_Fsq), " %"), fontface = "bold")
      if(addarrow & (x_epsi==0 | y_epsi == 0)){
        if(x_epsi == 0 & y_epsi == 0){
          Fmsy_arrow <- data.frame(x = c(Fsq, 0.80 * max(Fmsy_table$f)), y = c(0.02 * max(Fmsy_table$yield), Landing_Fsq), vx = c(Fmsy-Fsq, 0), vy = c(0, max(Fmsy_table$yield) - Landing_Fsq))
        } else {
          if(x_epsi == 0){
            Fmsy_arrow <- data.frame(x = Fsq, y = 0.02 * max(Fmsy_table$yield), vx = Fmsy-Fsq, vy = 0)
          } else {
            Fmsy_arrow <- data.frame(x = 0.80 * max(Fmsy_table$f), y = Landing_Fsq, vx = 0, vy = max(Fmsy_table$yield) - Landing_Fsq)
          }
        }

        p <- p + ggplot2::geom_segment(data = Fmsy_arrow, ggplot2::aes(x = x, y =  y, xend = x + vx, yend = y + vy), size = 1, arrow = ggplot2::arrow(length = ggplot2::unit(2.5,"mm"), type='closed'))
      }
    }
  }

  p <- p + ggplot2::xlab(bquote('F ('*year^-1*')')) + ggplot2::ylab("Yield (Landing in tons)") + ggplot2::ggtitle(func_grp)

  if(compareFsq | compareYFsq){
    p <- p + ggplot2::annotation_custom(grob = grid::textGrob(label = paste0("Fsq = ", signif(Fsq, 2)), rot = 90, gp = grid::gpar(fontsize = 10)),
                                        ymin = max(Fmsy_table$yield) * 1.14,
                                        ymax = max(Fmsy_table$yield)* 1.14,
                                        xmin = Fsq - max(Fmsy_table$f)*x_epsi,
                                        xmax = Fsq - max(Fmsy_table$f)*x_epsi)
  }

  p <- p + ggplot2::annotation_custom(grob = grid::textGrob(label = paste0("Fmsy = ", round(Fmsy, 2)), rot = 90, gp = grid::gpar(fontsize = 10)),
                                      ymin = max(Fmsy_table$yield) * 1.14,
                                      ymax = max(Fmsy_table$yield)* 1.14,
                                      xmin = Fmsy + max(Fmsy_table$f)*x_epsi,
                                      xmax = Fmsy + max(Fmsy_table$f)*x_epsi)

  # Code to override clipping
  gt <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(p))
  gt$layout$clip[gt$layout$name == "panel"] <- "off"

  # plot(gt) # issue with plot in a loop if you store file in pdf draw an empty page before starting drawing the plot
  grid::grid.draw(gt) # if used and atlantisfmsy_plot is used in a loop or if you want to draw several species, the function grid::grid.newpage() need to be included in your loop to avoid overwriting the same plot
  return(gt)
}
