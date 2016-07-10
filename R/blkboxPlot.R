#' Nested Crossfold Validation Performance Plot.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Compares the performance of each algorithm in a boxplot. Each holdout will contribute at least one data point to each algorithms boxplot.
#' @param obj An object produced by the blkboxCV function.
#' @param metric Which metric you wish to plot, can only plot those specified to the blkboxNCV function at time of running. Area under the Receiver operating curve = "AUROC", Accuracy = "ACC", Error rate = "ERR", Matthews correlation coefficient = "MCC", F-1 score = "F-1". Default is the first metric specified to your NCV arguments vector.
#' @param y_ranges is the y axis limits for the plot, defaults to c(0,1). Must be a numeric vector with two entries.
#' @param title the title to be adhered to the plot. Default is no title.
#' @keywords NCV, Plot, ggplot2, boxplot
#' @export
ncv.plot <- function(obj, metric, y_ranges, title){

  #ONLY SUPPORTS 1 ALGORITHM FEATURE SELECTION CURRENTLY

  if (!hasArg(metric)){
    metric = unique(obj$HoldoutPerf$Metric)[1]
  }

  if (!hasArg(y_ranges)){
    y_ranges = c(0, 1)
  }

  if (!hasArg(title)){
    title = ""
  }

  Algorithm <- Metric <- NULL
  data = obj$HoldoutPerf %>% dplyr::filter(Metric == metric)
  ggplot(data, aes(x = factor(Algorithm), y = Performance, fill = Algorithm)) + geom_boxplot() +  theme_bw() + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), legend.key = element_blank(), plot.title = element_text(lineheight = .9, face = "bold", size = 16)) + xlab("Algorithms") + ylab(paste(metric)) + ylim(y_ranges) + geom_hline(yintercept = 0.5,  linetype = "dotted", size = 1) + ggtitle(paste(title))

}


#' Crossfold Validation Performance Plot.
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Compares the performance of each algorithm in a boxplot OR barplot. Each holdout will contribute at least one data point to each algorithm.
#' @param obj An object produced by the blkboxCV function.
#' @param metric Which metric you wish to plot. Area under the Receiver operating curve = "AUROC", Accuracy = "ACC", Error rate = "ERR", Matthews correlation coefficient = "MCC", F-1 score = "F-1". default = c("AUROC")
#' @param y_ranges is the y axis limits for the plot, defaults to c(0,1). Must be a numeric vector with two entries. Invalid for barplots.
#' @param title the title to be adhered to the plot. Default is no title.
#' @param type The plot can be either a barplot or boxplot. For the barplot the consensus performance is used, for a boxplot consensus is false. If only one performance measure is found for each algorithm then it will be forced to a barplot. default = "boxplot", unless data is unsupported.
#' @keywords CV, Plot, ggplot2, boxplot, barplot
#' @importFrom methods hasArg
#' @export
cv.plot <- function(obj, metric = "AUROC", y_ranges = c(0, 1), title = "", type = "boxplot"){

  repeats = dim(obj$algorithm.votes[[1]])[1]

  if (!( (type == "boxplot") | (type == "barplot"))){
    stop("Invalid plot type.")
  }

  if (type == "boxplot"){
   obj = Performance(obj, consensus = F, metric = metric)
  } else {
   obj = Performance(obj, consensus = T, metric = metric)
  }

  if (length(obj$Performance[[1]][[1]]) == 1){
    plot.type = "barplot"
    dim_h = 1
    if (plot.type != type){
      message("type of plot was changed to barplot. Not enough data points.")
      plot.type = "barplot"
    }
  } else {
    plot.type = "boxplot"
    dim_h = repeats
  }

  algs = names(obj$Performance[[metric[1]]])
  values = NULL

  c = 0
  for (a in algs){
    for (i in 1:repeats){
      c = c + 1
      values[c] = obj$Performance[[metric]][[a]]
    }
  }

  df <- data.frame(matrix(values, ncol = length(algs)))
  colnames(df) <- algs
  df_melt <- reshape2::melt(df)
  variable <- value <- NULL

  x <- ggplot(df_melt,
              aes(x = factor(variable),
                  y = value,
                  fill = variable)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90,hjust = 1,vjust = 0.5),
          legend.key = element_blank(),
          plot.title = element_text(lineheight = .9, face = "bold", size = 16)) +
    xlab("Algorithms") +
    ylab(paste(metric)) +
    ylim(y_ranges) +
    geom_hline(yintercept = 0.5,
               linetype = "dotted",
               size = 1) +
    ggtitle(paste(title))


  if (plot.type == "boxplot"){
    x <- x + geom_boxplot()
  } else {
    x <- + geom_bar(stat = "identity") + labs(fill = "Algorithms")
  }

  return(list(plot.data = df_melt, plot = x))
}

