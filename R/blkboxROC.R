#' ROC plots for blkbox
#'
#' @author Zachary Davies, Boris Guennewig
#' @description will plot ROC curves for output from Performance function if "AUROC" was specified.
#' @param results The output of blkbox Performance that had "AUROC" as one of the specified metrics.
#' @param title The title of the plot. Default is "ROC".
#' @keywords ROC, blkbox
#' @importFrom methods hasArg
#' @export
blkboxROC <- function(results, title = "ROC"){
  if(hasArg(results)){
    if(names(results)[3] != "roc.values"){
      stop("ROC plots requires AUROC to be a specified metric in Performance.")
    }
  } else {
    stop("blkbox results required.")
  }

  X1 <- X2 <- Algorithm <- NULL
  plot = ggplot(results$roc.values, aes(X1, X2)) +
    geom_line(aes(colour = Algorithm)) +
    scale_x_reverse(lim=c(1,0)) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5),
          legend.key = element_blank(),
          plot.title = element_text(lineheight=.9, face="bold", size = 16)) +
    xlab("Specificity") +
    labs(fill = "Algorithms") +
    ylab("Sensitivity") +
    geom_abline(intercept=1,
                slope = 1,
                linetype="dashed",
                color = "GREY",
                size = 1) +
    ggtitle(title)

  if(length(unique(results$roc.values$Rep)) > 1){
    plot = plot + facet_grid(Rep ~ .)
  }
  return(plot)
}
