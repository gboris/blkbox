#' blkbox paritioning
#'
#' @author Zachary Davies, Boris Guennewig
#' @description Prepares data for standard training and testing, data will be split into training and holdout set and output in a list which can be directly supplied to blkbox.
#' @param data A data.frame of the data. Rows represent samples and columns features.
#' @param labels The labels corresponding to the data, order must match with order of rows in data.
#' @param size determines the size of the holdout data, must be a numeric value between 0 and 1 that. Default is 0.8.
#' @param seed Determines the seed used to randomly sample the data by row.
#' @keywords blkbox, partition
#' @export
Partition <- function(data, labels, size, seed){

  if(!hasArg(data) || !hasArg(labels)){
    stop("Missing data or labels")
  }

  if(!hasArg(size)){
    size = 0.8
  }

  if(!hasArg(seed)){
    seed = sample(1:1000, 1)
  }
  set.seed(seed)
  message("Seed:", seed)

  new_order = sample(1:nrow(data), nrow(data))
  data = data[new_order, ]
  labels = labels[new_order]

  training.num = round(nrow(data)*size)

  training.data = data[1:training.num, ]
  training.labels = labels[1:training.num]
  # Holdout Data & Labels
  holdout.data = data[training.num:nrow(data), ]
  holdout.labels = labels[training.num:nrow(data)]

  return(list("training.data" = training.data, "holdout.data" = holdout.data, "training.labels" = training.labels, "holdout.labels" = holdout.labels, options = list(size, seed)))
}

