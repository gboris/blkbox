#' blkbox User Interface
#'
#' @author Zachary Davies
#' @description Invokes the shiny interface for blkbox.
#' @keywords Shiny, blkbox
#' @examples
#'\donttest{
#' blkboxUI()
#'}
#' @export
blkboxUI <- function() {
  appDir <- system.file("shiny", "blkboxUI", package = "blkbox")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `blkbox`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
