
#' @title Get Operating System Information
#'
#' @description This function checks the operating system being used and returns
#' a character string of the platform OS currently being used. Only for Windows
#' or MacOS/OSX.
#'
#' @examples
#'
#' get_os()
#'
#' @export

get_os <- function() {
  if (grepl('w|W', .Platform$OS.type)) {
    ## we are on Windows
    return("windows")
  } else {
    if (grepl('darwin', version$os)) {
      ## Mac
      return("osx")
    }
  }
}
