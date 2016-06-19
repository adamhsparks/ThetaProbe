#' @title Download, Clean and Generate Graphs From USQ Theta Probe Data
#'
#'@description This package automates downloading and cleaning data from the
#'University of Southern Queensland National Centre for Engineering in
#'Agriculture (NCEA).
#'
#'This is a slow process to retrieve all the files from the server for the first
#'run. After the first run, it will be much faster to only retrieve new files.
#'
#' @param userid The login provided by NCEA to login via file transfer protocol
#' (FTP)
#' @param password The password provided by NCEA to login via FTP
#' @param path Filepath to directory for saving a comma separated file (CSV)
#' output. Defaults to current working directory.
#' @param local_files Filepath to directory, which holds previous data logger
#' data monthly file folders, which contain hourly CSV files from loggers.
#'
#' @details This function will download CSV files from the server that
#' are not currently on the local machine in the user specified location and
#' coallate them into one dataframe named "soil_moisture" and saves a CSV file
#' to disk. If there are no existing data files locally, the function will
#' attempt to retrieve all files from the server.
#'
#' @examples
#' # NULL
#' @export
get_soil_moisture <- function(userid = NULL, password = NULL, path = NULL,
                              local_files = NULL) {

  if (is.null(userid)) {
    stop("You must enter a user id to login to the server")
  }
  if (is.null(password)) {
    stop("You must enter a password to login to the server")
  }
  if (is.null(local_files)) {
    readline(prompt = "You have not specified a location for local data files.
             R will attempt to download all data files from ftp.usqsoilmoisture.com.
             If this is correct, please press [enter] to continue")
  }

  path <- .get_data_path(path)
  local_files <- .get_data_path(local_files)
  JW_01 <- NULL
  JW_02 <- NULL

  local_dirs <- list.dirs(path = local_files)

  ftp_site <- paste0("ftp://", userid, ":",
                    password, "@ftp.usqsoilmoisture.com/public_html/data/")
  filenames <- RCurl::getURL(ftp_site, ftp.use.epsv = FALSE, ftplistonly = TRUE,
                             crlf = TRUE)
  filenames <- paste0(ftp_site, strsplit(filenames, "\r*\n")[[1]])[-c(1:2)]

  filenames <- filenames[filenames %in% local_dirs == FALSE, ]

  for (i in seq_len(length(filenames))) {
    csv_files <- list.files(filenames, pattern = ".csv$", full.names = TRUE)


    include_JW_01 <- grep("JW_01.", csv_files)
    JW_01 <- append(JW_01, csv_files[include_JW_01])

    include_JW_02 <- grep("JW_02.", csv_files)
    JW_02 <- append(JW_02, csv_files[include_JW_02])

    soil_moisture_JW_01 <- data.table::rbindlist(lapply(JW_01,
                                                        data.table::fread,
                                                        header = FALSE,
                                                        select = c(1:3)))
    soil_moisture_JW_01$Sensor <- rep("JW_01",
                                      length(soil_moisture_JW_01[, 1]))

    soil_moisture_JW_02 <- data.table::rbindlist(lapply(JW_02,
                                                        data.table::fread,
                                                        header = FALSE,
                                                        select = c(1:3)))
    soil_moisture_JW_02$Sensor <- rep("JW_02",
                                      length(soil_moisture_JW_02[, 1]))
    soil_moisture <- rbind(soil_moisture_JW_01, soil_moisture_JW_02)
    names(soil_moisture) <- c("Date", "Time", "Moisture", "Sensor")

    readr::write_csv(soil_moisture, paste0(path, "/", Sys.Date(),
                                           "_Soil_Moisture.csv"), append = TRUE)

    rm(list = c("include_JW_01", "include_JW_02", "csv_files",
                "soil_moisture_JW_01", "soil_moisture_JW_02", "soil_moisture",
                "local_files"))
    JW_01 <- NULL
    JW_02 <- NULL
  }
}

# shamelessly borrowed from RJ Hijmans Raster package
.get_data_path <- function(path) {
  path <- raster::trim(path)
  if (is.null(path)) {
    path <- getwd()
  } else {
    if (substr(path, nchar(path) - 1, nchar(path)) == "//") {
      p <- substr(path, 1, nchar(path) - 2)
    } else if (substr(path, nchar(path), nchar(path)) == "/" |
               substr(path, nchar(path), nchar(path)) == "\\") {
      p <- substr(path, 1, nchar(path) - 1)
    } else {
      p <- path
    }
    if (!file.exists(p) & !file.exists(path)) {
      stop("File path does not exist: ", path)
    }
  }
  if (substr(path, nchar(path), nchar(path)) != "/" &
      substr(path, nchar(path), nchar(path)) != "\\") {
    path <- paste0(path, "/")
  }
  return(path)
}
