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
#' @param local_dirs Filepath to directory, which holds previous data logger
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
                              local_dirs = NULL) {

  if (is.null(userid)) {
    stop("You must enter a user id to login to the server")
  }
  if (is.null(password)) {
    stop("You must enter a password to login to the server")
  }
  if (is.null(local_dirs)) {
    readline(prompt = "You have not specified a location for local data files.
             R will attempt to download all data files from ftp.usqsoilmoisture.com.
             If this is correct, please press [enter] to continue")
  }

  path <- .get_data_path(path)
  local_dirs <- .get_data_path(local_dirs)

  JW_01 <- NULL
  JW_02 <- NULL

  # get full list of local directories
  local_dirs <- list.dirs(path = local_dirs)


  # what is the most up to date directory that exists (month)?
  latest_dir <- max(local_dirs)
  # what files for that month are present locally?
  latest_files <- list.files(latest_dir, pattern = ".csv$")

  # take only the directory name, not full path
  latest_dir <- substr(latest_dir, 44, 49)

  ftp_site <- paste0("ftp://", userid, ":",
                    password, "@ftp.usqsoilmoisture.com/public_html/data/")
  remote_dirs <- RCurl::getURL(ftp_site, ftp.use.epsv = FALSE, ftplistonly = TRUE,
                             crlf = TRUE)
  remote_dirs <- paste0(ftp_site, strsplit(remote_dirs, "\r*\n")[[1]])[-c(1:2)]

  # take only directory names, not full path
  remote_dirs <- substr(remote_dirs, 68, 73)

  # which directories do not exist locally?
  remote_dirs <- remote_dirs[remote_dirs %in% local_dirs == FALSE]

  # add the latest local directory, it may not have complete data
  remote_dirs <- c(latest_dir, remote_dirs)

  for (i in seq_len(length(remote_dirs))) {
    remote <- paste(ftp_site, remote_dirs[i], sep = "")
    csv_files <- list.files(remote, pattern = ".csv$", full.names = TRUE)

    if (i == 1) {
      csv_files <- csv_files[csv_files %in% latest_files]
    }

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
                "local_dirs"))
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
