#' @title Download, Clean and Generate Graphs From USQ Theta Probe Data
#'
#'@description This package automates downloading and cleaning data from the
#'University of Southern Queensland National Centre for Engineering in
#'Agriculture (NCEA).
#'
#'This is a slow process to retrieve all the files from the server for the first
#'run. After the first run, it will be much faster to only retrieve new files.
#'
#' @param userpwd The login and password provided by NCEA to login via file
#' transfer protocol (FTP)
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
#' \dontrun{
#' get_soil_moisture(userpwd = "userid:password", path = NULL, local_dirs = NULL)
#' }
#' @export
get_soil_moisture <- function(userpwd = NULL, path = NULL,
                              local_dirs = NULL) {

  if (is.null(userpwd)) {
    stop("You must enter a user id and password to login to the server")
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
  local_dirs <- substr(local_dirs, 45, 50)[-1]

  # what is the most up to date directory that exists (month)?
  latest_dir <- max(local_dirs)
  # what files for that month are present locally?
  latest_files <- list.files(latest_dir, pattern = ".csv$")

  ftp_site <- paste0("ftp://", userpwd, "@ftp.usqsoilmoisture.com/public_html/data/")
  remote_dirs <- RCurl::getURL(ftp_site, ftp.use.epsv = FALSE, ftplistonly = TRUE,
                               crlf = TRUE, ssl.verifypeer = FALSE)
  remote_dirs <- paste0(ftp_site, strsplit(remote_dirs, "\r*\n")[[1]])[-c(1:2)]

  # take only directory names, not full path
  remote_dirs <- substr(remote_dirs, 68, 73)

  # which directories do not exist locally?
  remote_dirs <- remote_dirs[remote_dirs %in% local_dirs == FALSE]

  # add the latest local directory, it may not have complete data
  remote_dirs <- c(latest_dir, remote_dirs)

  for (i in seq_len(length(remote_dirs))) {
    remote <- paste(ftp_site, remote_dirs[i], "/", sep = "")
    csv_files <- RCurl::getURL(remote, ftp.use.epsv = FALSE, ftplistonly = TRUE,
                               crlf = TRUE, ssl.verifypeer = FALSE)
    csv_files <- strsplit(csv_files, "\r*\n")[[1]]
    csv_files <- csv_files[grep(".csv$", csv_files)]

    if (i == 1) {
      csv_files <- csv_files[csv_files %in% latest_files == FALSE]
    }

    include_JW_01 <- grep("JW_01.", csv_files)
    JW_01 <- append(JW_01, csv_files[include_JW_01])

    include_JW_02 <- grep("JW_02.", csv_files)
    JW_02 <- append(JW_02, csv_files[include_JW_02])

    if (dir.exists(paste0(path, remote_dirs[i])) == FALSE) {
      dir.create(file.path(path, remote_dirs[i]))
    }

    con <- RCurl::getCurlHandle(ftp.use.epsv = FALSE)
    JW_01 <- sapply(paste0(remote, JW_01), function(x) try(RCurl::getURL(x, curl = con)))

    JW_02 <- sapply(paste0(remote, JW_02), function(x) try(RCurl::getURL(x, curl = con)))

    JW_01_files <- lapply(JW_01, data.frame, stringsAsFactors = FALSE)
    JW_02_files <- lapply(JW_02, data.frame, stringsAsFactors = FALSE)

    for (i in 1:length(JW_01_files)) {
      files_out <- lapply(JW_01_files[[i]], function(x) utils::read.csv(text = x, header = FALSE))
      utils::write.csv(files_out, file = paste0(path, remote_dirs[i], "/", names(JW_01[i])))
    }

    for (i in 1:length(JW_02_files)) {
      files_out <- lapply(JW_02_files[[i]], function(x) utils::read.csv(text = x, header = FALSE))
      utils::write.csv(files_out, file = paste0(path, remote_dirs[i], "/", names(JW_02[i])))
    }

    JW_01 <- list.files(paste0(path, remote_dirs[i]),
                        pattern = "JW_01[[:graph:]]+Sensors.csv",
                        full.names = TRUE)
    JW_02 <- list.files(paste0(path, remote_dirs[i]),
                        pattern = "JW_02[[:graph:]]+Sensors.csv",
                        full.names = TRUE)

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

#' @noRd
# shamelessly borrowed from RJ Hijmans Raster package
.get_data_path <- function(path) {
  path <- trimws(path)
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
