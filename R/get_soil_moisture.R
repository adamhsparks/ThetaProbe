#' @title Download, Clean and Generate Graphs From USQ Theta Probe Data
#'
#'@description This package automates downloading and cleaning data from the
#'University of Southern Queensland National Centre for Engineering in
#'Agriculture (NCEA).
#'
#'This is a slow process to retrieve all the files from the server.
#'
#' @param userid The login provided by NCEA to login via FTP
#' @param password The password provided by NCEA to login via FTP
#' @param path Filepath to directory for saving .csv file output. Defaults to
#' current working directory.
#'
#' @details This function will download all the .csv files from the server and
#' coallate them into one dataframe named "soil_moisture" and saves a .csv file
#' to disk.
#'
#' @examples
#'
get_soil_moisture <- function(userid = "", password  = "", path = "") {

  path <- .get_data_path(path)
  JW_01 <- NULL
  JW_02 <- NULL


  ftp_site <- paste("ftp://", userid, ":",
                    password, "@ftp.usqsoilmoisture.com/public_html/data/",
                    sep = "")

  filenames <- RCurl::getURL(ftp_site, ftp.use.epsv = FALSE, ftplistonly = TRUE,
                             crlf = TRUE)
  filenames <- paste(ftp_site, strsplit(filenames, "\r*\n")[[1]],
                     sep = "")[-c(1:2)]

  for (i in seq_len(length(filenames))) {
    y <- readr::read_table(RCurl::getURL(paste0(filenames[i], "/"),
                                         ftp.use.epsv = FALSE),
                           col_names = FALSE, skip = 2)
    y <- y[grep("-Sensors.csv", y$X9), ]
    y <- subset(y, X5 > 0)
    csv_files <- paste0(filenames[i], "/", y[, 9])

    include_JW_01 <- grep(".JW_01.", csv_files)
    JW_01 <- append(JW_01, csv_files[include_JW_01])

    include_JW_02 <- grep(".JW_02.", csv_files)
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

    rm(list = c("include", "csv_files", "subdirectory_filenames",
                "soil_moisture_JW_01", "soil_moisture_JW_02", "soil_moisture"))

    JW_01 <- NULL
    JW_02 <- NULL
  }
}

# shamelessly borrowed from RJ Hijmans Raster package
.get_data_path <- function(path) {
  path <- raster::trim(path)
  if (path == "") {
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

