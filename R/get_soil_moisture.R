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

  ftp_site <- paste("ftp://", userid, ":",
                    password, "@ftp.usqsoilmoisture.com/public_html/data/",
                    sep = "")

  filenames <- RCurl::getURL(ftp_site, ftp.use.epsv = FALSE, ftplistonly = TRUE,
                             crlf = TRUE)
  filenames <- paste(ftp_site, strsplit(filenames, "\r*\n")[[1]], sep = "")

  for(i in 3:length(filenames)){
    subdirectory_filenames <- RCurl::getURL(paste0(filenames[i], "/"),
                                            ftp.use.epsv = FALSE,
                                            ftplistonly = TRUE, crlf = TRUE)
    subdirectory_filenames <- paste(filenames[i], "/",
                                    strsplit(subdirectory_filenames,
                                             "\r*\n")[[1]], sep = "")

    include <- grep("*.csv", subdirectory_filenames)
    csv_files <- subdirectory_filenames[include]
    include_JW_01 <- grep(".JW_01.", csv_files)
    JW_01 <- csv_files[include_JW_01]
    include_JW_02 <- grep(".JW_02.", csv_files)
    JW_02 <- csv_files[include_JW_02]

    if(i == 3){
      soil_moisture_JW_01 <- plyr::ldply(JW_01, readr::read_csv,
                                         col_names = FALSE)
      soil_moisture_JW_01$Sensor <- rep("JW_01",
                                        length(soil_moisture_JW_01[, 1]))
      soil_moisture_JW_02 <- plyr::ldply(JW_02, readr::read_csv,
                                         col_names = FALSE)
      soil_moisture_JW_02$Sensor <- rep("JW_02",
                                        length(soil_moisture_JW_02[, 1]))

      soil_moisture <- rbind(soil_moisture_JW_01[, c(1:3, 13)],
                             soil_moisture_JW_02[, c(1:3, 13)])
      names(soil_moisture) <- c("Date", "Time", "Moisture", "Sensor")
      readr::write_csv(soil_moisture, paste0(path, "/", Sys.Date(),
                                             "_Soil_Moisture.csv"))
    }

    sm_JW_01 <- plyr::ldply(JW_01, readr::read_csv, col_names = FALSE)
    sm_JW_01$Sensor <- rep("JW_01", length(sm_JW_01[, 1]))
    sm_JW_02 <- plyr::ldply(JW_02, readr::read_csv, col_names = FALSE)
    sm_JW_02$Sensor <- rep("JW_02", length(sm_JW_02[, 1]))

    sm <- rbind(sm_JW_01[, c(1:3, 13)], sm_JW_02[, c(1:3, 13)])
    readr::write_csv(sm, paste0(path, "/", Sys.Date(), "_Soil_Moisture.csv"),
                     append = TRUE)
    i <- i + 1
  }
}

# shamelessly borrowed from RJ Hijmans Raster package
.get_data_path <- function(path) {
  path <- raster::trim(path)
  if (path == "") {
    path <- getwd()
  } else {
    if (substr(path, nchar(path)-1, nchar(path)) == "//") {
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

