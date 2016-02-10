#' @title Download, Clean and Generate Graphs From USQ Theta Probe Data
#'
#'@description This function automates downloading and cleaning data from the
#'University of Southern Queensland National Centre for Engineering in
#'Agriculture (NCEA).
#'
#'This is a slow process to retrieve all the files from the server.
#'
#' @param userid The login provided by NCEA to login via FTP
#' @param password The password provided by NCEA to login via FTP
#'
#' @details This function downloads all CSV
#'
#' @examples
#' # Download data for years 2009 and 2010 and generate yearly summary files,
#' # GSOD_TP2009_XY and GSOD_TP2010_XY files in folders 2009 and 2010 of your
#' # working directory with a maximum of five missing days per weather station
#' # allowed.
#'
#' get_GSOD(start_year = 2009, end_year = 2010, max_missing = 5, country)
#'
ftp_soilmoisture <- function(userid = "", password  = "") {
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
    if(i == 3){
      soil_moisture <- plyr::ldply(csv_files, readr::read_csv,
                                   col_names = FALSE)
    } else {
      sm <- plyr::ldply(csv_files, readr::read_csv, col_names = FALSE)
      soil_moisture <- rbind(soil_moisture, sm)
    }
  }
  return(soil_moisture)
}

