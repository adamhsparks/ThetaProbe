



#' @title Download, Clean and Generate Graphs From USQ ThetaProbe Data
#'
#'@description This package automates downloading and cleaning of soil moisture
#' data from ThetaProbs.
#'
#'This is a slow process to retrieve all the files from the server for the first
#'run.  After the first run, it will be much faster to only retrieve new files.
#'
#' @param userpwd The login and password provided by NCEA to login via file
#' transfer protocol (FTP)
#' @param path File path to directory for saving a comma separated file (CSV)
#' output.  Defaults to current working directory.
#' @param local_dirs File path to directory, which holds previous data logger
#' data monthly file folders, which contain hourly CSV files from theta
#' probe loggers.
#'
#' @details This function will download CSV files from the server that
#' are not currently on the local machine in the user specified location and
#' collate them into one CSV file for further use.
#'
#' @examples
#' \dontrun{
#' get_soil_moisture(userpwd = "userid:password", path = NULL,
#' local_dirs = NULL)
#' }
#' @export
get_soil_moisture <-
  function(userpwd = NULL,
           path = NULL,
           local_dirs = NULL) {
    if (is.null(userpwd)) {
      stop("You must enter a user id and password to login to the server")
    }
    if (is.null(local_dirs)) {
      readline(
        prompt = "\n
        You have not specified a location for local data files.\n
        R will attempt to download all data files from\n
        ftp.usqsoilmoisture.com.\n
        If this is correct, please press [enter] to continue.\n
        Otherwise, [esc] will cancel this operation and you may specify the\n
        location of local files.\n"
      )
    }

    path <- .get_data_path(path)
    local_dirs <- .get_data_path(local_dirs)

    if (file.exists(paste0(path, "/", Sys.Date(), "_Soil_Moisture.csv"))) {
      file.remove(paste0(path, "/", Sys.Date(), "_Soil_Moisture.csv"))
    }

    JW_01 <- NULL
    JW_02 <- NULL

    # get full list of local directories
    local_dirs <- list.dirs(path = local_dirs)[-1]
    local_dirs <-
      substr(local_dirs, nchar(local_dirs) - 5, nchar(local_dirs))

    # what is the most up to date directory that exists (month)?
    latest_dir <- max(local_dirs)
    # what files for that month are present locally?
    latest_files <- list.files(file.path(path, latest_dir))

    ftp_site <-
      paste0("ftp://",
             userpwd,
             "@ftp.usqsoilmoisture.com/public_html/data/")
    remote_dirs <-
      RCurl::getURL(
        ftp_site,
        ftp.use.epsv = FALSE,
        ftplistonly = TRUE,
        crlf = TRUE,
        ssl.verifypeer = FALSE
      )
    remote_dirs <-
      paste0(ftp_site, strsplit(remote_dirs, "\r*\n")[[1]])[-c(1:2)]

    # take only directory names, not full path
    remote_dirs <- substr(remote_dirs, 68, 73)

    # which directories do not exist locally?
    remote_dirs <- remote_dirs[remote_dirs %in% local_dirs == FALSE]

    # add the latest local directory, it may not have complete data
    remote_dirs <- c(latest_dir, remote_dirs)

    for (dir in seq_along(remote_dirs)) {
      if (!utils::file_test("-d", paste0(path, "/", remote_dirs[dir]))) {
        dir.create(file.path(path, "/", remote_dirs[dir]))
      }
    }

    # Loop to download new data files from server ------------------------------
    for (i in seq_along(remote_dirs)) {
      remote <- paste(ftp_site, remote_dirs[i], "/", sep = "")
      csv_files <-
        RCurl::getURL(
          remote,
          ftp.use.epsv = FALSE,
          ftplistonly = TRUE,
          crlf = TRUE,
          ssl.verifypeer = FALSE
        )
      csv_files <- strsplit(csv_files, "\r*\n")[[1]]
      csv_files <-
        csv_files[grep("JW_0[[:graph:]]+Sensors.csv$", csv_files)]

      if (i == 1) {
        csv_files <- csv_files[csv_files %in% latest_files == FALSE]
      }

      include_JW_01 <- grep("JW_01.", csv_files)
      JW_01 <- append(JW_01, csv_files[include_JW_01])

      include_JW_02 <- grep("JW_02.", csv_files)
      JW_02 <- append(JW_02, csv_files[include_JW_02])

      con <-
        RCurl::getCurlHandle(ftp.use.epsv = FALSE, ssl.verifypeer = FALSE)

      JW_01_files <- sapply(paste0(remote, JW_01),
                            function(x)
                              try(RCurl::getURL(x, curl = con)))
      JW_02_files <- sapply(paste0(remote, JW_02),
                            function(x)
                              try(RCurl::getURL(x, curl = con)))

      JW_01_files <-
        lapply(JW_01_files, data.frame, stringsAsFactors = FALSE)
      JW_02_files <-
        lapply(JW_02_files, data.frame, stringsAsFactors = FALSE)

      names(JW_01_files) <- JW_01
      names(JW_02_files) <- JW_02

      for (f in seq_along(JW_01_files)) {
        readr::write_csv(
          as.data.frame(lapply(JW_01_files[[f]],
                               function(x)
                                 utils::read.csv(text = x, header = FALSE))),
          path = paste0(path, "/", remote_dirs[i],
                        "/", JW_01[f]),
          col_names = FALSE
        )
      }

      for (g in seq_along(JW_02_files)) {
        readr::write_csv(
          as.data.frame(lapply(JW_02_files[[g]],
                               function(x)
                                 utils::read.csv(text = x, header = FALSE))),
          path = paste0(path, "/", remote_dirs[i], "/", JW_02[g]),
          col_names = FALSE
        )
      }
    }

    # Loop in local directories after downloading new data files and -----------
    #  generate new CSV file
    for (l in local_dirs) {
      JW_01 <- list.files(file.path(path, l),
                          pattern = "JW_01[[:graph:]]+Sensors.csv",
                          full.names = TRUE)

      if (length(JW_01) > 0) {
        # check file sizes and discard those files with no data
        info <- file.info(JW_01)
        empty <- rownames(info[info$size < 40,])
        JW_01 <- JW_01[JW_01 %in% empty == FALSE]

        soil_moisture_JW_01 <- data.table::rbindlist(lapply(
          JW_01,
          data.table::fread,
          header = FALSE,
          select = c(1:3)
        ))
        if (nrow(soil_moisture_JW_01) >= 1) {
          soil_moisture_JW_01$Sensor <- rep("JW_01",
                                            length(soil_moisture_JW_01[, 1]))
        } else {
          soil_moisture_JW_01 <-
            data.table::data.table(matrix(
              c(NA, NA, NA, "JW_01"),
              ncol = 4,
              nrow = 1
            ))
          names(soil_moisture_JW_01) <-
            c("V1", "V2", "V3", "Sensor")
        }
      }


      JW_02 <- list.files(file.path(path, l),
                          pattern = "JW_02[[:graph:]]+Sensors.csv",
                          full.names = TRUE)

      if (length(JW_02) > 0) {
        # check file sizes and discard those files with no data
        info <- file.info(JW_02)
        empty <- rownames(info[info$size < 40,])
        JW_02 <- JW_02[JW_02 %in% empty == FALSE]

        soil_moisture_JW_02 <- data.table::rbindlist(lapply(
          JW_02,
          data.table::fread,
          header = FALSE,
          select = c(1:3)
        ))

        if (nrow(soil_moisture_JW_02) >= 1) {
          soil_moisture_JW_02$Sensor <- rep("JW_02",
                                            length(soil_moisture_JW_02[, 1]))

        } else {
          soil_moisture_JW_02 <-
            data.table::data.table(matrix(
              c(NA, NA, NA, "JW_02"),
              ncol = 4,
              nrow = 1
            ))
          names(soil_moisture_JW_02) <-
            c("V1", "V2", "V3", "Sensor")
        }
      }

      if (length(JW_01) > 0 & length(JW_02) > 0) {
        soil_moisture <- stats::na.omit(rbind(soil_moisture_JW_01,
                                              soil_moisture_JW_02))
      } else
        if (length(JW_01) > 0 & length(JW_02) < 0) {
          soil_moisture <- stats::na.omit(soil_moisture_JW_01)
        } else
          soil_moisture <- soil_moisture_JW_02

      names(soil_moisture) <-
        c("Date", "Time", "Moisture", "Sensor")
      readr::write_csv(soil_moisture,
                       paste0(path, "/", Sys.Date(),
                              "_Soil_Moisture.csv"),
                       append = TRUE)
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
  return(path)
}
