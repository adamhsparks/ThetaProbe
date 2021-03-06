% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clean_theta_data.R
\name{clean_theta_data}
\alias{clean_theta_data}
\title{Automated Cleaning of ThetaProbe Data}
\usage{
clean_theta_data(csv_in = NULL)
}
\arguments{
\item{csv_in}{A CSV file generated by \code{\link{get_soil_moisture}}, which
will be cleaned of outliers and aggregated to daily values}
}
\description{
This function checks for and removes outliers in the
University of Southern Queensland National Centre for Engineering in
Agriculture's (NCEA) Theta Probe data of soil moisture for the USQ CCH Summer
Crops Pathology group, saving the results as a CSV file with daily values.
}
\details{
This function will check the input file for outliers in five minute
data, filter them and generate a new file with daily moisture values that
will be written to disk in the user's specified location.
}
\examples{
\dontrun{
cleaned <- clean_theta_data(csv_in = "~/Soil_Moisture.csv")

write.csv(cleaned, "~/Documents/cleaned_data.csv")
}
}
