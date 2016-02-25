get_soil_moisture(userid = "usq21270", password = "g-vNZ17GoY", path = "~/tmp")


library(readr)
library(ggplot2)
library(scales)
library(pracma)
library(doBy)

soil <- read_csv("~/tmp/2016-02-16_Soil_Moisture.csv")
names(soil)[3] <- "Moisture"

# Use hampel filter to remove outliers before summarising to daily values
soil$Filtered_Moisture <- hampel(soil[, 3], 4, t0 = 3)$y

plot_data <- summaryBy(formula = Filtered_Moisture ~ Date + Sensor, data = soil,
                       FUN = mean)
plot_data[, 1] <- as.Date(plot_data[, 1], format = "%d/%m/%Y")

ggplot(plot_data, aes(x = Date, y = Filtered_Moisture.mean, colour = Sensor)) +
  geom_line() +
  ylab("Soil Moisture (%)")

