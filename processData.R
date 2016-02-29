require(dplyr)
require(lubridate)

file <- dir()[1]

compass.raw <- read.csv(file, stringsAsFactors = FALSE, header = TRUE)

# Check out the structure
# str(compass.raw)
# 5 variables:
# $DateTime (char) - timestamp
# $Location (char) - location
# $Transaction (char) - type of transaction e.g. tap-in or transfer
# $Product (char) - what pass is used
# $Amount (num) - value of transaction

# We only want to keep DateTime, Location, and Transaction here
compass.raw <- compass.raw[,c(1:3)]


# Convert character DateTime into POSIXCT object

compass.raw$time <- mdy_hm(compass.raw$DateTime, tz = "America/Vancouver")
compass.raw$DateTime <- NULL

# In this data, bustops are represented by 5 digit numbers, while skytrain stations are named entries all containing the expression "Stn".

compass.skytrain <- compass.raw[grepl("Stn",compass.raw$Location),]

# We want "Tap in" and "Transfer" to represent Start and "Tap Out" to represent Finish

compass.skytrain$tapin[grepl("Tap in|Transfer",compass.skytrain$Transaction)] <- 1
compass.skytrain$tapout[grepl("Tap out",compass.skytrain$Transaction)] <- 1
compass.skytrain[is.na(compass.skytrain)] <- 0

compass.skytrain$start[compass.skytrain$tapin==1] <- compass.skytrain$Location[compass.skytrain$tapin==1]
compass.skytrain$finish[compass.skytrain$tapout==1] <- compass.skytrain$Location[compass.skytrain$tapout==1]

# Can now get rid of the transaction as well
compass.skytrain$Transaction <- NULL

# Generate trips
compass.skytrain$trip <- rep(1:(length(compass.skytrain$time)/2), each = 2)

compass.trips <- compass.skytrain %>%
  group_by(trip) %>%
  mutate(startTime = min(time),
         endTime = max(time),
         tripLength = as.numeric(max(time)-min(time)),
         tripOrigin = sort(start)[1],
         tripDestination = sort(finish)[1])


compass.trips <- compass.trips[,c(7:12)]
compass.trips <- compass.trips[!duplicated(compass.trips),]


