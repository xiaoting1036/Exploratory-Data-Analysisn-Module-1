# this is the script for exploratory Data Analysis 
dir
## read in CSV file
power_consum <- read.csv("C:/Users/xiaot/datasciencecoursera/exploratory data/household_power_consumption.txt", sep=";")

## subset data from 2007-02-01 to 2007-02-02
library(dplyr)

# convert the a character string in the new format:
power_consum <- power_consum %>%
    mutate(Date = as.Date(Date, format = "%d/%m/%Y")) %>%
    filter(Date >= as.Date("2007-02-01") & Date <= as.Date("2007-02-02"))

## check the subset data
head (power_consum)
str(power_consum)

# First combine the Date and Time columns into one string
datetime_string <- paste(power_consum$Date, power_consum$Time)

# Then convert to POSIXlt datetime object
power_consum$DateTime <- strptime(datetime_string, format = "%Y-%m-%d %H:%M:%S")
 
# Verify conversion
class(power_consum$DateTime)  # Should return "POSIXlt" "POSIXt"
head(power_consum$DateTime)   # Shows full date and time
# Convert to numeric
power_consum$GAC_num <- as.numeric(as.character(power_consum$Global_active_power))
# Check range
summary(power_consum$GAC_num )

# 1. Open PNG device
png("Plot2.png", width = 800, height = 600, res = 100)  # Adjust width/height/dpi

# 2. Generate the plot
plot(power_consum$DateTime, power_consum$GAC_num,
     type = "l",
     xlab = "",
     ylab = "Global Active Power (kilowatts)",
     xaxt = "n")
axis(1, at = pretty(power_consum$DateTime), labels = format(pretty(power_consum$DateTime), "%a"))

# 3. Close the device
dev.off()