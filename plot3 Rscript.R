# this is the script for exploratory Data Analysis 
dir

## subset data from 2007-02-01 to 2007-02-02
library(ggplot2)
library(dplyr)
library(scales)

## read in CSV file
power_consum <- read.csv("C:/Users/xiaot/datasciencecoursera/exploratory data/household_power_consumption.txt", sep=";")



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


# convert Sub_metering_1, Sub_metering_2 inot numeric
power_consum$Sub_metering_1<- as.numeric(as.character(power_consum$Sub_metering_1))
power_consum$Sub_metering_2<- as.numeric(as.character(power_consum$Sub_metering_2))
power_consum$Sub_metering_3<- as.numeric(as.character(power_consum$Sub_metering_3))
# check the data
str(power_consum)
summary(power_consum$Sub_metering_1)
summary(power_consum$Sub_metering_2)
summary(power_consum$Sub_metering_3)

# check the frequecny
table(power_consum$Sub_metering_1)
table(power_consum$Sub_metering_2)
table(power_consum$Sub_metering_3)

class(power_consum$DateTime)

# 1. Ensure 'dateTime' is in POSIXct format
power_consum$DateTime <- as.POSIXct(power_consum$DateTime)
class(power_consum$DateTime)

# 2. Define custom breaks (start, middle, end of your data)
x_breaks <- c(
    min(power_consum$DateTime),  # First point (Thursday)
    median(power_consum$DateTime),  # Middle point (Friday)
    max(power_consum$DateTime)  # Last point (Saturday)
)

# Define custom labels
x_labels <- c("Thu", "Fri", "Sat")

# 3. Plot with corrected scale_x_datetime()
 p<- ggplot(power_consum, aes(x = DateTime)) +
    geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1"), linewidth = 0.6) +
    geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2"), linewidth = 0.6) +
    geom_line(aes(y = Sub_metering_3, color = "Sub_metering_3"), linewidth = 0.6) +
    scale_x_datetime(
        breaks = x_breaks,             # Custom breaks (Thu, Fri, Sat)
        labels = x_labels,             # Labels ("Thu", "Fri", "Sat")
        expand = c(0.01, 0.01)        # Reduce padding
    ) +
    scale_color_manual(
        name = "Sub Metering",
        values = c(
            "Sub_metering_1" = "black",  # Sub_metering_1
            "Sub_metering_2" = "red",    # Sub_metering_2
            "Sub_metering_3" = "blue"    # Sub_metering_3
        ),
        labels = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    ) +
    labs(
       x = " ",
        y = "Energy  Sub Metering"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 12, face = "bold"),
        legend.position = c(0.95, 0.95),  # Top-right corner (adjust as needed)
        legend.justification = c(1, 1),    # Anchors legend to top-right
        legend.background = element_rect(fill = "white", color = "grey80")  # Optional: improves visibility
# Save as PNG
# Save as PNG
 


