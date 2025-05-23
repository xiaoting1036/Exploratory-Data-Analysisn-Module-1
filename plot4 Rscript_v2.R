# this is the script for exploratory Data Analysis 
dir

## subset data from 2007-02-01 to 2007-02-02
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)  # For arranging plots
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
power_consum$GRP_num <- as.numeric(as.character(power_consum$Global_reactive_power))
power_consum$Voltage <- as.numeric(as.character(power_consum$Voltage))

# convert Sub_metering_1, Sub_metering_2 inot numeric
power_consum$Sub_metering_1<- as.numeric(as.character(power_consum$Sub_metering_1))
power_consum$Sub_metering_2<- as.numeric(as.character(power_consum$Sub_metering_2))
power_consum$Sub_metering_3<- as.numeric(as.character(power_consum$Sub_metering_3))

# check the data
str(power_consum)
summary(power_consum$Sub_metering_1)
summary(power_consum$Sub_metering_2)
summary(power_consum$Sub_metering_3)
summary(power_consum$GAC_num)
summary(power_consum$GRP_num)
summary(power_consum$Voltage)

# check the frequecny
table(power_consum$Sub_metering_1)
table(power_consum$Sub_metering_2)
table(power_consum$Sub_metering_3)

str(power_consum)
# 2. Define custom breaks (start, middle, end of your data)
x_breaks <- c(
    min(power_consum$DateTime),  # First point (Thursday)
    median(power_consum$DateTime),  # Middle point (Friday)
    max(power_consum$DateTime)  # Last point (Saturday)
)
# Define custom labels
x_labels <- c("Thu", "Fri", "Sat")


# P1 Global Active Power Over Time
p1 <- ggplot(power_consum, aes(x = DateTime, y = GAC_num)) +
    geom_line(color = "black", linewidth = 0.6) +  # linewidth instead of size in newer ggplot2
    scale_x_datetime(
        breaks = x_breaks,
        labels = x_labels,
        expand = c(0.01, 0.01)  # Reduce padding
    ) +
    labs(
        x = NULL,  # Equivalent to xlab = "" in base R
        y = "Global Active Power (kilowatts)",
        title = "Global Active Power Over Time"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
    )

# Print the plot
print(p1)

#2 .Voltage  vs DataTime
p2 <- ggplot(power_consum, aes(x = DateTime, y = Voltage)) +
    geom_line(color = "black", linewidth = 0.6) +  # linewidth instead of size in newer ggplot2
    scale_x_datetime(
        breaks = x_breaks,
        labels = x_labels,
        expand = c(0.01, 0.01)  # Reduce padding
    ) +
    labs(
        x = NULL,  # Equivalent to xlab = "" in base R
        y = "Global Active Power (kilowatts)",
        title = "Voltage"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
    )

# Print the plot
print(p2)

# 3. Sub-meterings vs DateTime

p3<- ggplot(power_consum, aes(x = DateTime)) +
    geom_line(aes(y = Sub_metering_1, color = "Sub_metering_1"), linewidth = 0.6) +
    geom_line(aes(y = Sub_metering_2, color = "Sub_metering_2"), linewidth = 0.6) +
    geom_line(aes(y = Sub_metering_3, color = "Sub_metering_3"), linewidth = 0.6) +
    scale_x_datetime(
        breaks = x_breaks,             # Custom breaks (Thu, Fri, Sat)
        labels = x_labels,             # Labels ("Thu", "Fri", "Sat")
        expand = c(0.01, 0.01)        # Reduce padding
    ) +
    scale_color_manual(
        name = " ",
        values = c(
            "Sub_metering_1" = "black",  # Sub_metering_1
            "Sub_metering_2" = "red",    # Sub_metering_2
            "Sub_metering_3" = "blue"    # Sub_metering_3
        ),
        labels = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3")
    ) +
    labs(
        x = " ",
        y = "Energy  Sub Metering",
        title = "Sub Metreing"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 6, face = "bold"),
        legend.position = "right",  # Changed to place legend outside
        #legend.position = c(0.4, 0.4),  # Top-right corner (adjust as needed)
        legend.justification = c(1, 1),    # Anchors legend to top-right
        legend.background = element_rect(fill = "white", color = "grey80")  # Optional: improves visibility
    )
# Print the plot
print(p3)

#4 GRP_num   Global_reactive_power vs Dateime

p4 <- ggplot(power_consum, aes(x = DateTime, y = GRP_num)) +
    geom_line(color = "black", linewidth = 0.6) +  # linewidth instead of size in newer ggplot2
    scale_x_datetime(
        breaks = x_breaks,
        labels = x_labels,
        expand = c(0.01, 0.01)  # Reduce padding
    ) +
    labs(
        x = NULL,  # Equivalent to xlab = "" in base R
        y = "Global ReActive Power (kilowatts)",
        title = "Global REActive Power Over Time"
    ) +
    theme_minimal() +
    theme(
        axis.text.x = element_text(size = 8, face = "bold"),
        axis.title.y = element_text(size = 10),
        panel.grid.major = element_line(color = "gray90"),
        panel.grid.minor = element_blank()
    )

# Print the plot
print(p4)

# combine as a panel plot
class(p1)
class(p2)
class(p3)
class(p4)

#create panel plot
panel_plot <- (p1 | p2) / (p3 | p4)


# Display the plot
print(panel_plot)


# Save the plot
ggsave("C:/Users/xiaot/datasciencecoursera/exploratory data/plot4.png", plot = panel_plot, width = 10, height = 8, dpi = 300)