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

# Create custom labels
 custom_labels <- paste0(breaks[-length(breaks)], "-", breaks[-1])

 #  First create your 0.5 kW bins (if not already done)
 power_consum$Power_category <- cut(power_consum$GAC_num ,
                                    breaks = seq(0, 8, by = 0.5),
                                    labels = c(1:16),
                                    include.lowest = TRUE)

 
 # Check the levels/categories created
 levels(power_consum$Power_category)
 
 # Create the custom labels vector
 range_labels <- rep("", 16)  # Start with all empty labels
 range_labels[8] <- "4"       # Label level 8 as "4"
 range_labels[12] <- "6"      # Label level 12 as "6"
 
 # 3. Create and save the plot
 plot <- ggplot(power_consum, aes(x = Power_category)) +
     geom_bar(fill = "Red", color="white", width = 1.0) +
     scale_x_discrete(labels = range_labels) +  # Explicitly set labels
     labs(title = "Global Active Power Distribution",
          x = "Power Range (kW)",
          y = "Frequency") +
     theme_minimal() +
     theme(
         # White background and dark border
         panel.background = element_rect(fill = "white", color = NA),
         plot.background = element_rect(fill = "white", color = NA),
         
         # Red x-axis labels
         axis.text.x = element_text(color = "red", angle = 0, hjust = 1),
         
         # Optional: customize other text elements
         axis.title = element_text(color = "black"),
         plot.title = element_text(color = "black"),
         
         # Grid lines customization (optional)
         panel.grid.major = element_line(color = "gray90"),
         panel.grid.minor = element_blank()
         )
  # Save as PNG
  ggsave("Plot1.png", plot, width = 8, height = 6, dpi = 300) 
