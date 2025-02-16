# Load necessary packages
library(tidyverse)
library(lubridate)

# Create data frame with swimmer data
swimmers <- data.frame(
  Year = c(
    2015, 2015, 2015, 2015, 2016, 2016, 2017, 2017, 2017, 2017, 2017, 2018, 2018, 
    2018, 2018, 2018, 2019, 2019, 2019, 2019, 2019, 2019, 2020, 2020, 2020, 2021, 
    2021, 2021, 2021, 2021, 2021, 2021, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 
    2022, 2023, 2023
  ),
  Haylee = c(
    "2.17.32", "2.16.12", "2.16.52", "2.15.32", "2.10.95", "2.13.91", "2.08.27", 
    "2.09.87", "2.15.38", "2.09.20", "2.07.72", "2.08.13", "2.07.59", "2.07.50", 
    "2.08.88", "2.09.09", "2.07.41", "2.08.12", "2.07.31", "2.07.31", "2.20.96", 
    "2.11.28", "2.14.83", "2.08.60", "2.09.03", "2.12.70", "2.10.80", "2.11.88", 
    "2.14.87", "2.13.67", "2.18.01", "2.09.52", "2.09.54", "2.13.18", "2.14.76", 
    "2.07.71", "2.08.24", "2.13.20", "2.10.94", "2.07.67", "2.07.55", "2.07.66", 
    "2.05.76"
  ),
  Amelia = c(
    "2.22.81", "2.21.61", "2.20.19", "2.18.99", "2.19.50", "2.18.30", "2.10.75", 
    "2.09.95", "2.16.06", "2.14.86", "2.13.15", "2.11.95", "2.12.46", "2.15.94", 
    "2.08.04", "2.07.32", "2.09.93", "2.15.35", "2.15.35", "2.07.13", "2.09.63", 
    "2.09.24", "2.11.06", "2.10.65", "2.10.65", "2.17.67", "2.16.47", "2.18.35",
  ))
    # Load required packages
    library(tidyverse)
    library(lubridate)
    
    # Read in the data
    swimmers <- read.csv("swimmers.csv")
    
    # Convert time strings to durations
    swimmers$Haylee <- hms(paste0("0:", swimmers$Haylee))
    swimmers$Amelia <- hms(paste0("0:", swimmers$Amelia))
    
    # Calculate the difference in swim times between the two swimmers
    swimmers$Diff <- swimmers$Haylee - swimmers$Amelia
    
    # Split the data by year
    swim_by_year <- swimmers %>%
      group_by(Year) %>%
      summarize(
        n = n(),
        Haylee_avg = mean(Haylee),
        Amelia_avg = mean(Amelia),
        Diff_avg = mean(Diff)
      )
    
    # Plot the swim times over time
    swim_plot <- swim_by_year %>%
      pivot_longer(
        cols = c("Haylee_avg", "Amelia_avg"),
        names_to = "Swimmer",
        values_to = "Time"
      ) %>%
      ggplot(aes(x = Year, y = Time, color = Swimmer)) +
      geom_line(size = 1.2) +
      geom_point(size = 2.5) +
      scale_y_time() +
      labs(
        title = "Swim Times Over Time",
        x = "Year",
        y = "Swim Time",
        color = "Swimmer"
      ) +
      theme_bw()
    
    swim_plot
    
    # Perform a t-test on the swim time difference between the two swimmers
    t.test(swimmers$Haylee, swimmers$Amelia, paired = TRUE)
    library(ggplot2)
    
    swim_data <- data.frame(
      Year = c(2015, 2015, 2015, 2015, 2016, 2016, 2017, 2017, 2017, 2017, 2017, 2018, 2018, 2018, 2018, 2018, 2019, 2019, 2019, 2019, 2019, 2019, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021, 2021, 2021, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2023, 2023),
      Haylee = c(2.17.32, 2.16.12, 2.16.52, 2.15.32, 2.10.95, 2.13.91, 2.08.27, 2.09.87, 2.15.38, 2.09.20, 2.07.72, 2.08.13, 2.07.59, 2.07.50, 2.08.88, 2.09.09, 2.07.41, 2.08.12, 2.07.31, 2.07.31, 2.20.96, 2.11.28, 2.14.83, 2.08.60, 2.09.03, 2.12.70, 2.10.80, 2.11.88, 2.14.87, 2.13.67, 2.18.01, 2.09.52, 2.09.54, 2.13.18, 2.14.76, 2.07.71, 2.08.24, 2.13.20, 2.10.94, 2.07.67, 2.07.55, 2.07.66),
      Amelia = c(2.22.81, 2.21.61, 2.20.19, 2.18.99, 2.19.50, 2.18.30, 2.10.75, 2.09.95, 2.16.06, 2.14.86, 2.13.15, 2.11.95, 2.12.46, 2.15.94, 2.08.04, 2.07.32, 2.09.93, 2.15.35, 2.15.35, 2.07.13, 2.09.63, 2.09.24, 2.11.06, 2.10.65, 2.10.65, 2.17.67, 2.16.47, 2.18.35, 2.12.97, 2.16.12, 2.15.12, 2.18.57, 2.
                 data <- c(2.17, 2.16, 2.16, 2.15, 2.10, 2.13, 2.08, 2.09, 2.15, 2.09, 2.07, 2.08, 2.07, 2.08, 2.07, 2.07, 2.20, 2.11, 2.14, 2.08, 2.09, 2.07, 2.07, 2.12, 2.10, 2.11, 2.14, 2.13, 2.18, 2.09, 2.09, 2.13, 2.14, 2.07, 2.08, 2.13, 2.10, 2.07, 2.07, 2.05)
                 hist(data, breaks = 10, main = "Histogram of 200m Individual Medley Times", xlab = "Time (minutes)")
                 data <- data.frame(year = c(2015, 2015, 2015, 2015, 2016, 2016, 2017, 2017, 2017, 2017, 2017, 2018, 2018, 2018, 2018, 2018, 2019, 2019, 2019, 2019, 2019, 2019, 2020, 2020, 2020, 2021, 2021, 2021, 2021, 2021, 2021, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2022, 2023, 2023),
                                    haylee = c(137.32, 136.12, 136.52, 135.32, 130.95, 133.91, 128.27, 129.87, 135.38, 129.20, 127.72, 128.13, 127.59, 127.50, 128.88, 129.09, 127.41, 128.12, 127.31, 127.31, 140.96, 131.28, 134.83, 128.60, 129.03, 132.70, 130.80, 131.88, 134.87, 133.67, 138.01, 129.52, 129.54, 133.18, 134.76, 127.71, 128.24, 133.20, 130.94, 127.67, 127.55),
                                    amelia = c(142.81, 141.61, 140.19, 138.99, 139.50, 138.30, 130.75, 129.95, 136.06, 134.86, 133.15, 131.95, 132.46, 135.94, 128.04, 127.32, 129.93, 135.35, 135.35, 137.13, 129.63, 129.24, 131.06, 130.65, 130.65, 137.67, 136.47, 138.35, 132.97, 218.99, 212.99, 212.99, 212.99, 208.00, 212.99, 213.87, 209.36, 212.73, 213.27, 214.92, 215.81, 209.95))
                 
                 plot(data$haylee, data$amelia, main = "Scatterplot of Haylee and Amelia's Times", xlab = "Haylee's Times (in seconds)", ylab = "Amelia's Times (in seconds)")
                 # Import necessary libraries
                 import pandas as pd
                 import numpy as np
                 import matplotlib.pyplot as plt
                 from scipy.stats import ttest_ind
                 
                 # Load the data
                 df = pd.read_csv('swimming_data.csv', delimiter='\t')
                 
                 # Create separate dataframes for each swimmer
                 haylee = df[df['Swimmer'] == 'Haylee']['Time']
                 amelia = df[df['Swimmer'] == 'Amelia']['Time']
                 
                 # Convert time strings to seconds
                 haylee_seconds = [int(t.split('.')[0])*60 + int(t.split('.')[1]) for t in haylee]
                 amelia_seconds = [int(t.split('.')[0])*60 + int(t.split('.')[1]) for t in amelia]
                 
                 # Calculate mean and standard deviation for each swimmer
                 haylee_mean = np.mean(haylee_seconds)
                 haylee_std = np.std(haylee_seconds)
                 amelia_mean = np.mean(amelia_seconds)
                 amelia_std = np.std(amelia_seconds)
                 
                 # Calculate t-statistic and p-value for independent samples t-test
                 t_stat, p_value = ttest_ind(haylee_seconds, amelia_seconds, equal_var=False)
                 
                 # Plot boxplot for each swimmer's times
                 plt.boxplot([haylee_seconds, amelia_seconds], labels=['Haylee', 'Amelia'])
                 plt.title('Swimming Times Boxplot')
                 plt.ylabel('Time (seconds)')
                 plt.show()
                 
                 # Print results
                 print('Haylee mean time:', haylee_mean)
                 print('Haylee standard deviation:', haylee_std)
                 print('Amelia mean time:', amelia_mean)
                 print('Amelia standard deviation:', amelia_std)
                 print('t-statistic:', t_stat)
                 print('p-value:', p_value)
                 