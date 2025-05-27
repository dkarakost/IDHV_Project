
#need these packages for the rest of the script
library(dplyr)
library(lubridate)

#read the CSV file of one TreeTalker (make sure working directory is set)
#change file name to correct TreeTalker file
raw_data <- read.csv("treetalker2.csv", sep=",", header=FALSE, stringsAsFactors=FALSE)

#defines column names for type 55 data based on manual
colnames_55 <- c("TT_ID", "Record_number", "String_type", "Timestamp",
                 "Soil_temp_dn", "Unused1", "Unused2", "adc_bandgap_dn",
                 "Number_of_bits", "Air_relative_humidity", "Air_temperature_10xC",
                 "Ax_out_dn", "Ax_out_stddev_dn", "Ay_out_dn", "Ay_out_stddev_dn",
                 "Az_out_dn", "Az_out_stddev_dn", "Tref_l_dn", "Theat_l_dn",
                 "ECf_Soil_Hz", "adc_Vbat_dn")

#filter only type 55 rows and apply column names, this deletes all spectrometer data that we are not using
ttsoil_data <- raw_data %>% 
  filter(V3 == 55) %>%
  `colnames<-`(colnames_55)

ttsoil_data <- ttsoil_data[, 1:(ncol(ttsoil_data) - 9)]

ttsoil_data <- ttsoil_data %>%
  mutate(
    DateTime_UTC = as.POSIXct(Timestamp, origin = "1970-01-01", tz = "UTC"),
    DateTime_CEST = with_tz(DateTime_UTC, tz = "Europe/Amsterdam")  # Automatically handles CEST (GMT+2)
  )
# Filter to keep only records from May 21, 2025, onwards and remove May 22 between 12:00-14:00
ttsoil_data <- ttsoil_data %>%
  filter(DateTime_UTC >= as.POSIXct("2025-05-21 1:00:00", tz = "UTC")) %>%
  filter(!(DateTime_UTC >= as.POSIXct("2025-05-22 10:00:00", tz = "UTC") & 
             DateTime_UTC <= as.POSIXct("2025-05-22 14:00:00", tz = "UTC")))
  

#convert soil temperature digital number to Celcius using formula 3 in manual
ttsoil_data <- ttsoil_data %>%
  mutate(Soil_temp_C = 127.6 - 0.006045*Soil_temp_dn + 
           1.26E-07*Soil_temp_dn^2 - 1.15E-12*Soil_temp_dn^3)


#initialize constants using S3 sample (see table in manual) I do not for sure know which constants to use
m <- -758
b <- 154844
B1 <- 1.89*10^-11
B2 <- -6.16*10^-5
B3 <- 59

#convert raw hz to hz excluding temperature impact, see formulan 4 in manual
ttsoil_data <- ttsoil_data %>%
  mutate(ECft = ECf_Soil_Hz - (m*(Soil_temp_C - 20) + b))

#calibration formula to volumemetric water content VWC using formula 5 in manual
ttsoil_data <- ttsoil_data %>% 
  mutate(Soil_VWC = B1 * ECft^2 +B2*ECft + B3)

#remove redundant columns, keeps only ID, record number, Soil temp, date and water content
ttsoil_data_S3 <- ttsoil_data %>% select(TT_ID, Record_number, Soil_temp_C, Timestamp, DateTime_UTC, DateTime_CEST, Soil_VWC)
unique(ttsoil_data_S3$TT_ID)


# Split the dataframe into a list of dataframes, one per TT_ID
split_dfs <- split(ttsoil_data_S3, ttsoil_data$TT_ID)

# Extract each dataframe into separate variables
df_018 <- split_dfs[[1]]  # Data for TT_ID 1
df_001 <- split_dfs[[2]]  # Data for TT_ID 2
df_003 <- split_dfs[[3]]  # Data for TT_ID 3
df_006 <- split_dfs[[4]]  # Data for TT_ID 4
df_009 <- split_dfs[[5]]  # Data for TT_ID 5
df_023 <- split_dfs[[6]]  # Data for TT_ID 5
# First, plot Soil_VWC from df_1
plot(df_003$DateTime_CEST,df_003$Soil_temp_C, col = 'blue', ylim = c(0, 22))
points(df_018$DateTime_CEST,df_018$Soil_temp_C, col = "red")
points(df_001$DateTime_CEST,df_001$Soil_temp_C, col = 'red')
points(df_006$DateTime_CEST,df_006$Soil_temp_C, col = 'blue')
points(df_009$DateTime_CEST,df_009$Soil_temp_C, col = 'red')
points(df_023$DateTime_CEST,df_023$Soil_temp_C, col = 'blue')

# Set up the plot with y-axis limits (0 to 21)
plot(df_003$DateTime_CEST, df_003$Soil_VWC, 
     col = 'blue', 
     ylim = c(0, 30),  # Set y-axis range
     xlab = "DateTime_CEST", 
     ylab = "Soil_VWC")

# Add points for other data frames
points(df_018$DateTime_CEST, df_018$Soil_VWC, col = 'red')
points(df_001$DateTime_CEST, df_001$Soil_VWC, col = 'red')
points(df_006$DateTime_CEST, df_006$Soil_VWC, col = 'blue')
points(df_009$DateTime_CEST, df_009$Soil_VWC, col = 'red')
points(df_023$DateTime_CEST, df_023$Soil_VWC, col = 'blue')




# Combine blue datasets
blue_data <- bind_rows(
  df_003 %>% mutate(Group = "Blue"),
  df_006 %>% mutate(Group = "Blue"),
  df_023 %>% mutate(Group = "Blue")
)

# Compute hourly average for blue group
blue_avg <- blue_data %>%
  mutate(DateTime_CEST_rounded = as.POSIXct(round(as.numeric(DateTime_CEST) / 3600) * 3600, 
                                            origin = "1970-01-01", tz = "CEST")) %>%
  group_by(DateTime_CEST_rounded) %>%
  summarise(Soil_VWC_avg = mean(Soil_VWC, na.rm = TRUE)) %>%
  rename(DateTime_CEST = DateTime_CEST_rounded)

# Combine red datasets
red_data <- bind_rows(
  df_018 %>% mutate(Group = "Red"),
  df_001 %>% mutate(Group = "Red"),
  df_009 %>% mutate(Group = "Red")
)

# Compute hourly average for red group
red_avg <- red_data %>%
  mutate(DateTime_CEST_rounded = as.POSIXct(round(as.numeric(DateTime_CEST) / 3600) * 3600, 
                                            origin = "1970-01-01", tz = "CEST")) %>%
  group_by(DateTime_CEST_rounded) %>%
  summarise(Soil_VWC_avg = mean(Soil_VWC, na.rm = TRUE)) %>%
  rename(DateTime_CEST = DateTime_CEST_rounded)

# Base plot with original points (optional)
plot(df_003$DateTime_CEST, df_003$Soil_VWC, col = 'blue', pch = 16, cex = 0.7,
     ylim = c(0, 21), xlab = "DateTime_CEST", ylab = "Soil VWC")
points(df_018$DateTime_CEST, df_018$Soil_VWC, col = 'red', pch = 16, cex = 0.7)
points(df_001$DateTime_CEST, df_001$Soil_VWC, col = 'red', pch = 16, cex = 0.7)
points(df_006$DateTime_CEST, df_006$Soil_VWC, col = 'blue', pch = 16, cex = 0.7)
points(df_009$DateTime_CEST, df_009$Soil_VWC, col = 'red', pch = 16, cex = 0.7)
points(df_023$DateTime_CEST, df_023$Soil_VWC, col = 'blue', pch = 16, cex = 0.7)

# Add averaged lines
lines(blue_avg$DateTime_CEST, blue_avg$Soil_VWC_avg, col = "darkblue", lwd = 2)
lines(red_avg$DateTime_CEST, red_avg$Soil_VWC_avg, col = "darkred", lwd = 2)

# Add legend
legend("bottomright", 
       legend = c("Mole Points", "Non-Mole Points", "Mole Avg", "Non-mole Avg"),
       col = c("blue", "red", "darkblue", "darkred"),
       pch = c(16, 16, NA, NA),
       lty = c(NA, NA, 1, 1),
       lwd = 2)


library(dplyr)

# Combine blue datasets (Mole)
blue_data_temp <- bind_rows(
  df_003 %>% mutate(Group = "Mole"),
  df_006 %>% mutate(Group = "Mole"),
  df_023 %>% mutate(Group = "Mole")
)

# Compute hourly average for mole group (Soil_temp_C)
blue_avg_temp <- blue_data_temp %>%
  mutate(DateTime_CEST_rounded = as.POSIXct(round(as.numeric(DateTime_CEST) / 3600) * 3600, 
                                            origin = "1970-01-01", tz = "CEST")) %>%
  group_by(DateTime_CEST_rounded) %>%
  summarise(Soil_temp_avg = mean(Soil_temp_C, na.rm = TRUE)) %>%
  rename(DateTime_CEST = DateTime_CEST_rounded)

# Combine red datasets (Non-Mole)
red_data_temp <- bind_rows(
  df_018 %>% mutate(Group = "Non-Mole"),
  df_001 %>% mutate(Group = "Non-Mole"),
  df_009 %>% mutate(Group = "Non-Mole")
)

# Compute hourly average for non-mole group (Soil_temp_C)
red_avg_temp <- red_data_temp %>%
  mutate(DateTime_CEST_rounded = as.POSIXct(round(as.numeric(DateTime_CEST) / 3600) * 3600, 
                                            origin = "1970-01-01", tz = "CEST")) %>%
  group_by(DateTime_CEST_rounded) %>%
  summarise(Soil_temp_avg = mean(Soil_temp_C, na.rm = TRUE)) %>%
  rename(DateTime_CEST = DateTime_CEST_rounded)

# Base plot with original points (Soil_temp_C)
plot(df_003$DateTime_CEST, df_003$Soil_temp_C, col = 'blue', pch = 16, cex = 0.7,
     ylim = c(0, 30),  # Adjust y-axis for temperature range (e.g., 0-30°C)
     xlab = "DateTime_CEST", ylab = "Soil Temperature (°C)")
points(df_018$DateTime_CEST, df_018$Soil_temp_C, col = 'red', pch = 16, cex = 0.7)
points(df_001$DateTime_CEST, df_001$Soil_temp_C, col = 'red', pch = 16, cex = 0.7)
points(df_006$DateTime_CEST, df_006$Soil_temp_C, col = 'blue', pch = 16, cex = 0.7)
points(df_009$DateTime_CEST, df_009$Soil_temp_C, col = 'red', pch = 16, cex = 0.7)
points(df_023$DateTime_CEST, df_023$Soil_temp_C, col = 'blue', pch = 16, cex = 0.7)

# Add averaged lines
lines(blue_avg_temp$DateTime_CEST, blue_avg_temp$Soil_temp_avg, col = "darkblue", lwd = 2)
lines(red_avg_temp$DateTime_CEST, red_avg_temp$Soil_temp_avg, col = "darkred", lwd = 2)

# Add legend
legend("topright", 
       legend = c("Mole Points", "Non-Mole Points", "Mole Avg", "Non-Mole Avg"),
       col = c("blue", "red", "darkblue", "darkred"),
       pch = c(16, 16, NA, NA),
       lty = c(NA, NA, 1, 1),
       lwd = 2)
