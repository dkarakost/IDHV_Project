
#need these packages for the rest of the script
library(dplyr)
library(lubridate)

#read the CSV file of one TreeTalker (make sure working directory is set)
#change file name to correct TreeTalker file
raw_data <- read.csv("63210010.csv", sep=";", header=FALSE, stringsAsFactors=FALSE)

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

#convert timestamp to UTC datetime using formula in manual
ttsoil_data <- ttsoil_data %>%
  mutate(DateTime_UTC = as.POSIXct(Timestamp, origin="1970-01-01", tz="UTC"))


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
ttsoil_data_S3 <- ttsoil_data %>% select(TT_ID, Record_number, Soil_temp_C, DateTime_UTC, Soil_VWC)


