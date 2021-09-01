# Wesley Wong Kee Han
# TP059618

# ----------------------------------- Import Data Set & Packages -------------------------------------------------------------------------------------------

install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
install.packages("plotrix")
library(plotrix)

weather_stats = read.csv(file="C://Users//USER//Desktop//Course Material//Programming for Data Analysis//Assignment//weather.csv", header = TRUE)

# --------------------------------------- Initial Analysis -------------------------------------------------------------------------------------------------

summary(weather_stats)
names(weather_stats)
str(weather_stats)
head(weather_stats)
nrow(weather_stats)
ncol(weather_stats)

# ------------------------------------- Data Pre-Processing -----------------------------------------------------------------------------------------------

# Find number of missing values for all columns (Before pre-processing)
(colNA <- apply(weather_stats, 2, function(x) sum(is.na(x))))

# Drop WindDir9am & WindDir3pm (non-quantitative)
weather_cleaned = select(weather_stats, -c(WindDir9am, WindDir3pm))

# Calculate Mode Function
getMode <- function(windDir) {
  uniqWindDir <- unique(windDir)
  uniqWindDir[which.max(tabulate(match(windDir, uniqWindDir)))]
}

# Replace missing values with Mode for WindGustDir (non-quantitative) columns
windGustDirMode = getMode(weather_cleaned$WindGustDir)
weather_cleaned$WindGustDir = ifelse(is.na(weather_cleaned$WindGustDir), windGustDirMode, weather_cleaned$WindGustDir)

# Replace missing values with Mean for other quantitative columns
for(i in 1:ncol(weather_cleaned)){
  weather_cleaned[is.na(weather_cleaned[,i]), i] = mean(weather_cleaned[,i], na.rm = TRUE)
}

# Find number of missing values for all columns (After pre-processing)
(colNA <- apply(weather_cleaned, 2, function(x) sum(is.na(x))))

# ----------------------------------------- Question 1 ----------------------------------------------------------------------------------------------
# What is the intensity of rainfall if it is certain to rain?
# ======================================== Analysis 1.1 ==============================================================================================

# Find the borderline value that separates different output
all.equal(weather_cleaned$RISK_MM > 0.5, weather_cleaned$RainTomorrow == "Yes")
all.equal(weather_cleaned$RISK_MM > 0.75, weather_cleaned$RainTomorrow == "Yes")
all.equal(weather_cleaned$RISK_MM > 1, weather_cleaned$RainTomorrow == "Yes")
all.equal(weather_cleaned$Rainfall > 0.5, weather_cleaned$RainToday == "Yes")
all.equal(weather_cleaned$Rainfall > 0.75, weather_cleaned$RainToday == "Yes")
all.equal(weather_cleaned$Rainfall > 1, weather_cleaned$RainToday == "Yes")

# ======================================== Analysis 1.2 ==============================================================================================

# Find the number of RISK_MM with value lower than 1 based on RainTomorrow using histogram
weather_cleaned %>%
  ggplot(aes(x = RISK_MM, fill = RainTomorrow)) + geom_histogram() +
  labs(title = "Distribution of RISK_MM based on RainTomorrow", y = "Number of occurrence")

# Find the number of Rainfall with value lower than 1 based on RainToday using histogram
weather_cleaned %>%
  ggplot(aes(x = Rainfall, fill = RainToday)) + geom_histogram() +
  labs(title = "Distribution of Rainfall based on RainToday", y = "Number of occurrence")

# ======================================== Analysis 1.3 ==============================================================================================

# Shows the count of each unique RISK_MM values (Intensity of rainfall) when tomorrow is raining.
weather_cleaned %>% 
  filter(RainTomorrow == "Yes") %>%
    group_by(RISK_MM) %>%
      summarise(count=n()) %>%
        print(n=41)

# ======================================== Analysis 1.4 ==============================================================================================

# Categorize based on intensity of rainfall
weather_cleaned = mutate(weather_cleaned, RainIntensity = case_when (RISK_MM <= 1 ~ "No Rain",
                                                                     RISK_MM > 1 & RISK_MM <= 10 ~ "Light",
                                                                     RISK_MM > 10 & RISK_MM <= 20 ~ "Moderate",
                                                                     RISK_MM > 20 ~ "Heavy"))

# Calculate number of cases based on rain intensity
weather_cleaned %>% 
  group_by(RainIntensity) %>%
  summarise(count=n())

# ---------------------------------------- Question 2 ----------------------------------------------------------------------------------------------
# What is the likelihood to rain tomorrow based on today's weather?
# ======================================= Analysis 2.1 ==============================================================================================

# Filter data to 4 case scenario
conseqRain = weather_cleaned %>% na.omit %>% filter(RainToday == "Yes" & RainTomorrow == "Yes")
noConseqRain = weather_cleaned %>% na.omit %>% filter(RainToday == "No" & RainTomorrow == "No")
Rr = weather_cleaned %>% na.omit %>% filter(RainToday == "Yes" & RainTomorrow == "No")
rR = weather_cleaned %>% na.omit %>% filter(RainToday == "No" & RainTomorrow == "Yes")

# Calculate percentage of occurrence 
proabilityConseqRain = nrow(conseqRain) / nrow(weather_cleaned) # ~ 0.058
proabilitynoConseqRain = nrow(noConseqRain) / nrow(weather_cleaned) # ~ 0.700
proabilityRr = nrow(Rr) / nrow(weather_cleaned) # ~ 0.123
proabilityrR = nrow(rR) / nrow(weather_cleaned) # ~ 0.123

# ======================================= Analysis 2.2 ==============================================================================================

# Prepare subset when tomorrow is raining
tmrRain = weather_cleaned %>% na.omit %>% filter(RainTomorrow == "Yes")

# Show number of rainy days tomorrow based on today's rainfall amount
tmrRain = weather_cleaned %>% na.omit %>% filter(RainTomorrow == "Yes")
tmrRain %>%
  ggplot(aes(x = RainToday, fill = RainTomorrow)) + geom_bar() + 
  geom_text(stat = "count", aes(label = after_stat(count))) + 
  labs(title = "Likelihood to rain tomorrow based on today's weather", y = "Number of occurrence")

# ======================================= Analysis 2.3 ==============================================================================================

# Probability to rain tomorrow in percentage shown in a pie chart
var = c(nrow(conseqRain), nrow(rR))
labelName = c("Raining", "Not raining")
pie_labels = paste0(labelName,"=", round(100*var/sum(var), 2), "%") 
pie3D(var,labels = pie_labels, radius = 0.8, main = "Probability to rain tomorrow when today is:")

# ----------------------------------------- Question 3 ----------------------------------------------------------------------------------------------
# What is the speed of wind for each geographical direction and its effect on Rainfall?
# ======================================= Analysis 3.1 ==============================================================================================

# Get the maximum speed of wind and maximum RISK_MM for each direction
print(
  weather_cleaned %>%
    group_by(WindGustDir) %>%
    summarise(MaxWindSpeed = max(WindGustSpeed), Max_RISK_MM = max(RISK_MM)))

# ======================================= Analysis 3.2 ==============================================================================================

# Data Visualization
weather_cleaned %>%
  ggplot(aes(x = WindGustSpeed, fill = RainTomorrow)) + geom_histogram() + facet_wrap(~ WindGustDir) +
  labs(title = "Wind Gust Speed for each direction", y = "Number of occurrence")

# ----------------------------------------- Question 4 ----------------------------------------------------------------------------------------------
# What are the early signs of rainfall?
# ======================================= Analysis 4.1 ==============================================================================================

# Well separated cases
weather_cleaned %>%
  ggplot(aes(y = Humidity3pm, x = RainTomorrow)) + geom_boxplot()

weather_cleaned %>%
  ggplot(aes(y = Cloud3pm, x = RainTomorrow)) + geom_boxplot()

weather_cleaned %>%
  ggplot(aes(y = Sunshine, x = RainTomorrow)) + geom_boxplot()

# ======================================= Analysis 4.2 ==============================================================================================

# Not Well separated cases
weather_cleaned %>%
  ggplot(aes(y = MaxTemp, x = RainTomorrow)) + geom_boxplot()

weather_cleaned %>%
  ggplot(aes(y = Evaporation, x = RainTomorrow)) + geom_boxplot()

# ----------------------------------------- Question 5 ----------------------------------------------------------------------------------------------
# How does the distribution of Sunshine and Clouds affect Rainfall?
# ======================================= Analysis 5.1 ==============================================================================================

# Distribution of Sunshine and Cloud@3pm
weather_cleaned %>%
  ggplot(aes(x = Cloud3pm, y = Sunshine)) + geom_count() + facet_wrap(~ RainTomorrow) +
  labs(title = "Distribution of Sunshine and Cloud3pm")

# ======================================= Analysis 5.2 ==============================================================================================

# Inverse Proportional Relationship
weather_cleaned %>%
  ggplot(aes(x = Cloud3pm, col = RainTomorrow)) + geom_density() +
  labs(title = "Fluctuation of Cloud3pm based on RainTomorrow")
weather_cleaned %>%
  ggplot(aes(x = Sunshine, col = RainTomorrow)) + geom_density() +
  labs(title = "Fluctuation of Sunshine based on RainTomorrow")

# ======================================= Analysis 5.3 ==============================================================================================

# Declining/Downfall Relationship
tmrRain %>%
  ggplot(aes(x = Sunshine, y = Cloud3pm)) + geom_line() + geom_smooth(method = lm) + 
  labs(title = "Relationship between Sunshine and Cloud3pm")

# ----------------------------------------- Question 6 ----------------------------------------------------------------------------------------------
# How does Humidity and Temperature affect Rainfall?
# ======================================= Analysis 6.1 ==============================================================================================

# Determine Relationship between Humidity and Temperature at 3PM
weather_cleaned %>%
  ggplot(aes(x = Humidity3pm, y = Temp3pm)) + geom_point() + geom_smooth() + 
  labs(title = "Relationship between Humidity and Temperature3pm")

# ======================================= Analysis 6.2 ==============================================================================================

# Find the average, first quartile and third quartile of humidity & temperature at 3PM
weather_cleaned %>%
  filter(RainTomorrow == "Yes") %>%
  summarise(HumidityAVG = mean(Humidity3pm), HumidityQ1 = quantile(Humidity3pm, 0.25), HumidityQ3 = quantile(Humidity3pm, 0.75), 
            TempAVG = mean(Temp3pm), TempQ1 = quantile(Temp3pm, 0.25), TempQ3 = quantile(Temp3pm, 0.75))

# ======================================= Analysis 6.3 ==============================================================================================

# Find the RISK_MM when the humidity is greater than Q3, and temperature is lesser than Q1
weather_cleaned %>%
  filter(Humidity3pm > 74, Temp3pm < 15) %>%
  select(Humidity3pm, Temp3pm, RISK_MM)

# ----------------------------------------- Question 7 ----------------------------------------------------------------------------------------------
# How does Pressure change over time based on rainfall?
# ======================================= Analysis 7.1 ==============================================================================================

# Asymmetrical distribution
weather_cleaned %>%
  ggplot(aes(x = Pressure3pm, col = RainTomorrow)) + geom_density() +
  labs(title = "Fluctuation of Pressure3pm based on tomorrow's weather")

# ======================================= Analysis 7.2 ==============================================================================================

# Find the median, first quartile and third quartile of pressure and wind speed 
tmrRain %>%
  summarise(PressureMedian = quantile(Pressure3pm, 0.50), PressureQ1 = quantile(Pressure3pm, 0.25), 
            PressureQ3 = quantile(Pressure3pm, 0.75), WindSpeedMedian = quantile(WindGustSpeed, 0.50), 
            WindSpeedQ1 = quantile(WindGustSpeed, 0.25), WindSpeedQ3 = quantile(WindGustSpeed, 0.75))

# ======================================= Analysis 7.3 ==============================================================================================

# Find the RISK_MM when the pressure is lesser than Q1, and wind speed is greater than Q3
print(
  tmrRain %>%
    filter(Pressure3pm < 1008, WindGustSpeed > 59) %>%
    select(Pressure3pm, WindGustSpeed, RISK_MM))

# ----------------------------------------- Question 8 ----------------------------------------------------------------------------------------------
# How likely is it to rain after a hot day?
# ======================================= Analysis 8.1 ==============================================================================================

# Prepare subset when today is raining
tdyRain = weather_cleaned %>% na.omit %>% filter(RainToday == "Yes")

# Evaporation rate when today is raining
tdyRain %>%
  ggplot(aes(x = Evaporation, fill = RainToday)) + geom_freqpoly() +
  labs(title = "Evaporation rate when today is raining", y = "Number of occurrence")

# ======================================= Analysis 8.2 ==============================================================================================

# Find the average value for Evaporation and Sunshine to be used as filter for next analysis
tdyRain %>%
  summarise(EvaporationMean = mean(Evaporation), SunshineMean = mean(Sunshine))

# Find the minimum and maximum value of temperature and evaporation
tdyRain %>%
  summarise(min(MaxTemp), max(MaxTemp))
tdyRain %>%
  summarise(MinEvaporation = min(Evaporation), MaxEvaporation = max(Evaporation))

# ======================================= Analysis 8.3 ==============================================================================================

# Create a new column to label Temperate Range and Evaporation Range
tdyRain = mutate(tdyRain, TempRange = case_when (MaxTemp >= 8.4 & MaxTemp <= 13 ~ "Low",
                                                 MaxTemp > 13 & MaxTemp <= 27 ~ "Medium",
                                                 MaxTemp > 27 & MaxTemp <= 33.8 ~ "High"))

tdyRain = mutate(tdyRain, EvaporationRange = case_when (Evaporation >= 0.2 & Evaporation < 3.7 ~ "Low",
                                                        Evaporation >= 3.7 & Evaporation < 7.2 ~ "Medium",
                                                        Evaporation >= 7.2 & Evaporation <= 10.2 ~ "High"))

# ======================================= Analysis 8.4 ==============================================================================================

# Observe the Temperature Range and Evaporation Range when the Evaporation Rate and Sunshine is above the average value
print(
  tdyRain %>%
    filter(Evaporation > 5, Sunshine > 7) %>%
    select(MaxTemp, Evaporation, Rainfall, TempRange, EvaporationRange))

# ======================================= Analysis 8.5 ==============================================================================================

# Calculate the Rainfall when both the temperature and evaporation rate is high
print(
  tdyRain %>%
    filter(TempRange == "High", EvaporationRange == "High") %>%
    select(Rainfall))
