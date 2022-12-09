# Prepare Environment -----------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)

# import data -------------------------------------------------------------
weather_files <- c("Weather_Station_2020.csv", "Weather_Station_2021.csv")

filels <- vector(mode = "list", length = length(weather_files))
filels <- lapply(
  X = weather_files, 
  FUN = function(i) {
    read.csv(
      file = file.path(getwd(), "data", i) ,
      header = TRUE
      
    )
  }
)

# cleanup variable names --------------------------------------------------
str(filels[[1]])
str(filels[[2]])

colnames(filels[[1]]) <- c(
  "Line",
  "Date",
  "Temp1",
  "Pressure1",
  "Dew_Point1",
  "Rain",
  "Rel_Humidity",
  "Solar_Radiation1",
  "Wind_Direction1",
  "Wind_Speed",
  "Gust_Speed",
  "Temp2",
  "Dew_Point2",
  "Solar_Radiation2",
  "Wind_Direction2",
  "Dew_Point3"
)
colnames(filels[[2]]) <- c(
  "Line",
  "Date",
  "Temp",
  "Pressure",
  "Dew_Point",
  "Rain",
  "Rel_Humidity",
  "Solar_Radiation",
  "Wind_Direction",
  "Wind_Speed",
  "Gust_Speed"
)

# fix data types
str(filels[[1]])
str(filels[[2]])

datls <- vector(mode = "list", length = length(filels))
datls <- lapply(
  X = filels, 
  FUN = function(i) {
    nam <- colnames(i)
    
    # as.POSIXct(head(dat$Date), format = "%m/%d/%y %H:%M:%S")
    # i$Date <- as.POSIXct(i$Date, format = "")
    
    if ("Pressure" %in% nam) {
      i$Pressure <- as.numeric(gsub(",","", i$Pressure))
    }
    if ("Pressure1" %in% nam) {
      i$Pressure1 <- as.numeric(gsub(",","", i$Pressure1))
    }
    if ("Pressure2" %in% nam) {
      i$Pressure2 <- as.numeric(gsub(",","", i$Pressure2))
    }
    
    if ("Solar_Radiation" %in% nam) {
      i$Solar_Radiation <- as.numeric(i$Solar_Radiation)
    }
    if ("Solar_Radiation1" %in% nam) {
      i$Solar_Radiation1 <- as.numeric(i$Solar_Radiation1)
    }
    if ("Solar_Radiation2" %in% nam) {
      i$Solar_Radiation2 <- as.numeric(i$Solar_Radiation2)
    }
    
    if ("Wind_Direction" %in% nam) {
      i$Wind_Direction <- as.numeric(i$Wind_Direction)
    }
    if ("Wind_Direction1" %in% nam) {
      i$Wind_Direction1 <- as.numeric(i$Wind_Direction1)
    }
    if ("Wind_Direction2" %in% nam) {
      i$Wind_Direction2 <- as.numeric(i$Wind_Direction2)
    }
    return(i)
  }
)

str(datls[[1]])
str(datls[[2]])


# convert to long format and join dataframes ------------------------------
datls <- lapply(
  X = datls, 
  FUN = function(i) {
    # make long
    tmpdat <- i %>% pivot_longer(
      cols = colnames(i)[-c(1,2)], 
      names_to = "Parameter", 
      values_to = "value"
    )
    
    # remove zero entries
    tmpdat <- tmpdat %>% drop_na(value)
    
    return(tmpdat)
  }
)
head(datls)


# join individual dataframes
dat <- plyr::ldply(datls, rbind, .id = NULL)
str(dat)


# handle date info --------------------------------------------------------
# convert date to an actual date/time format
head(dat$Date)
# as.POSIXct(head(dat$Date), format = "%m/%d/%y %H:%M:%S")
# lubridate::year(as.POSIXct(head(dat$Date), format = "%m/%d/%y %H:%M:%S"))
# lubridate::month(as.POSIXct(head(dat$Date), format = "%m/%d/%y %H:%M:%S"))
# lubridate::day(as.POSIXct(head(dat$Date), format = "%m/%d/%y %H:%M:%S"))

dat$Date <- as.POSIXct(dat$Date, tz = "Etc/GMT-5", format = "%m/%d/%y %H:%M:%S")

# any dates incorrectly formatted? 
summary(dat$Date) 
which(is.na(dat$Date))

# calculate date statistics
dat$Year <- lubridate::year(dat$Date)
dat$Year <- as.factor(dat$Year)
dat$Month <- lubridate::month(dat$Date)
dat$mDay <- lubridate::mday(dat$Date)
dat$yDay <- lubridate::yday(dat$Date)

# rename (merge) parameters -----------------------------------------------
str(dat)
sort(unique(dat$Parameter))
dat$Parameter <- gsub("[1-9]", "", dat$Parameter)
sort(unique(dat$Parameter))


# daily rain totals -------------------------------------------------------
raindat <- dat %>% filter(Parameter == "Rain") %>%  
  group_by(Year, Month, mDay, yDay) %>% 
  summarize(Daily_Precip = sum(value)) %>% ungroup()

max(raindat$Daily_Precip)

# plot rain/precip --------------------------------------------------------
ggplot(
  data = raindat, 
  aes(
    x = yDay, 
    y = Daily_Precip, 
    color = Year, 
    shape = Year
  )
) +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_point(alpha = 0.25) +
  # geom_smooth() +
  geom_smooth(se = FALSE, method = "loess") +
  coord_cartesian(
    # ylim = c(0, max(raindat$Daily_Precip)),
    # ylim = c(0, 10),
    xlim = c(0, 366)
  ) +
  theme(
    legend.position = "bottom"
  ) +
  ylab("Daily Precipitation (mm)") +
  xlab("Day of the Year")

ggsave(
  filename = file.path(getwd(), "figs", "weatherstation_rain_smooth.pdf"),
  width = 3, height = 7, units = "in"
)

# export precipitation summary --------------------------------------------
write.csv(
  raindat, 
  file.path(getwd(), "output", "raindat.csv")
)


