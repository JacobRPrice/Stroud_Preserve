# prepare environment -----------------------------------------------------
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(patchwork)
library(scales)

# load measured data ------------------------------------------------------
dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

sampling_dates <- unique(dat$Date)

# load sms data -----------------------------------------------------------
smsfiles <- list.files(
  path = file.path(getwd(), "data", "/SMS/"), 
  pattern = ".TXT",
  ignore.case = TRUE,
  full.names = TRUE,
  recursive = TRUE 
)

smsfunc <- function(
  sms.file
) {
  # read in file
  tsdat <- read.csv(
    sms.file,
    skip = 1, header = TRUE, sep = ","
  )
  # convert to date time class
  tsdat$DateTime_EST <- lubridate::ymd_hms(
    as.POSIXct(tsdat$DateTime_EST, format = "%Y-%m-%d %H:%M"), 
    tz = "Etc/GMT-5"
  )
  # obtain date
  tsdat$Date <- lubridate::date(tsdat$DateTime_EST)
  
  # remove any NA values
  tsdat <- tsdat[!is.na(tsdat$Date),]
  
  # make sure data is in correct format(s)
  tsdat$Segment1_percent <- as.numeric(tsdat$Segment1_percent)
  
  # only keep variables we're interested in 
  tsdat <- tsdat[, c("DateTime_EST", "Segment1_percent", "Date")]
  
  # append field data
  tsdat$Field <- substr(basename(sms.file), 1, 2)
  tsdat$FieldSub <- substr(basename(sms.file), 1, 3)
  tsdat$Sub <- substr(basename(sms.file), 3, 3)
  
  # return
  if (dim(tsdat)[1] == 0) {
    return(NULL)
  } else { return(tsdat) }
}

resls <- vector(mode = "list", length = length(smsfiles))
resls <- parallel::mclapply(
  X = smsfiles, 
  FUN = smsfunc, 
  mc.cores = parallel::detectCores() - 1
)

# check for errors
which(unlist(lapply(X = resls, FUN = is.null)))
basename(smsfiles[which(unlist(lapply(X = resls, FUN = is.null)))])

# rbind the remaining into a single dataframe
smsdat <- plyr::ldply(resls, rbind, .id = NULL)
str(smsdat)

# # tabulate observations
# table(smsdat$Date, smsdat$FieldSub)

# any NA's?
names(smsdat)
which(is.na(smsdat$Segment1_percent))
which(is.na(smsdat$DateTime_EST))
which(is.na(smsdat$Date))
# nope! 

# look at data ranges really quick ----------------------------------------
summary(smsdat$Segment1_percent)

# # heads up: this plots *a lot* of data
# ggplot(
#   data = smsdat, 
#   aes(
#     x = as.Date(DateTime_EST), 
#     y = Segment1_percent, 
#     color = Field, 
#     shape = Sub
#   )
# ) +
#   theme_bw() +
#   geom_vline(xintercept = as.Date("2021-01-01")) +
#   geom_point(alpha = 0.05) +
#   scale_x_date(
#     date_labels = "%b/%y",
#     limits = as.Date(c("2020-01-01", "2021-12-31")), 
#     date_breaks = "3 months", 
#     minor_breaks = "1 month"
#   )

# field 32A was consistently above 99% every day from 6/4/2020 to 7/28/2020
with(
  smsdat[which(smsdat$Segment1_percent>=99),], 
  table(Date, FieldSub)
)
subset(smsdat, FieldSub == "32A")[, c(1,2, 3, 5)]
# Only one sensor experienced this error, but the other observations behave similarly to the other sensors
# remove all of the error observations
smsdat <- smsdat[smsdat$Segment1_percent < 99,]

# # Save smsdat for correlation.R
# saveRDS(
#   object = smsdat, 
#   file = file.path(getwd(), "data", "SMS_dat.RDS")
# )

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# now weather station data ------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

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

wdatls <- vector(mode = "list", length = length(filels))
wdatls <- lapply(
  X = filels, 
  FUN = function(i) {
    nam <- colnames(i)
    
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

str(wdatls[[1]])
str(wdatls[[2]])

# convert to long format and join dataframes ------------------------------
wdatls <- lapply(
  X = wdatls, 
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
head(wdatls)

# join individual dataframes
wdat <- plyr::ldply(wdatls, rbind, .id = NULL)
str(wdat)

# handle date info --------------------------------------------------------
# convert date to an actual date/time format
head(wdat$Date, 10)
wdat$Date <- as.POSIXct(wdat$Date, tz = "Etc/GMT-5", format = "%m/%d/%y %H:%M:%S")

# any dates incorrectly formatted? 
summary(wdat$Date) 
which(is.na(wdat$Date))

# calculate date statistics
wdat$Year <- lubridate::year(wdat$Date)
wdat$Year <- as.factor(wdat$Year)
wdat$Month <- lubridate::month(wdat$Date)
wdat$mDay <- lubridate::mday(wdat$Date)
wdat$yDay <- lubridate::yday(wdat$Date)

# rename (merge) parameters -----------------------------------------------
str(wdat)
sort(unique(wdat$Parameter))
wdat$Parameter <- gsub("[1-9]", "", wdat$Parameter)
sort(unique(wdat$Parameter))

# rain summary stats ------------------------------------------------------
# get daily precipitation
raindat <- wdat %>% filter(Parameter == "Rain") %>%  
  group_by(Year, Month, mDay, yDay) %>% 
  summarize(
    `Daily Precip (mm)` = sum(value)
  ) %>% ungroup()

raindat
max(raindat$`Daily Precip (mm)`)
dim(raindat)

# get cumulative precipitation 
raindat <- raindat %>% group_by(Year) %>% 
  mutate(
    `Cumulative Precip (mm)` = cumsum(`Daily Precip (mm)`)
  ) %>% ungroup()
raindat

# pivot into longer format
raindat <- pivot_longer(
  raindat, 
  cols = c(`Daily Precip (mm)`, `Cumulative Precip (mm)`), 
  names_to = "Parameter",
  values_to = "value"
)

# obtain total annual rainfall
raindat %>% filter(Parameter == quote(`Cumulative Precip (mm)`)) %>%
  group_by(Year) %>% 
  summarise(annual_total_rainfall = max(value)) %>% 
  ungroup() %>% as.data.frame()

# how many days without rain?
raindat %>% filter(Parameter == quote(`Daily Precip (mm)`)) %>% 
  group_by(Year) %>% 
  summarise(days_w_no_rain = sum(value == 0))

# # save rain dataframe
# write.csv(
#   raindat, 
#   file.path(getwd(), "output", "raindat.csv")
# )

# temperature -------------------------------------------------------------
tempdat <- wdat %>% filter(Parameter == "Temp") %>% 
  group_by(Year, Month, mDay, yDay) %>% 
  summarize(
    Parameter = "Temperature (deg C)",
    value = mean(value)
  ) %>% ungroup()

# get average daily temperature 
tempdat %>% group_by(Year) %>% 
  summarise(yearly_mean = mean(value)) %>% 
  ungroup %>% as.data.frame()

# # save temperature dataframe
# write.csv(
#   tempdat, 
#   file.path(getwd(), "output", "temperaturedat.csv")
# )

# -------------------------------------------------------------------------
# -------------------------------------------------------------------------
# plots -------------------------------------------------------------------
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

(ptemp <- ggplot(
  data = tempdat,
  aes(
    x = yDay,
    y = value,
    color = Year
  )
) +
  theme_bw() +
  geom_smooth(
    se = FALSE, method = "loess", linewidth = 0.5, show.legend = FALSE
  ) +
  coord_cartesian(xlim = c(0, 366)) +
  xlab("Day of the Year") +
  ylab("Temperature [deg C]")
)




(pcumprecip <- ggplot(
  data = raindat %>% filter(Parameter == quote(`Cumulative Precip (mm)`)), 
  aes(
    x = yDay,
    y = value,
    color = Year
  )
) +
    theme_bw() +
    geom_path(show.legend = FALSE) +
    coord_cartesian(xlim = c(0, 366)) +
    xlab("Day of the Year") +
    ylab("Cumulative Precip [mm]")
)

###
# plot combining daily precip and soil moisture results
###

# calculate daily means
# for plotting daily soil moisture over time 
sdat.collapseall <- smsdat %>% group_by(Date) %>% 
  summarize(
    Soil_Moisture = mean(Segment1_percent)
  )
sdat.collapseall$Year <- factor(year(sdat.collapseall$Date))
sdat.collapseall$yDay <- lubridate::yday(sdat.collapseall$Date)

# calculate field*daily means 
#   using soil moisture observations from 8 am - 12 pm (noon)
#   for violin plots on sampling dates
sdat <- smsdat %>% group_by(Date, Field) %>% 
  filter(
    (hour(DateTime_EST) >= 8) & (hour(DateTime_EST) < 12) 
  ) %>% summarize(
    Soil_Moisture = mean(Segment1_percent)
  ) %>% ungroup()


# Manipulate raindat
raindat <- raindat %>% mutate(
  Date = as.Date(paste0(Year, "-", Month, "-", mDay))
)
raindat.wide <- pivot_wider(
  raindat,
  names_from = Parameter, 
  values_from = value
)

raindat
str(smsdat)
sdat
sdat.collapseall

# create groups to split up contiguous soil moisture data sections 
(idx <- c(1, diff(sdat.collapseall$Date)))
(i2 <- c(1,which(idx != 1), nrow(sdat.collapseall)+1))
as.data.frame(sdat.collapseall)[76:78,]
as.data.frame(sdat.collapseall)[356:358,]
as.data.frame(sdat.collapseall)[503:505,]
sdat.collapseall$grp <- rep(1:length(diff(i2)), diff(i2))

# transformation parameters
trans <- compose_trans("reverse")
scale <- 3
translate <- 250

# create plot
(p.precip.moisture <- ggplot(
  data = raindat.wide, 
  aes(x = Date)
) + 
    theme_bw() +
    # plot daily precip
    geom_col(
      aes(y = `Daily Precip (mm)`), 
      color = "blue"
    ) +
    # plot violin plots for each sampling date
    geom_violin(
      data = sdat %>% filter(Date %in% sampling_dates), 
      aes(
        x = Date, 
        y = trans$transform(Soil_Moisture) * scale + translate, 
        group = Date
      ),
      color = "black",
      show.legend = FALSE
    ) +
    # plot average daily soil moisture
    geom_line(
      data = sdat.collapseall, 
      aes(
        x = Date,
        y = trans$transform(Soil_Moisture) * scale + translate, 
        color = Year, 
        group = grp
      )
    ) +
    # implement secondary axis 
    scale_y_reverse(
      sec.axis = sec_axis(
        ~ trans$inverse((.x - translate) / scale), 
        "Soil Moisture [%]", 
        breaks = seq(0, 100, 25)
      )
    ) +
    scale_x_date(
      date_labels = "%b/%y", 
      minor_breaks = "1 month"
    ) +
    ylab("Daily Precip [mm]")
)

(ptemp + pcumprecip) / p.precip.moisture &
  plot_layout(guides = "collect") & theme(
    legend.position = "bottom"
  )

# ggsave(
##   filename = file.path(getwd(), "figs", "Weather_&_SoilMoisture.pdf"),
#   filename = file.path(getwd(), "figs", "Figure_2.pdf"),
#   width = 18, height = 18, units = "cm"
# )
