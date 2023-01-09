# Prepare Environment -----------------------------------------------------
library(ggplot2)
library(dplyr)
library(tidyr)
library(patchwork)

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
head(dat$Date, 10)
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


# rain summary stats ------------------------------------------------------
raindat <- dat %>% filter(Parameter == "Rain") %>%  
  group_by(Year, Month, mDay, yDay) %>% 
  summarize(
    `Daily Precip (mm)` = sum(value)
  ) %>% ungroup()

raindat
max(raindat$`Daily Precip (mm)`)
dim(raindat)

raindat <- raindat %>% group_by(Year) %>% 
  mutate(
    `Cumulative Precip (mm)` = cumsum(`Daily Precip (mm)`)
  ) %>% ungroup()

raindat <- pivot_longer(
  raindat, 
  cols = c(`Daily Precip (mm)`, `Cumulative Precip (mm)`), 
  names_to = "Parameter",
  values_to = "value"
)

raindat %>% filter(Parameter == quote(`Cumulative Precip (mm)`)) %>%
  group_by(Year) %>% 
  summarise(annual_total_rainfall = max(value)) %>% 
  ungroup() %>% as.data.frame()

# # plot rain/precip --------------------------------------------------------
# (prain.combined <- ggplot(
#   data = raindat,
#   aes(
#     x = yDay,
#     y = value,
#     color = Year
#   )
# ) +
#   theme_bw() +
#   # geom_hline(yintercept = 1167.0) +
#   # geom_point() +
#   # geom_path() +
#   geom_smooth(se = FALSE, method = "loess", size = 0.5) +
#   facet_grid(Parameter ~ ., scales = "free_y") +
#   coord_cartesian(xlim = c(0, 366)) +
#   theme(
#     axis.title.y = element_blank(),
#     legend.position = "bottom"
#   ) +
#   xlab("Day of the Year")
# )
# 
# # ggsave(
# #   filename = file.path(getwd(), "figs", "weatherstation_rain_smooth.pdf"),
# #   prain.combined,
# #   width = 8.5, height = 2*8.5+2, units = "cm"
# # )


# temperature -------------------------------------------------------------
tempdat <- dat %>% filter(Parameter == "Temp") %>% 
  group_by(Year, Month, mDay, yDay) %>% 
  summarize(
    Parameter = "Temperature (deg C)",
    value = mean(value)
  ) %>% ungroup()

tempdat %>% group_by(Year) %>% 
  summarise(yearly_mean = mean(value)) %>% 
  ungroup %>% as.data.frame()

# plot individually -------------------------------------------------------
(ptemp <- ggplot(
  data = tempdat,
  aes(
    x = yDay,
    y = value,
    color = Year
  )
) +
  theme_bw() +
  geom_smooth(se = FALSE, method = "loess", size = 0.5, show.legend = FALSE) +
  # facet_grid(Parameter ~ ., scales = "free_y") +
  coord_cartesian(xlim = c(0, 366)) +
  # theme(
  #   # axis.title.y = element_blank(),
  #   legend.position = "bottom"
  # ) +
  xlab("Day of the Year") +
  ylab("Temperature (deg C)")
)

(pcumrain <- ggplot(
  data = raindat %>% filter(Parameter == quote(`Cumulative Precip (mm)`)), 
  aes(
    x = yDay,
    y = value,
    color = Year
  )
) +
    theme_bw() +
    geom_point(size = 0.75, alpha = 1) +
    geom_path() +
    # geom_smooth(se = FALSE, method = "loess", size = 0.5) +
    # facet_grid(Parameter ~ ., scales = "free_y") +
    coord_cartesian(xlim = c(0, 366)) +
    # theme(
    #   # axis.title.y = element_blank(),
    #   legend.position = "bottom"
    # ) +
    xlab("Day of the Year") +
    ylab("Cumulative Precip (mm)")
)

(pdailyrain <- ggplot(
  data = raindat %>% filter(Parameter == quote(`Daily Precip (mm)`)), 
  aes(
    x = yDay,
    y = value,
    color = Year
  )
) +
    theme_bw() +
    # geom_point(alpha = 0.5) +
    # geom_path() +
    geom_smooth(se = FALSE, method = "loess", size = 0.5, show.legend = FALSE) +
    # facet_grid(Parameter ~ ., scales = "free_y") +
    coord_cartesian(xlim = c(0, 366)) +
    # theme(
    #   # axis.title.y = element_blank(),
    #   legend.position = "bottom"
    # ) +
    xlab("Day of the Year") +
    ylab("Daily Precip (mm)")
)

# join plots --------------------------------------------------------------
ptemp +
  pcumrain +
  pdailyrain +
  plot_layout(guides = "collect", ncol = 1) & 
  theme(
    legend.position = "bottom", 
    legend.title = element_blank()
  )

ggsave(
  filename = file.path(getwd(), "figs", "weatherstation.pdf"),
  width = 8.5, height = 24, units = "cm"
)

# # merge rain and temperature datasets -------------------------------------
# raindat
# tempdat
# 
# rain.temp <- rbind(raindat, tempdat)
# 
# # plot both datasets ------------------------------------------------------
# (p.rain.temp <- ggplot(
#   data = rain.temp, 
#   aes(
#     x = yDay, 
#     y = value, 
#     color = Year
#   )
# ) +
#   theme_bw() +
#   geom_smooth(se = FALSE, method = "loess", size = 0.5) +
#   facet_grid(Parameter ~ ., scales = "free_y") +
#   coord_cartesian(xlim = c(0, 366)) +
#   theme(
#     axis.title.y = element_blank(),
#     legend.position = "bottom"
#   ) +
#   xlab("Day of the Year")
# )
# 
# ggsave(
#   filename = file.path(getwd(), "figs", "weatherstation.pdf"),
#   p.rain.temp, 
#   width = 8.5, height = 24, units = "cm"
# )


# export precipitation summary --------------------------------------------
write.csv(
  raindat, 
  file.path(getwd(), "output", "raindat.csv")
)
write.csv(
  tempdat, 
  file.path(getwd(), "output", "temperaturedat.csv")
)

# # Dew Point ---------------------------------------------------------------
# dpdat <- dat %>% filter(Parameter == "Dew_Point") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Dew_Point")
# 
# # gust speed --------------------------------------------------------------
# gsdat <- dat %>% filter(Parameter == "Gust_Speed") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Gust_Speed")
# 
# # pressure ----------------------------------------------------------------
# pdat <- dat %>% filter(Parameter == "Pressure") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Pressure")
# 
# # rain --------------------------------------------------------------------
# raindat <- dat %>% filter(Parameter == "Rain") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(Daily_Precip = sum(value)) %>% ungroup()
# 
# raindat
# max(raindat$Daily_Precip)
# dim(raindat)
# 
# raindat <- raindat %>% group_by(Year) %>% 
#   mutate(
#     Cumulative_Precip = cumsum(Daily_Precip), 
#     var_group = "Precip"
#   ) %>% ungroup()
# 
# # rel humidity ------------------------------------------------------------
# rhdat <- dat %>% filter(Parameter == "Rel_Humidity") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Rel_Humidity")
# 
# # Solar Radiation ---------------------------------------------------------
# srdat <- dat %>% filter(Parameter == "Solar_Radiation") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Solar_Radiation")
# 
# # Temp --------------------------------------------------------------------
# tdat <- dat %>% filter(Parameter == "Temp") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Temp")
# 
# # Wind Speed --------------------------------------------------------------
# wsdat <- dat %>% filter(Parameter == "Wind_Speed") %>%  
#   group_by(Year, Month, mDay, yDay) %>% 
#   summarize(
#     Mean = mean(value),
#     Min = min(value),
#     Max = max(value)
#   ) %>% ungroup() %>% 
#   mutate(var_group = "Wind_Speed")
# 
# 
# # join individual datasets back together.  --------------------------------
# # pivot_longer(
# #   dpdat, 
# #   cols = ends_with(c("_Mean", "_Min", "_Max")), 
# #   names_to = "Parameter", 
# #   values_to = "value"
# # )
# # pivot_longer(
# #   raindat,
# #   cols = ends_with(c("_Mean", "_Min", "_Max")),
# #   names_to = "Parameter",
# #   values_to = "value"
# # )
# # lapply(
# #   X = list(dpdat, gsdat, pdat, rhdat, srdat, tdat, wsdat), 
# #   names
# # )
# 
# sumdat_ls <- vector(mode = "list", length = length(list(dpdat, gsdat, pdat, rhdat, srdat, tdat, wsdat)))
# 
# sumdat_ls <- lapply(
#   X = list(dpdat, gsdat, pdat, rhdat, srdat, tdat, wsdat), 
#   FUN = function(i) {
#     # print(i)
#     pivot_longer(
#       i, 
#       cols = ends_with(c("Mean", "Min", "Max")), 
#       names_to = "Parameter", values_to = "value"
#     )
#   }
# )
# 
# sumdat <- do.call("rbind", sumdat_ls)
# 
# # raindat
# # raindat_long <- pivot_longer(
# #   raindat, 
# #   cols = c("Daily_Precip", "Cumulative_Precip"), 
# #   names_to = "Parameter", values_to = "value"
# # )
# # sumdat <- rbind(sumdat, raindat_long)
# 
# # plot all ----------------------------------------------------------------
# ggplot(
#   data = sumdat, 
#   aes(
#     x = mDay, 
#     y = value,
#     # shape = Year,
#     col = Year
#   )
# ) +
#   theme_bw() +
#   geom_point(alpha = 0.25) +
#   # geom_smooth(se = FALSE) +
#   # facet_grid(var_group ~ ., scales = "free_y")
#   facet_grid(Parameter ~ ., scales = "free_y")
#   
# 
# 
# 
# # # daily stats -------------------------------------------------------------
# # dailydat <- dat %>% group_by(Year, Month, mDay, yDay, Parameter) %>% 
# #   summarize(
# #     Mean = mean(value),
# #     Min = min(value),
# #     Max = max(value)
# #   ) %>% ungroup()
# # 
# # ggplot(
# #   data = dailydat, 
# #   aes(
# #     x = Day, 
# #     y = 
# #   )
# # )
# 
# 
# 
# 
# 
# 
