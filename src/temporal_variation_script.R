# collect arguments -------------------------------------------------------
args = commandArgs(trailingOnly=TRUE)
tvar <- args[1]

tvar 
# class(tvar)

# -------------------------------------------------------------------------
# Only for Running Manually -----------------------------------------------
# -------------------------------------------------------------------------
# this section is only for running the script manually, for both building/writing and debugging. 
# tvar <- "netMIN"
# tvar <- "qPCR"
# tvar <- "MinNit"
# tvar <- "EEA"
# -------------------------------------------------------------------------
# -------------------------------------------------------------------------

# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)
# library(openxlsx)
# library(rstatix)
# library(ggpubr)
# library(broom)
# library(tidyverse)
# library(corrplot)
# library(MASS)
dat <- 
  readRDS(file.path(getwd(), "/data/", "dat.RDS"))


# prepare data ------------------------------------------------------------
# str(dat)


# get year and month/day
dat$Year <- lubridate::year(dat$Date)
dat$Year <- as.factor(dat$Year)
dat$Month <- lubridate::month(dat$Date)
dat$Day <- lubridate::yday(dat$Date)


# if qPCR argument is being passed, we must take the log of the values 
if (tvar == "qPCR") {
  dat$AOA <- log10(dat$AOA)
  dat$AOB <- log10(dat$AOB)
  dat$nosZ <- log10(dat$nosZ)
}

# restructure data
str(dat)
dat <- pivot_longer(
  data = dat, 
  cols = names(dat)[-c(1:6, 22:24)], 
  names_to = "Parameter"
)
str(dat)




# subset data to just the group/type being passed
if (tvar == "qPCR") {
  dat <- subset(
    dat,
    Parameter %in% c("AOA", "AOB", "nosZ")
  )
} else if (tvar == "NitMin") {
  dat <- subset(
    dat,
    Parameter %in% c("netMIN", "netNIT", "NH4Nstock", "NO3Nstock")
  )
} else if (tvar == "EEA") {
  dat <- subset(
    dat,
    Parameter %in% c(
      "OM_percent", "Moisture_percent", 
      "GLU_gSoil", "NAG_gSoil", "PHO_gSoil"#, 
      # "GLU_gOM", "NAG_gOM", "PHO_gOM"
    )
  )
}
str(dat)

# # remove NA entries
dim(dat)
dat <- dat %>% drop_na(value) 
dim(dat)

# plot --------------------------------------------------------------------

(p <- ggplot(
  data = dat, 
  aes(
    # x = Date, 
    x = Day,
    y = value, 
    # group = Treatment_Group, 
    color = Treatment_Group, 
    # linetype = as.factor(Year) # I need to figure out how to have two trendlines, one for each year in this plot. 
    linetype = Year
  )
) +
  theme_bw() +
  facet_grid(Parameter~., scales = "free_y") +
  # scale_x_date(
  #   date_labels = "%m/%y",
  #   limits = c(as.Date("2020-01-01"), as.Date("2021-12-31")),
  #   breaks = scales::date_breaks("2 months")
  # ) +
  geom_point(alpha = 0.5) +
  geom_smooth(se = FALSE) #+
  # # geom_smooth() +
  # # stat_smooth(geom = "line") +
  # # stat_smooth(geom = "line", alpha = 0.5) +
  # theme(
  #   # legend.position = "bottom",
  #   axis.title.x = element_blank(),
  #   axis.title.y = element_text(size = rel(.8)),
  #   axis.text.x = element_text(size = rel(.8)),
  #   axis.text.y = element_text(
  #     size = rel(.8), 
  #     angle = 90, hjust = +0.5, vjust = 1)
  # ) #+
  # # labs(
  # #   y = "Enzyme Activity [umol/hr/g Soil]"
  # # ) +
  # # facet_grid(Parameter~., scales = "free_y")
)

ggsave(
  file.path(getwd(), "figs", paste0("seasonal_variation_", tvar, ".pdf")), 
  width = 8, height = 3*length(unique(dat$Parameter)), units = "in"
)
