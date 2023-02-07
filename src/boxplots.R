# prepare environment -----------------------------------------------------
library(tidyr)
library(dplyr)
library(ggplot2)

dat <- readRDS(file.path(getwd(), "/data/", "dat.RDS"))

# prepare data ------------------------------------------------------------
# subset 
metadata <- c("Date", "Site", "Management_System" , "Tillage", "Cover_Crop", "Treatment_Group", "Year", "Month", "Day")

datmain <- subset(
  dat,
  select = c(
    metadata,
    c(
      "Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N",
      
      # "AOA", "AOB", "nosZ",
      "log(AOA)", "log(AOB)", "log(nosZ)",
      
      # "BG", "NAG", "AP",
      "ln(BG)", "ln(NAG)", "ln(AP)",
      "NAG:BG", "NAG:AP"
    )
  )
)

# restructure data
datmain <- pivot_longer(
  data = datmain, 
  cols = names(datmain)[-c(1:9)], 
  names_to = "Parameter"
)
  
# order parameters according to how they need to be displayed
datmain$Parameter <- factor(
  datmain$Parameter,
  levels = c(
    "Net_Nitrification", "Soil_NH4N", "Net_Mineralization", "Soil_NO3N",
    "log(AOA)", "log(AOB)", "log(nosZ)",
    "ln(BG)", "ln(NAG)", "ln(AP)",
    "NAG:BG", "NAG:AP"
  )
)

# remove NA entries
datmain <- datmain %>% drop_na(value)

# main text figures -------------------------------------------------------
ggplot(
  data = datmain, 
  aes(
    x = Treatment_Group, 
    y = value, 
    color = Year
  )
) +
  theme_bw() +
  geom_point(position = position_jitterdodge(jitter.height = 0), alpha = 0.5) +
  geom_boxplot() +
  facet_wrap(Parameter~., scales = "free_y", ncol = 2, dir = "v") +
  theme(
    legend.position = "bottom", 
    axis.title.y = element_blank(), 
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(size = 8)
  ) +
  xlab("Treatment Group")

# ggsave(
#   filename = file.path(getwd(), "figs", "boxplot_MAIN.pdf"), 
#   width = 18, height = 24, units = "cm"
# )
